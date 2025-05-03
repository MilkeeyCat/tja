use super::CallingConvention;
use crate::{
    codegen::{
        CodeGen, Location,
        allocator::Allocator,
        operands::{Base, EffectiveAddress, Offset},
        register::Register,
    },
    repr::ty::{Ty, TyIdx},
};

#[derive(Debug, PartialEq, Clone, Copy)]
enum ClassKind {
    Integer,
    Memory,
    Sse,
    SseUp,
    NoClass,
}

pub struct SysVAmd64;

impl SysVAmd64 {
    pub fn new() -> Self {
        Self
    }

    fn classify(&self, codegen: &CodeGen, ty: TyIdx, offset: usize, eightbytes: &mut [ClassKind]) {
        match codegen.module.ty_storage.get_ty(ty) {
            Ty::Void => unimplemented!(),
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::Ptr => {
                let idx = offset / 8;

                eightbytes[idx] = self.merge(eightbytes[idx], ClassKind::Integer);
            }
            Ty::Struct(tys) => {
                let storage = &codegen.module.ty_storage;

                for (i, &ty) in tys.iter().enumerate() {
                    self.classify(
                        codegen,
                        ty,
                        codegen.abi.field_offset(storage, &tys, i),
                        eightbytes,
                    );
                }
            }
        };
    }

    fn ty_class(&self, codegen: &CodeGen, ty: TyIdx) -> Vec<ClassKind> {
        let size = codegen.abi.ty_size(&codegen.module.ty_storage, ty);
        let mut eightbytes = vec![ClassKind::NoClass; size.next_multiple_of(8) / 8];

        if eightbytes.len() > 8 {
            eightbytes[0] = ClassKind::Memory;
        } else {
            self.classify(codegen, ty, 0, &mut eightbytes);
        }

        // post merger cleanup
        {
            // 5.a
            if eightbytes.contains(&ClassKind::Memory) {
                eightbytes.clear();
                eightbytes.push(ClassKind::Memory);
            }

            // 5.c
            if eightbytes.len() > 2
                && (eightbytes[0] != ClassKind::Sse
                    || eightbytes[1..].iter().any(|kind| kind != &ClassKind::SseUp))
            {
                eightbytes.clear();
                eightbytes.push(ClassKind::Memory);
            }
        }

        eightbytes
    }

    fn merge(&self, lhs: ClassKind, rhs: ClassKind) -> ClassKind {
        match (lhs, rhs) {
            (lhs, rhs) if lhs == rhs => lhs,
            (ClassKind::NoClass, other) | (other, ClassKind::NoClass) => other,
            (ClassKind::Memory, other) | (other, ClassKind::Memory) => other,
            (ClassKind::Integer, other) | (other, ClassKind::Integer) => other,
            _ => ClassKind::Sse,
        }
    }
}

impl CallingConvention for SysVAmd64 {
    fn parameters(
        &self,
        codegen: &CodeGen,
        allocator: &mut Allocator,
        tys: &[TyIdx],
    ) -> Vec<Vec<Location>> {
        let mut locations: Vec<_> = self
            .arguments(codegen, tys)
            .into_iter()
            .map(|locations| {
                locations
                    .into_iter()
                    .map(|location| {
                        match location {
                            Location::Address {
                                mut effective_address,
                                spilled,
                            } => {
                                let displacement = effective_address.displacement.as_mut().unwrap();

                                // stack contains a return address and rbp's value
                                *displacement = *displacement + Offset(8 + 8);

                                Location::Address {
                                    effective_address,
                                    spilled,
                                }
                            }
                            other => other,
                        }
                    })
                    .collect::<Vec<_>>()
            })
            .collect();

        for (local_idx, locations) in locations.iter_mut().enumerate() {
            for location in locations {
                // it's not possible to assign multiple locations to one local,
                // so to let allocator know that other location is also taken,
                // a new node has to be created with the same neighbors as
                // current local
                if allocator.is_precolored(&local_idx) {
                    let node = allocator.create_node(tys[local_idx]);

                    for neighbor in allocator
                        .neighbors(&local_idx)
                        .into_iter()
                        .cloned()
                        .collect::<Vec<_>>()
                        .into_iter()
                    {
                        allocator.add_edge((node, neighbor));
                    }
                } else {
                    allocator.precolor(local_idx, location.clone());
                }
            }
        }

        locations
    }

    fn arguments(&self, codegen: &CodeGen, tys: &[TyIdx]) -> Vec<Vec<Location>> {
        let mut locations = Vec::new();
        let mut registers = vec![
            Register::Rdi,
            Register::Rsi,
            Register::Rdx,
            Register::Rcx,
            Register::R8,
            Register::R9,
        ]
        .into_iter();
        let mut stack_offset = 0;
        let mut alloc = |size: usize| -> Location {
            let location = Location::Address {
                effective_address: EffectiveAddress {
                    base: Base::Register(Register::Rbp),
                    index: None,
                    scale: None,
                    displacement: Some(Offset(stack_offset as isize)),
                },
                spilled: false,
            };

            stack_offset += size;

            location
        };

        for (local_idx, classes) in tys
            .into_iter()
            .map(|ty| self.ty_class(codegen, *ty))
            .enumerate()
        {
            locations.push(Vec::new());
            let locations = locations.last_mut().unwrap();
            let mut in_progress = false;

            for class in classes {
                match class {
                    ClassKind::Integer => {
                        if let Some(r) = registers.next() {
                            locations.push(Location::Register(r));
                        } else {
                            if in_progress {
                                locations.clear();
                            }

                            locations.push(alloc(
                                codegen
                                    .abi
                                    .ty_size(codegen.module.ty_storage, tys[local_idx]),
                            ));
                        }
                    }
                    ClassKind::Memory => locations.push(alloc(
                        codegen
                            .abi
                            .ty_size(codegen.module.ty_storage, tys[local_idx]),
                    )),
                    ClassKind::Sse => unimplemented!(),
                    ClassKind::SseUp => unimplemented!(),
                    ClassKind::NoClass => unreachable!(),
                }

                in_progress = true;
            }
        }

        locations
    }
}
