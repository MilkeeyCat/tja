use crate::{
    ast::{
        Body, ConcreteTerm, ConcreteTermKind, Definition, Expr, Extern, GlobalKind, Guard, IntType,
        Let, Literal, OverloadTerm, Pattern, Rule, Term, Type,
    },
    grammar::DefinitionsParser,
    lexer::Lexer,
};

macro_rules! test_definitions_parser {
    ($name: ident, $input: expr, $expected: expr) => {
        #[test]
        fn $name() {
            let result = DefinitionsParser::new().parse(Lexer::new($input));

            assert_eq!(result, $expected);
        }
    };
}

test_definitions_parser!(
    parse_terms,
    r"
        pure partial term foo((P1, P2)) -> P3;
        total term bar(P4, P5) -> P6;
        pure multi term baz(P7, P8) -> (P9, P10);
        overload term qux = foo + bar + baz;
    ",
    Ok(vec![
        Definition::Term(Term::Concrete(ConcreteTerm {
            kind: ConcreteTermKind::Partial,
            name: "foo".into(),
            arg_tys: vec![Type::Tuple(vec![
                Type::Ident("P1".into()),
                Type::Ident("P2".into()),
            ])],
            ret_ty: Type::Ident("P3".into()),
            is_pure: true,
        })),
        Definition::Term(Term::Concrete(ConcreteTerm {
            kind: ConcreteTermKind::Total,
            name: "bar".into(),
            arg_tys: vec![Type::Ident("P4".into()), Type::Ident("P5".into())],
            ret_ty: Type::Ident("P6".into()),
            is_pure: false
        })),
        Definition::Term(Term::Concrete(ConcreteTerm {
            kind: ConcreteTermKind::Multi,
            name: "baz".into(),
            arg_tys: vec![Type::Ident("P7".into()), Type::Ident("P8".into())],
            ret_ty: Type::Tuple(vec![Type::Ident("P9".into()), Type::Ident("P10".into())]),
            is_pure: true,
        })),
        Definition::Term(Term::Overload(OverloadTerm {
            name: "qux".into(),
            terms: vec!["foo".into(), "bar".into(), "baz".into()],
        })),
    ])
);

test_definitions_parser!(
    parse_externs,
    r"
        extern type Foo;
        extern type Bar = Baz;
        extern pure partial term qux(Foo) -> Bar;
        extern multi term qux(Foo) -> Bar = make;
        extern let A: Abc = const A;
        extern let B: Abc = field b;
    ",
    Ok(vec![
        Definition::Extern(Extern::Type {
            name: "Foo".into(),
            external_name: None,
        }),
        Definition::Extern(Extern::Type {
            name: "Bar".into(),
            external_name: Some("Baz".into()),
        }),
        Definition::Extern(Extern::Term {
            term: ConcreteTerm {
                kind: ConcreteTermKind::Partial,
                name: "qux".into(),
                arg_tys: vec![Type::Ident("Foo".into())],
                ret_ty: Type::Ident("Bar".into()),
                is_pure: true,
            },
            external_name: None,
        }),
        Definition::Extern(Extern::Term {
            term: ConcreteTerm {
                kind: ConcreteTermKind::Multi,
                name: "qux".into(),
                arg_tys: vec![Type::Ident("Foo".into())],
                ret_ty: Type::Ident("Bar".into()),
                is_pure: false,
            },
            external_name: Some("make".into()),
        }),
        Definition::Extern(Extern::Global {
            name: "A".into(),
            ty: Type::Ident("Abc".into()),
            kind: GlobalKind::Const,
            external_name: "A".into(),
        }),
        Definition::Extern(Extern::Global {
            name: "B".into(),
            ty: Type::Ident("Abc".into()),
            kind: GlobalKind::Field,
            external_name: "b".into(),
        }),
    ])
);

test_definitions_parser!(
    parse_rules,
    r"
        rule foo(bar(), (a, false), 5) if
            let true = has_avx512() &&
            let _ = 10usize
        {
            let n: (usize, i8, isize) = now();

            (n.0, n.2)
        }
    ",
    Ok(vec![Definition::Rule(Rule {
        pat: Pattern::Application {
            name: "foo".into(),
            args: vec![
                Pattern::Application {
                    name: "bar".into(),
                    args: Vec::new(),
                },
                Pattern::Tuple(vec![
                    Pattern::Ident("a".into()),
                    Pattern::Literal(Literal::Bool(false)),
                ]),
                Pattern::Literal(Literal::Int { value: 5, ty: None }),
            ],
        },
        guards: vec![
            Guard {
                pat: Pattern::Literal(Literal::Bool(true)),
                expr: Expr::Call {
                    name: "has_avx512".into(),
                    args: Vec::new(),
                },
            },
            Guard {
                pat: Pattern::Wildcard,
                expr: Expr::Literal(Literal::Int {
                    value: 10,
                    ty: Some(IntType::Usize),
                }),
            },
        ],
        body: Body {
            lets: vec![Let {
                name: "n".into(),
                ty: Type::Tuple(vec![
                    Type::Ident("usize".into()),
                    Type::Ident("i8".into()),
                    Type::Ident("isize".into()),
                ]),
                value: Expr::Call {
                    name: "now".into(),
                    args: Vec::new(),
                },
            }],
            expr: Expr::Tuple(vec![
                Expr::TupleIndex {
                    expr: Box::new(Expr::Ident("n".into())),
                    idx: 0,
                },
                Expr::TupleIndex {
                    expr: Box::new(Expr::Ident("n".into())),
                    idx: 2,
                },
            ]),
        },
    })])
);

test_definitions_parser!(parse_empty_string, "", Ok(Vec::new()));
