macro_rules! usize_wrapper {
    ($name: ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
        pub struct $name(pub usize);

        impl std::ops::Deref for $name {
            type Target = usize;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
    };
}

pub(crate) use usize_wrapper;
