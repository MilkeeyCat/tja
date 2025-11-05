use std::{
    mem::MaybeUninit,
    ops::{Deref, DerefMut},
};

#[derive(Debug)]
pub struct Uninitialized<T> {
    value: MaybeUninit<T>,
    initialized: bool,
}

impl<T> Uninitialized<T> {
    pub fn new() -> Self {
        Self {
            value: MaybeUninit::<T>::uninit(),
            initialized: false,
        }
    }

    pub fn write(&mut self, value: T) -> &mut T {
        self.initialized = true;

        self.value.write(value)
    }

    pub fn inner(mut self) -> T {
        assert!(self.initialized);

        unsafe {
            let mut value = MaybeUninit::uninit();

            std::mem::swap(&mut self.value, &mut value);
            self.initialized = false;

            value.assume_init()
        }
    }
}

impl<T> Deref for Uninitialized<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        assert!(self.initialized);

        unsafe { self.value.assume_init_ref() }
    }
}

impl<T> DerefMut for Uninitialized<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        assert!(self.initialized);

        unsafe { self.value.assume_init_mut() }
    }
}

impl<T> Drop for Uninitialized<T> {
    fn drop(&mut self) {
        if self.initialized {
            unsafe { self.value.assume_init_drop() }
        }
    }
}
