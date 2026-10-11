/// Wrapper macro to hide deprecated function use warnings.
/// [`index_vec::define_index_type`] internally uses deprecated
/// `core::num::<impl usize>::max_value` as `MAX_INDEX` value.
#[macro_export]
macro_rules! define_index_type {
    ($vis:vis struct $type:ident = $raw:ident;) => {
        index_vec::define_index_type! {
            $vis struct $type = $raw;

            MAX_INDEX = $raw::MAX;
        }
    };
}
