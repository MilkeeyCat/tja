use std::ops::{Deref, Sub};

#[derive(Debug, Clone, PartialEq)]
pub struct VecSet<T>(Vec<T>);

impl<T: PartialEq> VecSet<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, value: T) -> bool {
        if self.0.contains(&value) {
            false
        } else {
            self.0.push(value);

            true
        }
    }

    pub fn remove(&mut self, value: &T) -> bool {
        match self.iter().position(|v| v == value) {
            Some(idx) => {
                self.0.remove(idx);

                true
            }
            None => false,
        }
    }

    pub fn union<'a>(&'a self, other: &'a Self) -> impl Iterator<Item = &'a T> {
        self.0.iter().chain(
            other
                .0
                .iter()
                .filter_map(|value| (!self.0.contains(value)).then_some(value)),
        )
    }

    pub fn difference(&self, other: &Self) -> impl Iterator<Item = &T> {
        self.0
            .iter()
            .filter_map(|value| (!other.0.contains(value)).then_some(value))
    }

    pub fn intersection(&self, other: &Self) -> impl Iterator<Item = &T> {
        self.0
            .iter()
            .filter_map(|value| (other.0.contains(value)).then_some(value))
    }

    pub fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }
}

impl<T: Eq> Extend<T> for VecSet<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        for value in iter {
            if !self.0.contains(&value) {
                self.0.push(value);
            }
        }
    }
}

impl<T> From<Vec<T>> for VecSet<T> {
    fn from(value: Vec<T>) -> Self {
        Self(value)
    }
}

impl<T, const N: usize> From<[T; N]> for VecSet<T> {
    fn from(value: [T; N]) -> Self {
        Self(Vec::from(value))
    }
}

impl<T> Default for VecSet<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T> FromIterator<T> for VecSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self(Vec::from_iter(iter))
    }
}

impl<T> IntoIterator for VecSet<T> {
    type Item = T;
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a VecSet<T> {
    type Item = &'a T;
    type IntoIter = <&'a Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<T> Deref for VecSet<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Eq + Clone> Sub for &VecSet<T> {
    type Output = VecSet<T>;

    fn sub(self, rhs: Self) -> Self::Output {
        self.difference(rhs).cloned().collect()
    }
}

#[cfg(test)]
mod test {
    use super::VecSet;

    #[test]
    fn union() {
        let a = VecSet::from(vec![1, 2, 3, 4, 5, 6]);
        let b = VecSet::from(vec![0, 1, 2, 3, 4, 5]);
        let expected = vec![1, 2, 3, 4, 5, 6, 0];

        assert!(a.union(&b).eq(expected.iter()));
    }

    #[test]
    fn difference() {
        let a = VecSet::from(vec![1, 2, 3, 4, 5, 6]);
        let b = VecSet::from(vec![0, 1, 3, 5]);
        let expected = vec![2, 4, 6];

        assert!(a.difference(&b).eq(expected.iter()));
    }

    #[test]
    fn intersection() {
        let a = VecSet::from(vec![1, 2, 3, 4, 5, 6]);
        let b = VecSet::from(vec![0, 1, 3, 5]);
        let expected = vec![1, 3, 5];

        assert!(a.intersection(&b).eq(expected.iter()));
    }
}
