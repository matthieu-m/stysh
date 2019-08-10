//! Common model items.

use std::fmt;

use crate::basic::com;

/// ModuleId.
///
/// A ModuleId is the unique identifier of any module with the dependency graph.
///
/// While its representation is opaque, it efficiently encodes:
/// -   A PackageId: the unique ID of a package within the dependency graph,
///     for its size category.
/// -   A LocalModuleId: the unique ID of a module within a package.
///
/// There are 4 size categories: Small, Medium, Large, Huge, corresponding to
/// maximum number of local modules a package contains.
#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ModuleId(com::CoreId);

impl ModuleId {
    /// Creates an instance of a ModuleId.
    pub fn new(package: u32, local: u32, nb_locals: u32) -> ModuleId {
        let (mask, package_bits, local_bits) =
            Self::format_from_locals(nb_locals);

        debug_assert!(local < (1 << local_bits));

        if package < (1 << package_bits) {
            let masked = mask << Self::MASK_OFFSET;
            let packaged = package << local_bits;

            return ModuleId(com::CoreId::new(masked + packaged + local));
        }

        panic!("{:?} packages in a single dependency tree exceeds the maximum of {} supported.",
            package, 1 << package_bits);
    }

    /// Returns the package Id.
    pub fn package(&self) -> u32 { self.unpack().1 }

    /// Returns the local Id.
    pub fn local(&self) -> u32 { self.unpack().2 }

    /// Returns the maximum number of locals in the package.
    pub fn capacity(&self) -> u32 { 1 << Self::LOCAL_BITS[self.unpack().0 as usize] }
}

impl ModuleId {
    const MASK_OFFSET: u32 = 30;
    const LOCAL_BITS: [u32; 4] = [6, 12, 18, 22];

    fn format_from_locals(nb_locals: u32) -> (u32, u32, u32) {
        for (mask, &local_bits) in Self::LOCAL_BITS.iter().enumerate() {
            if nb_locals <= (1 << local_bits) {
                return (mask as u32, Self::MASK_OFFSET - local_bits, local_bits);
            }
        }

        panic!("{:?} modules in a single package exceeds the maximum of {} supported.",
            nb_locals, 1 << Self::LOCAL_BITS.last().unwrap())
    }

    fn raw(&self) -> u32 { self.0.raw() }

    fn unpack(&self) -> (u32, u32, u32) {
        let id = self.raw();

        let mask = id >> Self::MASK_OFFSET;
        let local_bits = Self::LOCAL_BITS[mask as usize];

        let unmarked = id & ((1 << Self::MASK_OFFSET) - 1);
        (mask, unmarked >> local_bits, unmarked & ((1 << local_bits) - 1))
    }
}

impl fmt::Debug for ModuleId {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let (mask, package, local) = self.unpack();
        write!(f, "ModuleId({}, {}, {})", mask, package, local)
    }
}

#[cfg(test)]
mod tests {
    use super::ModuleId;

    #[test]
    fn round_trip_small() {
        for i in 0..64 {
            let id = ModuleId::new(77, i, i + 1);

            assert_eq!(id.package(), 77);
            assert_eq!(id.local(), i);
            assert_eq!(id.capacity(), 64);
        }
    }

    #[test]
    fn round_trip_medium() {
        for e in 6..12 {
            let i = 1 << e;

            let id = ModuleId::new(77, i, i + 1);
            println!("{:x}", id.raw());

            assert_eq!(id.package(), 77);
            assert_eq!(id.local(), i);
            assert_eq!(id.capacity(), 4096);

            let i = (1 << (e + 1)) - 1;

            let id = ModuleId::new(77, i, i + 1);
            println!("{:x}", id.raw());

            assert_eq!(id.package(), 77);
            assert_eq!(id.local(), i);
            assert_eq!(id.capacity(), 4096);
        }
    }

    #[test]
    fn round_trip_large() {
        for e in 12..18 {
            let i = 1 << e;

            let id = ModuleId::new(77, i, i + 1);
            assert_eq!(id.package(), 77);
            assert_eq!(id.local(), i);
            assert_eq!(id.capacity(), 262144);

            let i = (1 << (e + 1)) - 1;

            let id = ModuleId::new(77, i, i + 1);
            assert_eq!(id.package(), 77);
            assert_eq!(id.local(), i);
            assert_eq!(id.capacity(), 262144);
        }
    }

    #[test]
    fn round_trip_huge() {
        for e in 18..22 {
            let i = 1 << e;

            let id = ModuleId::new(77, i, i + 1);
            assert_eq!(id.package(), 77);
            assert_eq!(id.local(), i);
            assert_eq!(id.capacity(), 4194304);

            let i = (1 << (e + 1)) - 1;

            let id = ModuleId::new(77, i, i + 1);
            assert_eq!(id.package(), 77);
            assert_eq!(id.local(), i);
            assert_eq!(id.capacity(), 4194304);
        }
    }
}
