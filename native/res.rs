use xorf::{BinaryFuse16, BinaryFuse32, BinaryFuse8};

pub struct BF8Res(pub BinaryFuse8);
pub struct BF16Res(pub BinaryFuse16);
pub struct BF32Res(pub BinaryFuse32);

impl From<BinaryFuse8> for BF8Res {
    fn from(other: BinaryFuse8) -> Self {
        BF8Res(other)
    }
}

impl From<BinaryFuse16> for BF16Res {
    fn from(other: BinaryFuse16) -> Self {
        BF16Res(other)
    }
}

impl From<BinaryFuse32> for BF32Res {
    fn from(other: BinaryFuse32) -> Self {
        BF32Res(other)
    }
}
