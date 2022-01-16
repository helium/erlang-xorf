use xorf::{BinaryFuse16, BinaryFuse32, BinaryFuse8, Filter};

pub enum BFRes {
    BF8(BinaryFuse8),
    BF16(BinaryFuse16),
    BF32(BinaryFuse32),
}

impl Filter<u64> for BFRes {
    fn contains(&self, key: &u64) -> bool {
        match self {
            BFRes::BF8(filter) => filter.contains(key),
            BFRes::BF16(filter) => filter.contains(key),
            BFRes::BF32(filter) => filter.contains(key),
        }
    }

    fn len(&self) -> usize {
        match self {
            BFRes::BF8(filter) => filter.len(),
            BFRes::BF16(filter) => filter.len(),
            BFRes::BF32(filter) => filter.len(),
        }
    }
}

impl BFRes {
    pub fn serialize_into<W: std::io::Write>(&self, writer: W) -> Result<(), bincode::Error> {
        match self {
            BFRes::BF8(filter) => bincode::serialize_into(writer, filter),
            BFRes::BF16(filter) => bincode::serialize_into(writer, filter),
            BFRes::BF32(filter) => bincode::serialize_into(writer, filter),
        }
    }

    pub fn serialized_size(&self) -> Result<usize, bincode::Error> {
        match self {
            BFRes::BF8(filter) => bincode::serialized_size(filter),
            BFRes::BF16(filter) => bincode::serialized_size(filter),
            BFRes::BF32(filter) => bincode::serialized_size(filter),
        }
        .map(|v| v as usize)
    }
}
