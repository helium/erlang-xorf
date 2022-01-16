use xorf::{BinaryFuse16, BinaryFuse32, BinaryFuse8, Filter, Xor16, Xor32, Xor8};

pub enum XorRes {
    BF8(BinaryFuse8),
    BF16(BinaryFuse16),
    BF32(BinaryFuse32),
    X8(Xor8),
    X16(Xor16),
    X32(Xor32),
}

impl Filter<u64> for XorRes {
    fn contains(&self, key: &u64) -> bool {
        match self {
            XorRes::BF8(filter) => filter.contains(key),
            XorRes::BF16(filter) => filter.contains(key),
            XorRes::BF32(filter) => filter.contains(key),
            XorRes::X8(filter) => filter.contains(key),
            XorRes::X16(filter) => filter.contains(key),
            XorRes::X32(filter) => filter.contains(key),
        }
    }

    fn len(&self) -> usize {
        match self {
            XorRes::BF8(filter) => filter.len(),
            XorRes::BF16(filter) => filter.len(),
            XorRes::BF32(filter) => filter.len(),
            XorRes::X8(filter) => filter.len(),
            XorRes::X16(filter) => filter.len(),
            XorRes::X32(filter) => filter.len(),
        }
    }
}

impl XorRes {
    pub fn serialize_into<W: std::io::Write>(&self, writer: W) -> Result<(), bincode::Error> {
        match self {
            XorRes::BF8(filter) => bincode::serialize_into(writer, filter),
            XorRes::BF16(filter) => bincode::serialize_into(writer, filter),
            XorRes::BF32(filter) => bincode::serialize_into(writer, filter),
            XorRes::X8(filter) => bincode::serialize_into(writer, filter),
            XorRes::X16(filter) => bincode::serialize_into(writer, filter),
            XorRes::X32(filter) => bincode::serialize_into(writer, filter),
        }
    }

    pub fn serialized_size(&self) -> Result<usize, bincode::Error> {
        match self {
            XorRes::BF8(filter) => bincode::serialized_size(filter),
            XorRes::BF16(filter) => bincode::serialized_size(filter),
            XorRes::BF32(filter) => bincode::serialized_size(filter),
            XorRes::X8(filter) => bincode::serialized_size(filter),
            XorRes::X16(filter) => bincode::serialized_size(filter),
            XorRes::X32(filter) => bincode::serialized_size(filter),
        }
        .map(|v| v as usize)
    }
}
