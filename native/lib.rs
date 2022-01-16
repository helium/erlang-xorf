// use bin::Bin;
use res::BFRes;
use rustler::{Atom, Binary, Env, Error, NifResult, OwnedBinary, ResourceArc, Term};
use std::ops::Deref;
use xorf::{BinaryFuse16, BinaryFuse32, BinaryFuse8, Filter};

mod res;

rustler::atoms! {
    ok,
    serialize,
}

#[rustler::nif(name = "bf_new")]
pub fn bf_new(size: u8, keys: Vec<u64>) -> NifResult<(Atom, ResourceArc<BFRes>)> {
    match size {
        8 => BinaryFuse8::try_from(keys).map(BFRes::BF8),
        16 => BinaryFuse16::try_from(keys).map(BFRes::BF16),
        32 => BinaryFuse32::try_from(keys).map(BFRes::BF32),
        _ => return Err(Error::BadArg),
    }
    .map_err(|msg| Error::Term(Box::new(msg)))
    .map(|filter| (ok(), ResourceArc::new(filter)))
}

fn serialize_err(err: bincode::Error) -> rustler::Error {
    Error::Term(Box::new((serialize(), err.to_string())))
}

#[rustler::nif(name = "bf_to_bin")]
pub fn bf_to_bin(env: Env, filter: ResourceArc<BFRes>) -> NifResult<(Atom, Binary)> {
    let mut data = filter
        .serialized_size()
        .map_err(serialize_err)
        .map(OwnedBinary::new)?
        .unwrap();

    filter
        .serialize_into(data.as_mut_slice())
        .map_err(serialize_err)?;

    Ok((ok(), Binary::from_owned(data, env)))
}

#[rustler::nif(name = "bf_from_bin")]
pub fn bf_from_bin(size: u8, bin: Binary) -> NifResult<(Atom, ResourceArc<BFRes>)> {
    let bytes = bin.as_slice();
    match size {
        8 => bincode::deserialize::<BinaryFuse8>(bytes).map(BFRes::BF8),
        16 => bincode::deserialize::<BinaryFuse16>(bytes).map(BFRes::BF16),
        32 => bincode::deserialize::<BinaryFuse32>(bytes).map(BFRes::BF32),
        _ => return Err(Error::BadArg),
    }
    .map_err(serialize_err)
    .map(|filter| (ok(), ResourceArc::new(filter)))
}

#[rustler::nif(name = "bf_contains")]
pub fn bf_contains(filter: ResourceArc<BFRes>, key: u64) -> bool {
    filter.deref().contains(&key)
}

pub fn on_load(env: Env, _load_info: Term) -> bool {
    rustler::resource!(BFRes, env);
    true
}

rustler::init!(
    "xorf_nif",
    [bf_new, bf_contains, bf_to_bin, bf_from_bin],
    load = on_load
);
