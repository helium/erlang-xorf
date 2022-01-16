// use bin::Bin;
use res::XorRes;
use rustler::{Atom, Binary, Env, Error, NifResult, OwnedBinary, ResourceArc, Term};
use std::ops::Deref;
use xorf::{BinaryFuse16, BinaryFuse32, BinaryFuse8, Filter, Xor16, Xor32, Xor8};

mod res;

rustler::atoms! {
    ok,
    serialize,
    invalid_xor_construction,
}

#[rustler::nif(name = "bf_new")]
pub fn bf_new(size: u8, keys: Vec<u64>) -> NifResult<(Atom, ResourceArc<XorRes>)> {
    match size {
        8 => BinaryFuse8::try_from(keys).map(XorRes::BF8),
        16 => BinaryFuse16::try_from(keys).map(XorRes::BF16),
        32 => BinaryFuse32::try_from(keys).map(XorRes::BF32),
        _ => return Err(Error::BadArg),
    }
    .map_err(|msg| Error::Term(Box::new(msg)))
    .map(|filter| (ok(), ResourceArc::new(filter)))
}

#[rustler::nif(name = "xor_new")]
pub fn xor_new(size: u8, keys: Vec<u64>) -> NifResult<(Atom, ResourceArc<XorRes>)> {
    match size {
        8 => Xor8::try_from(keys).map(XorRes::X8),
        16 => Xor16::try_from(keys).map(XorRes::X16),
        32 => Xor32::try_from(keys).map(XorRes::X32),
        _ => return Err(Error::BadArg),
    }
    // NOTE: msg here is reported Infallible, hence constructing an error by hand
    .map_err(|_msg| Error::Term(Box::new(invalid_xor_construction())))
    .map(|filter| (ok(), ResourceArc::new(filter)))
}

fn serialize_err(err: bincode::Error) -> rustler::Error {
    Error::Term(Box::new((serialize(), err.to_string())))
}

#[rustler::nif(name = "to_bin")]
pub fn to_bin(env: Env, filter: ResourceArc<XorRes>) -> NifResult<(Atom, Binary)> {
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
pub fn bf_from_bin(size: u8, bin: Binary) -> NifResult<(Atom, ResourceArc<XorRes>)> {
    let bytes = bin.as_slice();
    match size {
        8 => bincode::deserialize::<BinaryFuse8>(bytes).map(XorRes::BF8),
        16 => bincode::deserialize::<BinaryFuse16>(bytes).map(XorRes::BF16),
        32 => bincode::deserialize::<BinaryFuse32>(bytes).map(XorRes::BF32),
        _ => return Err(Error::BadArg),
    }
    .map_err(serialize_err)
    .map(|filter| (ok(), ResourceArc::new(filter)))
}

#[rustler::nif(name = "xor_from_bin")]
pub fn xor_from_bin(size: u8, bin: Binary) -> NifResult<(Atom, ResourceArc<XorRes>)> {
    let bytes = bin.as_slice();
    match size {
        8 => bincode::deserialize::<Xor8>(bytes).map(XorRes::X8),
        16 => bincode::deserialize::<Xor16>(bytes).map(XorRes::X16),
        32 => bincode::deserialize::<Xor32>(bytes).map(XorRes::X32),
        _ => return Err(Error::BadArg),
    }
    .map_err(serialize_err)
    .map(|filter| (ok(), ResourceArc::new(filter)))
}

#[rustler::nif(name = "contains")]
pub fn contains(filter: ResourceArc<XorRes>, key: u64) -> bool {
    filter.deref().contains(&key)
}

pub fn on_load(env: Env, _load_info: Term) -> bool {
    rustler::resource!(XorRes, env);
    true
}

rustler::init!(
    "xorf_nif",
    [bf_new, xor_new, contains, to_bin, bf_from_bin, xor_from_bin],
    load = on_load
);
