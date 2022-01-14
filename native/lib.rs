use bin::Bin;
use res::{BF16Res, BF32Res, BF8Res};
use rustler::{Atom, Env, NifResult, ResourceArc, Term};
use xorf::{BinaryFuse16, BinaryFuse32, BinaryFuse8, Filter};

mod bin;
mod res;

rustler::atoms! {
    ok,
    error,
    invalid_code,
}

#[rustler::nif(name = "bf8_new")]
pub fn bf8_new(keys: Vec<u64>) -> NifResult<(Atom, ResourceArc<BF8Res>)> {
    let filter = BinaryFuse8::try_from(keys).expect("Error creating filter!");
    Ok((ok(), ResourceArc::new(BF8Res::from(filter))))
}

#[rustler::nif(name = "bf16_new")]
pub fn bf16_new(keys: Vec<u64>) -> NifResult<(Atom, ResourceArc<BF16Res>)> {
    let filter = BinaryFuse16::try_from(keys).expect("Error creating filter!");
    Ok((ok(), ResourceArc::new(BF16Res::from(filter))))
}

#[rustler::nif(name = "bf32_new")]
pub fn bf32_new(keys: Vec<u64>) -> NifResult<(Atom, ResourceArc<BF32Res>)> {
    let filter = BinaryFuse32::try_from(keys).expect("Error creating filter!");
    Ok((ok(), ResourceArc::new(BF32Res::from(filter))))
}

#[rustler::nif(name = "bf8_to_bin")]
pub fn bf8_to_bin(filterarc: ResourceArc<BF8Res>) -> NifResult<(Atom, Bin)> {
    let bytes = bincode::serialize(&filterarc.0).expect("Unable to serialize!");
    Ok((ok(), Bin(bytes)))
}

#[rustler::nif(name = "bf16_to_bin")]
pub fn bf16_to_bin(filterarc: ResourceArc<BF16Res>) -> NifResult<(Atom, Bin)> {
    let bytes = bincode::serialize(&filterarc.0).expect("Unable to serialize!");
    Ok((ok(), Bin(bytes)))
}

#[rustler::nif(name = "bf32_to_bin")]
pub fn bf32_to_bin(filterarc: ResourceArc<BF32Res>) -> NifResult<(Atom, Bin)> {
    let bytes = bincode::serialize(&filterarc.0).expect("Unable to serialize!");
    Ok((ok(), Bin(bytes)))
}

#[rustler::nif(name = "bf8_from_bin")]
pub fn bf8_from_bin(bin: Bin) -> NifResult<(Atom, ResourceArc<BF8Res>)> {
    let filter: BinaryFuse8 = bincode::deserialize(&bin.0).expect("Unable to deserialize!");
    Ok((ok(), ResourceArc::new(BF8Res::from(filter))))
}

#[rustler::nif(name = "bf16_from_bin")]
pub fn bf16_from_bin(bin: Bin) -> NifResult<(Atom, ResourceArc<BF16Res>)> {
    let filter: BinaryFuse16 = bincode::deserialize(&bin.0).expect("Unable to deserialize!");
    Ok((ok(), ResourceArc::new(BF16Res::from(filter))))
}

#[rustler::nif(name = "bf32_from_bin")]
pub fn bf32_from_bin(bin: Bin) -> NifResult<(Atom, ResourceArc<BF32Res>)> {
    let filter: BinaryFuse32 = bincode::deserialize(&bin.0).expect("Unable to deserialize!");
    Ok((ok(), ResourceArc::new(BF32Res::from(filter))))
}

#[rustler::nif(name = "bf8_contains")]
pub fn bf8_contains(filterarc: ResourceArc<BF8Res>, key: u64) -> bool {
    filterarc.0.contains(&key)
}

#[rustler::nif(name = "bf16_contains")]
pub fn bf16_contains(filterarc: ResourceArc<BF16Res>, key: u64) -> bool {
    filterarc.0.contains(&key)
}

#[rustler::nif(name = "bf32_contains")]
pub fn bf32_contains(filterarc: ResourceArc<BF32Res>, key: u64) -> bool {
    filterarc.0.contains(&key)
}

pub fn on_load(env: Env, _load_info: Term) -> bool {
    rustler::resource!(BF8Res, env);
    rustler::resource!(BF16Res, env);
    rustler::resource!(BF32Res, env);
    true
}

rustler::init!(
    "xorf_nif",
    [
        bf8_new,
        bf8_to_bin,
        bf8_from_bin,
        bf8_contains,
        bf16_new,
        bf16_to_bin,
        bf16_from_bin,
        bf16_contains,
        bf32_new,
        bf32_to_bin,
        bf32_from_bin,
        bf32_contains
    ],
    load = on_load
);
