use rustler::{Binary, Encoder, Env, NifResult, Term};

rustler::atoms! {
    ok,
    error,
    invalid_code,
}

#[rustler::nif]
fn code<'a>(env: Env<'a>, _data: Binary) -> NifResult<Term<'a>> {
    Ok((ok(), 0u64).encode(env))
}

pub fn on_load(_env: Env, _load_info: Term) -> bool {
    true
}

rustler::init!("xorf_nif", [code], load = on_load);
