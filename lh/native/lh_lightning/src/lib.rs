mod path_shorten;
mod paths_sort;

#[rustler::nif]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

#[rustler::nif]
fn hfr() -> &'static str {
    "hello from rust 2!\n"
}

rustler::init!(
    "Elixir.LH.Lightning",
    [add, hfr, path_shorten::path_shorten, paths_sort::paths_sort]
);
