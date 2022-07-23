mod git;
mod path_shorten;
mod paths_sort;

rustler::init!(
    "Elixir.LH.Lightning",
    [
        git::repo_stats,
        path_shorten::path_shorten,
        paths_sort::paths_sort
    ]
);
