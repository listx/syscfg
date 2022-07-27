mod git;
mod path_shorten;
mod paths_sort;

rustler::init!(
    "Elixir.LH.Lightning",
    [
        git::find_existing_parent,
        git::is_git_index_file,
        git::is_git_repo,
        git::get_repo_id,
        git::repo_stats,
        path_shorten::path_shorten,
        paths_sort::paths_sort
    ]
);
