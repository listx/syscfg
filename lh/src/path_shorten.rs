use std::collections::HashMap;
use std::env;
use std::fs;

pub fn shorten(path: &str, path_aliases_file: &str) -> String {
    let path_canonical = make_canonical_path(path, path_aliases_file);
    path_shorten(&path_canonical)
}

fn path_shorten(path_canonical: &str) -> String {
    if path_canonical.chars().count() < 2 {
        return path_canonical.to_string();
    }

    let mut dirs: Vec<String> = Vec::new();

    let num_dirs = path_canonical.split("/").count();

    // Construct the shortened path. As we consider each directory that makes up
    // the path, shorten it if using it as-is would result in >30 characters
    // used.
    let mut path_len = 0;
    let mut path_len_rem = path_canonical.len();
    for (i, dir) in path_canonical.split("/").enumerate() {
        // Handle special case where a leading slash, e.g., "/a/b" results in
        // iterating over ["", "a", "b"]. We skip over the first element.
        if i == 0 && dir.len() == 0 {
            path_len += 1;
            path_len_rem -= 1;
            continue;
        }

        let dir_long = dir.to_string();
        let dir_long_len = dir.len();

        let dir_short = dir.chars().next().unwrap().to_string();

        if path_len + path_len_rem > 30 && !dir.contains("~") && i != num_dirs - 1 {
            dirs.push(dir_short);
            path_len += 1;
            path_len_rem -= dir_long_len;
        } else {
            dirs.push(dir_long);
            path_len += dir_long_len;
            path_len_rem -= dir_long_len;
        }
        // Only add length of "/" separator if we're not at the end.
        if i != num_dirs - 1 {
            path_len += 1;
            path_len_rem -= 1;
        }
    }

    if path_canonical.chars().next().unwrap() == '/' {
        format!("/{}", dirs.join("/"))
    } else {
        dirs.join("/")
    }
}

fn make_canonical_path(path: &str, path_aliases_file: &str) -> String {
    // The '?' operator short-circuits if we don't have a HOME variable defined
    // in the environment.
    let home_dir = env::var("HOME").expect("$HOME not set");

    let path_aliases_contents = fs::read_to_string(path_aliases_file).unwrap_or_default();
    let path_aliases = make_path_aliases(&path_aliases_contents, &home_dir);

    let path_canonical = match get_matching_path_alias(path, &path_aliases) {
        // Find the longest matching expanded path in the path aliases. If there
        // is a match, then we use "~ALIAS" (the leading "~" does not mean $HOME
        // and just signifies that the word that immediately follows it is a
        // path alias).
        Some((expanded_path, path_alias)) => {
            let new_path = path.replacen(&expanded_path, &path_alias, 1);
            format!("~{}", new_path)
        }
        // If there is no match, just replace whatever $HOME is with "~".
        None => {
            if path.starts_with(&home_dir) {
                format!("{}", path.replacen(&home_dir, "~", 1))
            } else {
                format!("{}", path)
            }
        }
    };

    path_canonical
}

fn make_path_aliases(path_aliases_contents: &str, home_dir: &String) -> HashMap<String, String> {
    let mut path_aliases = HashMap::new();

    for line in path_aliases_contents.lines() {
        if line.starts_with("hash") {
            for word in line.split_ascii_whitespace() {
                if word.contains("=") {
                    let mut k_or_v = word.split("=");
                    let v = k_or_v.next().unwrap_or("").to_string();
                    let k = k_or_v.next().unwrap_or("").to_string();
                    let k_final = k.replacen("$HOME", &home_dir, 1);
                    path_aliases.insert(k_final, v);
                }
            }
        }
    }

    path_aliases
}

fn get_matching_path_alias(
    path: &str,
    path_aliases: &HashMap<String, String>,
) -> Option<(String, String)> {
    let mut keys = path_aliases.keys().collect::<Vec<_>>();
    keys.sort();
    keys.reverse();

    for k in keys {
        if path.starts_with(k) {
            let v = path_aliases.get(k).unwrap();
            return Some((k.to_string(), v.to_string()));
        }
    }

    None
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_path_shorten() {
        assert_eq!(path_shorten(""), "");
        assert_eq!(path_shorten("~"), "~");
        assert_eq!(path_shorten("/"), "/");
        //assert_eq!("/a/b".split("/").collect::<Vec<&str>>(), [""]);
        assert_eq!(path_shorten("/a"), "/a");
        assert_eq!(path_shorten("/a/b/c"), "/a/b/c");
        assert_eq!(path_shorten("a"), "a");
        assert_eq!(path_shorten("a/b/c"), "a/b/c");
        // If the path is exactly 30 characters, we should not shorten anything.
        assert_eq!(
            path_shorten("/a23456789/b23456789/c23456789"),
            "/a23456789/b23456789/c23456789"
        );
        // If the path is just over 30 characters, we should shorten the first
        // directory.
        assert_eq!(
            path_shorten("/a23456789/123456789/123456789a"),
            "/a/123456789/123456789a"
        );
        // Some longer directories.
        assert_eq!(
            path_shorten("/a23456789/b23456789/123456789/123456789"),
            "/a/b/123456789/123456789"
        );
        assert_eq!(
            path_shorten("a23456789/b23456789/123456789/123456789"),
            "a/b/123456789/123456789"
        );
        // Shortening of aliases (directories with "~") in them are forbidden.
        assert_eq!(
            path_shorten("~123456789/123456789/123456789/123456789"),
            "~123456789/1/1/123456789"
        );
        // Realistic example (last directory remains untouched).
        assert_eq!(
            path_shorten("~/prog/foreign/git/contrib/thunderbird-patch-inline"),
            "~/p/f/g/c/thunderbird-patch-inline"
        );
        // Extreme example.
        assert_eq!(
            path_shorten(
                "~/aaaaaaaaaaaaaaaaaaaa/bbbbbbbbbbbbbbbbbbbbbb/cccccccccccccccccccccc/hello"
            ),
            "~/a/b/c/hello"
        );
    }
}
