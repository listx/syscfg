use std::collections::HashMap;
use std::env;
use std::fs;

pub fn shorten(path: &str, path_aliases_file: &str) -> String {
    let path_canonical = make_canonical_path(path, path_aliases_file);
    path_shorten(&path_canonical)
}

fn path_shorten(path_canonical: &str) -> String {
    // Don't shorten paths that are 30 characters or less in length.
    if path_canonical.chars().count() <= 30 {
        return path_canonical.to_string();
    }

    // Don't bother shortening anything if there is only 1 directory.
    let parts_count = path_canonical.split("/").count();
    if parts_count == 1 {
        return path_canonical.to_string();
    }
    let first_char = path_canonical.chars().next().unwrap();
    if first_char == '/' && parts_count == 2 {
        return path_canonical.to_string();
    }

    // Determine overall "search" area of possible directories within the path
    // to shorten to 1 character. We exclude from the search the very first and
    // last directories.
    let (j, shortenable_dirs) = match first_char {
        // Do not shorten leading directories that start with '~', and also do
        // not consider the root directory '/'.
        '/' | '~' => (1, 1..(parts_count - 1)),
        _ => (0, 0..(parts_count - 1)),
    };

    // Construct a set of ranges, using shortenable_dirs. E.g., if
    // shortenable_dirs is (1..3), then construct:
    //   (1..2)
    //   (1..3)
    //   (1..4).
    // We use these ranges to denote directories that should be shortened. As
    // these ranges include more and more numbers, we shorten more and more
    // directories until we are satisified with how much we've shortened
    // path_canonical.
    let mut ranges: Vec<std::ops::Range<usize>> = Vec::new();

    for i in shortenable_dirs {
        ranges.push(j..i + 1);
    }

    let mut candidate_best: Option<String> = None;
    for range in ranges {
        // Construct shortened path candidate with all directories in the range
        // shortened.
        let mut candidate: Vec<String> = Vec::new();
        for (part_idx, part) in path_canonical.split("/").enumerate() {
            if range.contains(&part_idx) {
                // Add shortened version.
                candidate.push(part.chars().next().unwrap().to_string());
            } else {
                // Add as-is.
                candidate.push(part.to_string());
            }
        }
        let shortened = candidate.join("/");
        // If a better (shorter) candidate is found, prefer it over the previous
        // candidate.
        if candidate_best.is_none()
            || shortened.chars().count() < candidate_best.as_ref().unwrap().chars().count()
        {
            candidate_best = Some(shortened);
        };

        // If a candidate is already under 30 characters, stop searching.
        if candidate_best.is_some() && candidate_best.as_ref().unwrap().chars().count() <= 30 {
            break;
        }
    }

    if candidate_best.is_none() {
        path_canonical.to_string()
    } else {
        candidate_best.unwrap().to_string()
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
            path_shorten("/a23456789/b23456789/c23456789d"),
            "/a/b23456789/c23456789d"
        );
        // Some longer directories.
        assert_eq!(
            path_shorten("/a23456789/b23456789/c23456789/d23456789"),
            "/a/b/c23456789/d23456789"
        );
        assert_eq!(
            path_shorten("a23456789/b23456789/c23456789/d23456789"),
            "a/b/c23456789/d23456789"
        );
        // Shortening of aliases (directories with "~") in them are forbidden.
        assert_eq!(
            path_shorten("~a23456789/b23456789/c23456789/d23456789"),
            "~a23456789/b/c/d23456789"
        );
        // Realistic example (last directory remains untouched).
        assert_eq!(
            path_shorten("~/prog/foreign/git/contrib/thunderbird-patch-inline"),
            "~/p/f/g/c/thunderbird-patch-inline"
        );
        // Extreme cases.
        assert_eq!(
            path_shorten(
                "~/aaaaaaaaaaaaaaaaaaaa/bbbbbbbbbbbbbbbbbbbbbb/cccccccccccccccccccccc/hello"
            ),
            "~/a/b/c/hello"
        );
        // Unusual case of just 2 directories, where both are very long.
        assert_eq!(
            path_shorten(
                "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
            ),
            "a/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
        );
        // Non-ASCII (exactly 30 characters).
        assert_eq!(
            path_shorten("/일이삼사오육칠팔구/일이삼사오육칠팔구/일이삼사오육칠팔구"),
            "/일이삼사오육칠팔구/일이삼사오육칠팔구/일이삼사오육칠팔구"
        );
        assert_eq!(
            path_shorten("일이삼사오육칠팔구/일이삼사오육칠팔구/일이삼사오육칠팔구a"),
            "일이삼사오육칠팔구/일이삼사오육칠팔구/일이삼사오육칠팔구a"
        );
        assert_eq!(
            path_shorten("~일이삼사오육칠팔구/일이삼사오육칠팔구/일이삼사오육칠팔구"),
            "~일이삼사오육칠팔구/일이삼사오육칠팔구/일이삼사오육칠팔구"
        );
        // Non-ASCII (over 30 characters).
        assert_eq!(
            path_shorten("/일이삼사오육칠팔구/일이삼사오육칠팔구/일이삼사오육칠팔구/a"),
            "/일/일이삼사오육칠팔구/일이삼사오육칠팔구/a"
        );
        assert_eq!(
            path_shorten("~/일일일일일일일일일일일일일일일일일일일일/이이이이이이이이이이이이이이이이이이이이/삼삼삼삼삼삼삼삼삼삼삼삼삼삼삼삼삼삼삼삼/hello"),
            "~/일/이/삼/hello"
        );
    }
}
