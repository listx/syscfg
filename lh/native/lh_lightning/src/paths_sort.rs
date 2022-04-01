use std::collections::HashMap;
use std::collections::HashSet;

// paths: a list of paths separated with the colon ':' character; same
//   appearance as $PATH var
// priorities_raw: priority of preferred directories
// home: $HOME var
//
// We sort the paths in $PATH into 2 groups: those paths that are found in
// priorities_raw, and those that are not. The former are sorted according to
// the ordering in priorities_raw; the latter remain undisturbed.
#[rustler::nif]
pub fn paths_sort(paths: &str, priorities_raw: &str, home: &str) -> String {
    let paths_canonical = make_canonical_paths(&paths, &home);
    _paths_sort(&paths_canonical, priorities_raw).replace("~", home)
}

fn _paths_sort(paths_canonical: &Vec<String>, priorities_raw: &str) -> String {
    let priorities: Vec<String> = priorities_raw.split(":").map(|s| s.to_string()).collect();

    let nop: String = paths_canonical.join(":");

    // No need to do any work if we don't want to sort anything (NOP).
    if priorities.len() == 0 {
        return nop;
    }

    // Likewise, NOP if there is only 1 path in $PATH.
    if paths_canonical.len() == 1 {
        return nop;
    }

    // "paths" is the same as paths_canonical, but we encode the vector index as
    // a value to the HashMap.
    let mut paths: HashMap<String, i32> = HashMap::new();
    let mut idx = 0;
    for p in paths_canonical {
        // We have to check for membership (whether we've seen this same path
        // before), because otherwise a duplicate path of it later down the road
        // will "bring down" the weight of this path. E.g., for "a:b:a", we want
        // "a" to come before "b" (based on idx), so the second "a" must not be
        // allowed to be written in with its higher idx.
        if !paths.contains_key(p) {
            paths.insert((&p).to_string(), idx);
        }
        idx += 1;
    }

    // Collect all known-prioritized paths out into paths_final.
    let mut paths_final: Vec<String> = Vec::new();
    for p in priorities {
        if paths.contains_key(&p) {
            paths_final.push((&p).to_string());
            paths.remove(&p);
        }
    }

    // Now go through all the ones that were not prioritized, and add them at
    // the end. But also dedup while we're at it.
    let mut leftovers: Vec<(i32, String)> = Vec::new();
    for (p, idx) in paths.iter() {
        leftovers.push((*idx, p.to_string()));
    }
    leftovers.sort();
    let mut seen: HashSet<String> = HashSet::new();
    for (_, p) in leftovers {
        if !seen.contains(&p) {
            paths_final.push(p.clone());
            seen.insert(p);
        }
    }

    paths_final.join(":")
}

// Convert paths ($PATH) so that all instances of (basically) "/home/foo" are
// replaced with just "~".
fn make_canonical_paths(paths: &str, home: &str) -> Vec<String> {
    let parts = paths.split(":");
    parts.map(|x| x.replacen(home, "~", 1)).collect()
}

#[cfg(test)]
mod test {

    // From https://stackoverflow.com/a/38183903/437583.
    macro_rules! vec_strings {
        ($($x:expr),*) => (vec![$($x.to_string()),*]);
    }

    use super::*;

    #[test]
    fn test_paths_sort() {
        let prio = "~/a:~/b:~/c";
        // Empty case.
        assert_eq!(_paths_sort(&vec_strings![], prio), "");
        // No prioritization == NOP.
        assert_eq!(_paths_sort(&vec_strings!["~/b", "~/a"], ""), "~/b:~/a");
        // No prioritization == NOP (excep deduping).
        assert_eq!(
            _paths_sort(&vec_strings!["~/b", "~/b", "~/a"], ""),
            "~/b:~/a"
        );
        // Simple sort.
        assert_eq!(_paths_sort(&vec_strings!["~/b", "~/a"], prio), "~/a:~/b");
        // Dedup and sort.
        assert_eq!(
            _paths_sort(&vec_strings!["~/b", "~/b", "~/a", "~/a"], prio),
            "~/a:~/b"
        );
        // Dedup, sort, and collect unprioritized paths (and preserve their
        // order).
        assert_eq!(
            _paths_sort(
                &vec_strings!["~/c", "~/foo", "~/a", "~/bar/baz", "~/foo", "~/b"],
                prio
            ),
            "~/a:~/b:~/c:~/foo:~/bar/baz"
        );
    }
}
