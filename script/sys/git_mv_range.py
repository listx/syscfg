#!/usr/bin/env python3
""" Reorder the lines in a `git rebase -i ...' buffer. """

from __future__ import print_function
import sys


class IndeterminateCommitRange(Exception):
    # pylint: disable=missing-docstring
    pass


def populate_cherries(commit_range_start, commit_range_end, fpath):
    """ Break up the long list of cherry-picks into the bridge, range, and
    tail. The bridge comprises of all commits that come ancestrally prior (if
    any) to the range, wheras the tail comes afterwards (the range's
    descendants). """

    fobj = open(fpath, 'r')
    commits_bridge = []
    commits_range = []
    commits_tail = []
    commits_total = 0

    # Commits are listed oldest to newest.
    on_bridge = True
    on_range = False
    range_start_detected = False
    range_end_detected = False
    for line in fobj.readlines():
        if line.startswith("pick"):
            commits_total += 1
            short_sha = line.split(" ")[1]
            if on_bridge:
                if commit_range_start.startswith(short_sha):
                    on_bridge = False
                    on_range = True
                    range_start_detected = True
                    commits_range.append(short_sha)
                    # It could be that the commit range is only 1 commit long.
                    if commit_range_end.startswith(short_sha):
                        on_range = False
                        range_end_detected = True
                else:
                    commits_bridge.append(short_sha)
            elif on_range:
                if commit_range_end.startswith(short_sha):
                    on_range = False
                    range_end_detected = True
                commits_range.append(short_sha)
            else:
                commits_tail.append(short_sha)
    fobj.close()

    if not range_start_detected:
        print("Could not detect commit range start")
        raise IndeterminateCommitRange
    if not range_end_detected:
        print("Could not detect commit range end")
        raise IndeterminateCommitRange
    if len(commits_bridge + commits_range + commits_tail) != commits_total:
        print("Commit range parsing error")
        raise IndeterminateCommitRange

    return (commits_bridge, commits_range, commits_tail)


def reorder(commit_range_start, commit_range_end, fpath):
    """ Reorder commits from `bridge + range + tail' to `range + bridge +
    tail'. """

    try:
        res = populate_cherries(commit_range_start, commit_range_end, fpath)
    except IndeterminateCommitRange:
        # Abort rebase if we could not populate cherries.
        return

    fobj = open(fpath, 'w')
    commits_bridge = res[0]
    commits_range = res[1]
    commits_tail = res[2]
    reordered = commits_range + commits_bridge + commits_tail
    output_buffer = ""
    for commit in reordered:
        output_buffer += "pick " + commit + "\n"
    fobj.write(output_buffer)
    fobj.close()


if __name__ == "__main__":
    if len(sys.argv) == 4:
        # Usage: git_mv_range.py COMMIT_RANGE_START COMMIT_RANGE_END REBASE_BUFFER_FILEPATH # noqa
        # Both the COMMIT_RANGE_START and COMMIT_RANGE_END arguments must be
        # the long 40-character commit SHAs.
        reorder(sys.argv[1], sys.argv[2], sys.argv[3])
    else:
        # Last argument will always be the filename from GIT_SEQUENCE_EDITOR.
        # Anyway, truncate the file (abort the rebase) if we are not given
        # proper arguments.
        with open(sys.argv[-1], 'w') as f:
            f.truncate()
