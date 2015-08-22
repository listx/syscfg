#!/usr/bin/env bash

# This script fixes files in the index and working tree based on the following
# three criteria:
#
#   (1) lack of final newlines,
#   (2) trailing whitespace, and
#   (3) trailing blank lines at EOF,
#
# in that order. All three categories of errors are fixed only if they are in
# the staging area. Any such errors elsewhere in the repository are untouched.
# Trailing whitespace is only corrected on those specific lines added into the
# staging area; i.e., such lines that already exist from a previous commit are
# left alone.
#
# The last category (trailing blank lines of a file) is the only category
# treated aggressively. Adding a trailing blank line to the staging area
# triggers the deletion of *all* such blank lines in the file.

# We define "newline" as the traditional UNIX line feed character (0x0a).

set -e

function print_hash()
{
	hash_definition=$(declare -p $1)
	eval "declare -A hash="${hash_definition#*=}
	for key in "${!hash[@]}"; do
		echo -n "$key"
	done | sort
}

errors_bl=0
errors_nl=0
errors_tw=0
noeol="^\\\ No newline at end of file"
declare -a bl nl
declare -A tw

# Mark files without a final newline.
while read file; do
	if (($(git status --porcelain "$file" | grep -v "^D" | wc -l))) \
		&& (($(git diff --cached "$file" \
			| tail -n1 | grep "$noeol" | wc -l))); then
		nl+=("$file")
		errors_nl=1
	fi
done < <(git diff --cached --name-only)

# Mark files that have trailing whitespace.
while read line; do
	file=$(echo $line | cut -d: -f1)
	lnum=$(echo $line | cut -d: -f2)
	mode=$(git ls-files --stage | cut -d " " -f1)
	if [ -z "${tw[$file]}" ]; then
		tw[$file]=$lnum
	else
		tw[$file]+=" $lnum"
	fi
	errors_tw=1
done < <(git diff --check --cached \
	| grep 'trailing whitespace\.$' \
	| sed -e '/^+/d')

# Mark files that have unnecessary trailing blank lines. By default, Git does
# not mark all newly-added trailing blank lines; i.e., a file may have 100 newly
# added trailing blank lines, but Git will only complain on the first such line.
while read line; do
	file=$(echo $line | cut -d: -f1)
	bl+=("$file")
	errors_bl=1
done < <(git diff --check --cached \
	| grep "new blank line at EOF\.$" \
	| sed -e '/^+/d')

# Fix all files which need to be fixed for the final newline.
if ((errors_nl)); then
	for file in ${nl[*]}; do
		mode=$(git ls-files --stage $file | cut -d " " -f1)
		sha1=$((cat $file && echo) | git hash-object -w --stdin)
		# Fix index.
		git update-index --cacheinfo $mode,$sha1,$file
		# Fix working tree.
		echo >> $file
	done
fi

# Fix all files which need to be fixed for trailing whitespace.
if ((errors_tw)); then
	print_hash tw | while read file; do
		bools=""
		for lnum in ${tw[$file]}; do
			if [ -z "$bools" ]; then
				bools=" \$. == $lnum"
			else
				bools+=" || \$. == $lnum"
			fi
		done
		mode=$(git ls-files --stage $file | cut -d " " -f1)
		sha1=$(perl -p -e "s/[\t ]+$// if ($bools)" $file \
			| git hash-object -w --stdin)
		# Fix index.
		git update-index --cacheinfo $mode,$sha1,$file
		# Fix working tree.
		perl -pi -e "s/[\t ]+$// if ($bools)" $file
	done
fi

# Fix all files which need to be fixed for one or more trailing blank lines. It
# could be the case that a new trailing blank line was added on top of
# already-committed, existing blank lines. We delete these lines as well. This
# is the only portion of this script that is "intrusive" in the sense that we
# are touching lines that were *not* explicitly added to the index by the user.
#
# Because we first trim all trailing whitespace above, we only concern ourselves
# with consecutive newline characters.
if ((errors_bl)); then
	for file in ${bl[*]}; do
		mode=$(git ls-files --stage $file | cut -d " " -f1)
		sha1=$(perl -0777 -p -e "s/\n+\z/\n/" $file \
			| git hash-object -w --stdin)
		# Fix index.
		git update-index --cacheinfo $mode,$sha1,$file
		# Fix working tree.
		perl -0777 -pi -e "s/\n+\z/\n/" $file
	done
fi
