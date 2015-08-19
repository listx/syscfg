#!/usr/bin/env bash

# This script fixes trailing whitespace and files without final newlines in the
# index as well as the working tree. This is a conservative script that fixes
# only those lines that were added into the index.

set -e

function print_hash()
{
	hash_definition=$(declare -p $1)
	eval "declare -A hash="${hash_definition#*=}
	for key in "${!hash[@]}"; do
		echo -n "$key"
		if [ "$2" == full ]; then
			echo " ${hash[$key]}"
		else
			echo
		fi
	done | sort
}

errors_nl=0
errors_tw=0
noeol="\\ No newline at end of file"
declare -A tw nl

# Detect files without a newline at the end.
errors_nl=$(($(git diff --cached | grep "$noeol" | wc -l)))

# Detect trailing whitespace.
errors_tw=$(($(git diff --check --cached \
	| grep -v "new blank line at EOF." \
	| wc -l)))

# Mark files without a final newline.
if ((errors_nl)); then
	while read file; do
		if (($(git diff --cached "$file" | grep "$noeol" | wc -l))); then
			nl[$file]="x"
		fi
	done < <(git diff --cached --name-only)
fi

# Mark files that have trailing whitespace.
if ((errors_tw)); then
	while read line; do
		file=$(echo $line | cut -d: -f1)
		lnum=$(echo $line | cut -d: -f2)
		mode=$(git ls-files --stage | cut -d " " -f1)
		if [ -z "${tw[$file]}" ]; then
			tw[$file]=$lnum
		else
			tw[$file]+=" $lnum"
		fi
	done < <(git diff --check --cached \
		| grep -v 'new blank line at EOF\.' \
		| sed -e '/^+/d' -e 's/: trailing whitespace\.$//' -e 's/^/  /')
	fi

# Fix all files which need to be fixed for the final newline.
if ((errors_nl)); then
	print_hash nl | while read file; do
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
