#!/usr/bin/env bash

set -euo pipefail

copy_clipboard()
{
	case "$(uname)" in
	Linux) xsel --clipboard ;;
	Darwin) pbcopy ;;
	esac
}

main()
{
    copy_clipboard
}

main
