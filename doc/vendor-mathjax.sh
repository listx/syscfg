#!/usr/bin/env bash
set -euo pipefail

MAIN_PACKAGE="mathjax"
FONT_PACKAGE="mathjax-newcm-font"
VERSION="4.1.2"

mkdir -p vendor
pushd vendor

download_tarball()
{
	local package
	local version
	local url_prefix
	local tarball
	package="${1:-}"
	version="${2:-}"
	tarball="${package}-${version}.tgz"
	url_prefix="${3:-}"

	if [[ -d "${package}" ]]; then
		echo "${package} directory already exists"
		return 1
	fi

	curl -OL "${url_prefix}/-/${tarball}"
	tar xzvf "${tarball}"
	mv package "${package}"
	rm "${tarball}"
}

download_tarball \
	"${MAIN_PACKAGE}" \
	"${VERSION}" \
	"https://registry.npmjs.org/mathjax"
download_tarball \
	"${FONT_PACKAGE}" \
	"${VERSION}" \
	"https://registry.npmjs.org/@mathjax/${FONT_PACKAGE}"
