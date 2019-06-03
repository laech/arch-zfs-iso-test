#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

if [[ $# != 2 ]]; then
    echo "Usage: <this-program> <expected-zfs-version> <iso>"
    exit 1
fi

[[ ! -e "$2" ]] \
    && echo "error: $2 does not exist" 1>&2 \
    && exit 1

[[ ! -f "$2" ]] \
    && echo "error: $2 is not a file" 1>&2 \
    && exit 1

readonly iso=$(realpath "$2")

cd "$(dirname $0)"

stack setup
stack build
stack exec arch-zfs-iso-test "$1" "$iso"
