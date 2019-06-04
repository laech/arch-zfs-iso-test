#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

if [[ $# != 1 ]]; then
    echo "Usage: <this-program> <iso>"
    exit 1
fi

[[ ! -e "$1" ]] \
    && echo "error: $1 does not exist" 1>&2 \
    && exit 1

[[ ! -f "$1" ]] \
    && echo "error: $1 is not a file" 1>&2 \
    && exit 1

readonly iso=$(realpath "$1")

cd "$(dirname $0)"

stack setup
stack build
stack exec arch-zfs-iso-test "$iso"
