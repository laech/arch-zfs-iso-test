#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

if [[ $# != 1 ]]; then
    echo "Usage: <this-program> <iso>"
    exit 1
fi

cd "$(dirname $0)"

stack setup
stack build
stack exec arch-zfs-iso-test 0.6.5.10 "$1"
