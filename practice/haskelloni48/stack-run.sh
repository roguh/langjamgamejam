#!/usr/bin/env bash
set -x
set -e
set -o pipefail
PROGRAM="$1"

stack run -- "$PROGRAM" > .output.js
cat .output.js > /dev/stderr

if ! command -v node > /dev/null; then
    echo "Please install node to run the output"
    exit 1
fi

node .output.js
