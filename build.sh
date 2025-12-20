#!/usr/bin/env bash
set -x
IN="build/dev/javascript/"
OUT="game/src/gleamjunk"
gleam build --target javascript && \
    rm -rf "$OUT" && \
    cp -r "$IN" "$OUT"
