#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <rust code>" >&2
    exit 1
fi

rust_path="$1"
shift

if [ ! -f "$rust_path" ]; then
    echo "Rust file not found: $rust_path" >&2
    exit 1
fi

zig run src/main.zig -- "$rust_path" "$@" -o s.s && ./run_asm.sh s.s
