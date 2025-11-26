#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <assembly-file> [program-args...]" >&2
    exit 1
fi

asm_path="$1"
shift

if [ ! -f "$asm_path" ]; then
    echo "Assembly file not found: $asm_path" >&2
    exit 1
fi

cc_bin="${CC:-gcc}"
asm_abs=$(realpath "$asm_path")
tmp_dir=$(mktemp -d)
trap 'rm -rf "$tmp_dir"' EXIT

exe_path="$tmp_dir/$(basename "${asm_abs%.s}")"

"$cc_bin" -no-pie "$asm_abs" -o "$exe_path"
"$exe_path" "$@"
