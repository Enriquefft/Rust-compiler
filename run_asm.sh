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

cc_bin="${CC:-clang}"
asm_abs=$(realpath "$asm_path")
tmp_dir=$(mktemp -d)
trap 'rm -rf "$tmp_dir"' EXIT

exe_path="$tmp_dir/$(basename "${asm_abs%.s}")"

extra_flags=()
if "$cc_bin" --version 2>/dev/null | grep -qi "clang"; then
    extra_flags+=("-Wno-everything")
elif "$cc_bin" --version 2>/dev/null | grep -qi "gcc"; then
    extra_flags+=("-w")
fi

"$cc_bin" -no-pie "${extra_flags[@]}" "$asm_abs" -o "$exe_path"
"$exe_path" "$@"
