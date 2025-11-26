#!/usr/bin/env bash
set -uo pipefail

# Run compilation and execution for all Rust subset samples in the codes/ directory.
# Generates a Markdown report summarizing compile and run results.

repo_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
codes_dir="$repo_root/codes"
report_path="$repo_root/report.md"

if [ ! -d "$codes_dir" ]; then
    echo "codes directory not found at $codes_dir" >&2
    exit 1
fi

# Ensure zig is available before starting work.
if ! command -v zig >/dev/null 2>&1; then
    echo "zig is required to build the compiler" >&2
    exit 1
fi

compiler_bin="$repo_root/zig-out/bin/rust-compiler"
echo "Building compiler..."
if ! (cd "$repo_root" && zig build >/dev/null); then
    echo "Failed to build compiler" >&2
    exit 1
fi

if [ ! -x "$compiler_bin" ]; then
    echo "compiler binary not found at $compiler_bin" >&2
    exit 1
fi

tmp_dir="$(mktemp -d)"
cleanup() {
    rm -rf "$tmp_dir"
}
trap cleanup EXIT

printf "# Codes compilation report\n\n" > "$report_path"
printf "Generated: %s\\n\\n" "$(date --iso-8601=seconds)" >> "$report_path"

status_line() {
    local label="$1"
    local status="$2"
    if [ "$status" -eq 0 ]; then
        printf -- "- %s: success\\n" "$label" >> "$report_path"
    else
        printf -- "- %s: failed\\n" "$label" >> "$report_path"
    fi
}

compile_and_run() {
    local source_path="$1"
    local base
    base="$(basename -- "$source_path")"
    local stem="${base%.rs}"
    local asm_path="$tmp_dir/${stem}.s"

    echo "Processing ${base}..."
    printf "## %s\n\n" "$base" >> "$report_path"

    local compile_output
    local compile_status=0
    if command -v timeout >/dev/null 2>&1; then
        if compile_output=$(timeout 30s "$compiler_bin" --emit=asm -o "$asm_path" "$source_path" 2>&1); then
            compile_status=0
        else
            compile_status=$?
        fi
    else
        if compile_output=$("$compiler_bin" --emit=asm -o "$asm_path" "$source_path" 2>&1); then
            compile_status=0
        else
            compile_status=$?
        fi
    fi

    status_line "Compile" "$compile_status"
    if [ "$compile_status" -eq 124 ]; then
        printf "Compilation terminated after timeout (30s).\n\n" >> "$report_path"
    fi

    printf "#### Compile output\n\n``````\n%s\n``````\n\n" "$compile_output" >> "$report_path"

    if [ "$compile_status" -ne 0 ]; then
        printf "#### Run output\n\nSkipped because compilation failed.\n\n" >> "$report_path"
        return
    fi

    local run_output
    local run_status=0
    if command -v timeout >/dev/null 2>&1; then
        if run_output=$(timeout 10s "$repo_root/run_asm.sh" "$asm_path" <<<"" 2>&1); then
            run_status=0
        else
            run_status=$?
        fi
    else
        if run_output=$("$repo_root/run_asm.sh" "$asm_path" <<<"" 2>&1); then
            run_status=0
        else
            run_status=$?
        fi
    fi

    status_line "Run" "$run_status"
    if [ "$run_status" -eq 124 ]; then
        printf "Run terminated after timeout (10s).\n\n" >> "$report_path"
    fi
    printf "#### Run output\n\n``````\n%s\n``````\n\n" "$run_output" >> "$report_path"
}

shopt -s nullglob
code_files=("$codes_dir"/*.rs)
if [ ${#code_files[@]} -eq 0 ]; then
    echo "No .rs files found in $codes_dir" >&2
    exit 1
fi

for code_path in "${code_files[@]}"; do
    compile_and_run "$code_path"
done

echo "Report written to $report_path"
