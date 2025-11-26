#!/usr/bin/env bash
set -uo pipefail

repo_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
codes_dir="$repo_root/codes"
report_path="$repo_root/report.md"

if [ ! -d "$codes_dir" ]; then
    echo "codes directory not found at $codes_dir" >&2
    exit 1
fi

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
cleanup() { rm -rf "$tmp_dir"; }
trap cleanup EXIT

success_compile=0
success_run=0
failed_both=0

printf "# Codes compilation report\n\n" > "$report_path"
printf "Generated: %s\n\n" "$(date --iso-8601=seconds)" >> "$report_path"

status_line() {
    local label="$1"
    local status="$2"
    if [ "$status" -eq 0 ]; then
        printf -- "- %s: success\n" "$label" >> "$report_path"
    else
        printf -- "- %s: failed\n" "$label" >> "$report_path"
    fi
}

compile_and_run() {
    local source_path="$1"
    local base; base="$(basename -- "$source_path")"
    local stem="${base%.rs}"
    local asm_path="$tmp_dir/${stem}.s"

    printf "Processing %s..." "$base"

    printf "## %s\n\n" "$base" >> "$report_path"

    local compile_output compile_status run_output run_status
    compile_status=0
    run_status=1

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
    printf "#### Compile output\n\n``````\n%s\n``````\n\n" "$compile_output" >> "$report_path"

    if [ "$compile_status" -ne 0 ]; then
        printf "#### Run output\n\nSkipped because compilation failed.\n\n" >> "$report_path"
    else
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
        printf "#### Run output\n\n``````\n%s\n``````\n\n" "$run_output" >> "$report_path"
    fi

    if [ "$compile_status" -eq 0 ]; then
        success_compile=$((success_compile + 1))
    fi

    # Terminal annotation logic
    if [ "$compile_status" -eq 0 ] && [ "$run_status" -eq 0 ]; then
        success_run=$((success_run + 1))
        echo " ✔️"
    elif [ "$compile_status" -eq 0 ] && [ "$run_status" -ne 0 ]; then
        failed_both=$((failed_both + 1))
        echo " ⚠️"
    else
        failed_both=$((failed_both + 1))
        echo " ❌"
    fi
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

printf "## Summary\n\n" >> "$report_path"
printf "- Successful compilations: %d\n" "$success_compile" >> "$report_path"
printf "- Successful executions: %d\n" "$success_run" >> "$report_path"
printf "- Failed overall (compile or run): %d\n" "$failed_both" >> "$report_path"

echo "Report written to $report_path"
