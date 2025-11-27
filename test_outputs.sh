#!/usr/bin/env bash
set -uo pipefail

repo_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
codes_dir="$repo_root/codes"
expected_outputs_file="$codes_dir/expected_outputs.txt"
report_path="$repo_root/test_report.md"

if [ ! -d "$codes_dir" ]; then
    echo "codes directory not found at $codes_dir" >&2
    exit 1
fi

if [ ! -f "$expected_outputs_file" ]; then
    echo "expected_outputs.txt not found at $expected_outputs_file" >&2
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

passed=0
failed=0
skipped=0

printf "# Output comparison test report\n\n" > "$report_path"
printf "Generated: %s\n\n" "$(date --iso-8601=seconds)" >> "$report_path"

# Parse expected outputs file into associative array
declare -A expected_outputs
while IFS= read -r line || [ -n "$line" ]; do
    # Skip comments and empty lines
    [[ "$line" =~ ^[[:space:]]*# ]] && continue
    [[ -z "${line// /}" ]] && continue

    # Parse filename=expected_output
    if [[ "$line" =~ ^([^=]+)=(.*)$ ]]; then
        filename="${BASH_REMATCH[1]}"
        # Process escape sequences (like \n)
        expected="${BASH_REMATCH[2]}"
        expected_outputs["$filename"]="$expected"
    fi
done < "$expected_outputs_file"

compile_run_and_compare() {
    local source_path="$1"
    local base; base="$(basename -- "$source_path")"
    local stem="${base%.rs}"
    local asm_path="$tmp_dir/${stem}.s"

    printf "Testing %s..." "$base"

    printf "## %s\n\n" "$base" >> "$report_path"

    # Check if expected output exists
    if [ -z "${expected_outputs[$base]+isset}" ]; then
        printf "#### Status: SKIPPED\n\nNo expected output defined.\n\n" >> "$report_path"
        skipped=$((skipped + 1))
        echo " ⏭️  (no expected output)"
        return
    fi

    local expected="${expected_outputs[$base]}"
    # Process escape sequences
    expected=$(printf '%b' "$expected")

    local compile_output compile_status run_output run_status
    compile_status=0
    run_status=1

    # Compile
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

    if [ "$compile_status" -ne 0 ]; then
        printf "#### Status: FAILED\n\n**Compilation failed:**\n\n``````\n%s\n``````\n\n" "$compile_output" >> "$report_path"
        failed=$((failed + 1))
        echo " ❌ (compilation failed)"
        return
    fi

    # Run
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

    if [ "$run_status" -ne 0 ]; then
        printf "#### Status: FAILED\n\n**Execution failed:**\n\n``````\n%s\n``````\n\n" "$run_output" >> "$report_path"
        failed=$((failed + 1))
        echo " ❌ (execution failed)"
        return
    fi

    # Compare output
    if [ "$run_output" = "$expected" ]; then
        printf "#### Status: PASSED\n\n**Output:**\n\n``````\n%s\n``````\n\n" "$run_output" >> "$report_path"
        passed=$((passed + 1))
        echo " ✔️"
    else
        printf "#### Status: FAILED\n\n**Expected:**\n\n``````\n%s\n``````\n\n**Actual:**\n\n``````\n%s\n``````\n\n" "$expected" "$run_output" >> "$report_path"
        failed=$((failed + 1))
        echo " ❌ (output mismatch)"
    fi
}

shopt -s nullglob
code_files=("$codes_dir"/*.rs)
if [ ${#code_files[@]} -eq 0 ]; then
    echo "No .rs files found in $codes_dir" >&2
    exit 1
fi

for code_path in "${code_files[@]}"; do
    compile_run_and_compare "$code_path"
done

printf "## Summary\n\n" >> "$report_path"
printf "- Passed: %d\n" "$passed" >> "$report_path"
printf "- Failed: %d\n" "$failed" >> "$report_path"
printf "- Skipped: %d\n" "$skipped" >> "$report_path"

echo ""
echo "Results: $passed passed, $failed failed, $skipped skipped"
echo "Report written to $report_path"

# Exit with failure if any tests failed
if [ "$failed" -gt 0 ]; then
    exit 1
fi
