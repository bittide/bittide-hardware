#!/bin/bash
# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# Verify RISC-V architecture features of compiled firmware binaries
# This checks that per-program .cargo/config.toml files are correctly setting
# target features to match the CPU architecture.

check_binary() {
    local prog=$1
    local expected_m=$2  # "0" for no M-extension, ">0" for M-extension expected
    local expected_f=$3  # "0" for no F-extension, ">0" for F-extension expected
    
    binary="_build/cargo/firmware-binaries/riscv32imc-unknown-none-elf/release/$prog"
    
    if [ ! -f "$binary" ]; then
        echo "ERROR: Binary not found: $binary"
        return 1
    fi
    
    echo "=== $prog ==="
    
    # Check basic architecture from ELF header
    file "$binary" | grep -o "RISC-V.*"
    
    # Disassemble and check for specific instruction patterns
    objdump -d "$binary" > /tmp/disasm_$prog.txt 2>/dev/null
    
    # Count M-extension instructions (mul, div, rem and variants)
    m_count=$(grep -E '^\s+[0-9a-f]+:\s+[0-9a-f]+\s+(mul|div|divu|rem|remu|mulh|mulhsu|mulhu)' /tmp/disasm_$prog.txt | wc -l)
    
    # Count F-extension instructions (floating point operations)
    f_count=$(grep -E '^\s+[0-9a-f]+:\s+[0-9a-f]+\s+(flw|fsw|fadd|fsub|fmul|fdiv|fsqrt|fcvt|fmv)' /tmp/disasm_$prog.txt | wc -l)
    
    echo "  M-extension instructions: $m_count (expected: $expected_m)"
    echo "  F-extension instructions: $f_count (expected: $expected_f)"
    
    # Validate expectations
    status="✓ PASS"
    if [ "$expected_m" = "0" ] && [ "$m_count" -gt "0" ]; then
        echo "  ❌ FAIL: Found M-extension instructions when none expected!"
        status="❌ FAIL"
    elif [ "$expected_m" = ">0" ] && [ "$m_count" -eq "0" ]; then
        echo "  ⚠️  Note: No M-extension instructions found (may be optimized out or not used)"
    fi
    
    if [ "$expected_f" = "0" ] && [ "$f_count" -gt "0" ]; then
        echo "  ❌ FAIL: Found F-extension instructions when none expected!"
        status="❌ FAIL"
    elif [ "$expected_f" = ">0" ] && [ "$f_count" -eq "0" ]; then
        echo "  ⚠️  Note: No F-extension instructions found (may not be used in this program)"
    fi
    
    echo "  $status"
    echo ""
    rm -f /tmp/disasm_$prog.txt
    
    [ "$status" = "✓ PASS" ]
}

echo "Verifying RISC-V architecture of firmware binaries..."
echo "This checks that .cargo/config.toml target-feature flags are working correctly."
echo ""

failed=0

# Boot CPU programs: rv32ic (no M extension, no F extension)
check_binary "switch-demo1-boot" "0" "0" || ((failed++))

# Management Unit programs: rv32imc (M extension, no F extension)
check_binary "soft-ugn-mu" ">0" "0" || ((failed++))
check_binary "switch-demo1-mu" ">0" "0" || ((failed++))
check_binary "switch-demo2-mu" ">0" "0" || ((failed++))

# GPPE programs: rv32imc (M extension, no F extension)
check_binary "soft-ugn-gppe" ">0" "0" || ((failed++))
check_binary "switch-demo2-gppe" ">0" "0" || ((failed++))

# Clock Control programs: rv32imcf (M extension, F extension)
check_binary "clock-control" ">0" ">0" || ((failed++))

if [ $failed -eq 0 ]; then
    echo "✓ All architecture verifications passed!"
    exit 0
else
    echo "❌ $failed verification(s) failed!"
    exit 1
fi
