// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! RISC-V instruction decoding and extension classification

use crate::build_utils::RiscvExtension;

/// Classify a RISC-V instruction by its extension and return its length in bytes.
pub fn classify_instruction(bytes: &[u8]) -> (Option<RiscvExtension>, usize) {
    let len = instruction_length(bytes);

    // Skip zero instructions (likely padding)
    if is_zero_instruction(bytes, len) {
        return (None, len);
    }

    if len == 2 {
        return (Some(RiscvExtension::C), len);
    }

    // Check each extension in order
    if is_m_extension(bytes) {
        return (Some(RiscvExtension::M), len);
    }

    if is_a_extension(bytes) {
        return (Some(RiscvExtension::A), len);
    }

    if is_f_extension(bytes) {
        return (Some(RiscvExtension::F), len);
    }

    if is_d_extension(bytes) {
        return (Some(RiscvExtension::D), len);
    }

    if is_q_extension(bytes) {
        return (Some(RiscvExtension::Q), len);
    }

    if is_zfh_extension(bytes) {
        return (Some(RiscvExtension::Zfh), len);
    }

    if is_zicsr_extension(bytes) {
        return (Some(RiscvExtension::Zicsr), len);
    }

    if is_zifencei_extension(bytes) {
        return (Some(RiscvExtension::Zifencei), len);
    }

    if is_zawrs_extension(bytes) {
        return (Some(RiscvExtension::Zawrs), len);
    }

    if is_zba_extension(bytes) {
        return (Some(RiscvExtension::Zba), len);
    }

    if is_zbc_extension(bytes) {
        return (Some(RiscvExtension::Zbc), len);
    }

    if is_zbs_extension(bytes) {
        return (Some(RiscvExtension::Zbs), len);
    }

    if is_zbkb_extension(bytes) {
        return (Some(RiscvExtension::Zbkb), len);
    }

    if is_zbb_extension(bytes) {
        return (Some(RiscvExtension::Zbb), len);
    }

    if is_i_extension(bytes) {
        return (Some(RiscvExtension::I), len);
    }

    (None, len)
}

fn instruction_length(bytes: &[u8]) -> usize {
    let target = riscv_isa::Target {
        c: true,
        ..Default::default()
    };

    match riscv_isa::decode_le_bytes(bytes, &target) {
        Some((_, 4)) if bytes.len() >= 4 => 4,
        Some((_, 2)) if bytes.len() >= 2 => 2,
        _ => 2,
    }
}

fn is_zero_instruction(bytes: &[u8], len: usize) -> bool {
    match len {
        2 if bytes.len() >= 2 => u16::from_le_bytes([bytes[0], bytes[1]]) == 0,
        4 if bytes.len() >= 4 => u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]) == 0,
        _ => false,
    }
}

/// Check if an instruction belongs to the I extension (base integer ISA)
fn is_i_extension(bytes: &[u8]) -> bool {
    use riscv_isa::Instruction::UNIMP;

    let target = riscv_isa::Target {
        c: true,
        ..Default::default()
    };

    match riscv_isa::decode_le_bytes(bytes, &target) {
        Some((UNIMP, _)) | None => false,
        Some(_) => true,
    }
}

/// Check if an instruction belongs to the M extension (integer multiply/divide)
fn is_m_extension(bytes: &[u8]) -> bool {
    use riscv_isa::Instruction::*;

    let target = riscv_isa::Target {
        m: true,
        c: true,
        ..Default::default()
    };

    let decoded = riscv_isa::decode_le_bytes(bytes, &target);

    matches!(
        decoded,
        Some((MUL { .. }, _))
            | Some((MULH { .. }, _))
            | Some((MULHSU { .. }, _))
            | Some((MULHU { .. }, _))
            | Some((DIV { .. }, _))
            | Some((DIVU { .. }, _))
            | Some((REM { .. }, _))
            | Some((REMU { .. }, _))
            | Some((MULW { .. }, _))
            | Some((DIVW { .. }, _))
            | Some((DIVUW { .. }, _))
            | Some((REMW { .. }, _))
            | Some((REMUW { .. }, _))
    )
}

/// Check if an instruction belongs to the A extension (atomics)
fn is_a_extension(bytes: &[u8]) -> bool {
    use riscv_isa::Instruction::*;

    let target = riscv_isa::Target {
        a: true,
        c: true,
        ..Default::default()
    };

    let decoded = riscv_isa::decode_le_bytes(bytes, &target);

    matches!(
        decoded,
        Some((LR_W { .. }, _))
            | Some((SC_W { .. }, _))
            | Some((AMOSWAP_W { .. }, _))
            | Some((AMOADD_W { .. }, _))
            | Some((AMOXOR_W { .. }, _))
            | Some((AMOAND_W { .. }, _))
            | Some((AMOOR_W { .. }, _))
            | Some((AMOMIN_W { .. }, _))
            | Some((AMOMAX_W { .. }, _))
            | Some((AMOMINU_W { .. }, _))
            | Some((AMOMAXU_W { .. }, _))
            | Some((LR_D { .. }, _))
            | Some((SC_D { .. }, _))
            | Some((AMOSWAP_D { .. }, _))
            | Some((AMOADD_D { .. }, _))
            | Some((AMOXOR_D { .. }, _))
            | Some((AMOAND_D { .. }, _))
            | Some((AMOOR_D { .. }, _))
            | Some((AMOMIN_D { .. }, _))
            | Some((AMOMAX_D { .. }, _))
            | Some((AMOMINU_D { .. }, _))
            | Some((AMOMAXU_D { .. }, _))
    )
}

/// Check if an instruction belongs to the F extension (single-precision floating-point)
fn is_f_extension(bytes: &[u8]) -> bool {
    use riscv_isa::Instruction::*;

    // Decode with M extension first to check for base ISA instructions
    let target_m = riscv_isa::Target {
        m: true,
        c: true,
        ..Default::default()
    };
    let decoded_m = riscv_isa::decode_le_bytes(bytes, &target_m);

    // Decode with F extension
    let target_f = riscv_isa::Target {
        f: true,
        c: true,
        ..Default::default()
    };
    let decoded_f = riscv_isa::decode_le_bytes(bytes, &target_f);

    // Check if it's an F instruction
    let is_f_instr = matches!(
        decoded_f,
        Some((FLW { .. }, _))
            | Some((FSW { .. }, _))
            | Some((FMADD_S { .. }, _))
            | Some((FMSUB_S { .. }, _))
            | Some((FNMSUB_S { .. }, _))
            | Some((FNMADD_S { .. }, _))
            | Some((FADD_S { .. }, _))
            | Some((FSUB_S { .. }, _))
            | Some((FMUL_S { .. }, _))
            | Some((FDIV_S { .. }, _))
            | Some((FSQRT_S { .. }, _))
            | Some((FSGNJ_S { .. }, _))
            | Some((FSGNJN_S { .. }, _))
            | Some((FSGNJX_S { .. }, _))
            | Some((FMIN_S { .. }, _))
            | Some((FMAX_S { .. }, _))
            | Some((FCVT_W_S { .. }, _))
            | Some((FCVT_WU_S { .. }, _))
            | Some((FMV_X_W { .. }, _))
            | Some((FEQ_S { .. }, _))
            | Some((FLT_S { .. }, _))
            | Some((FLE_S { .. }, _))
            | Some((FCLASS_S { .. }, _))
            | Some((FCVT_S_W { .. }, _))
            | Some((FCVT_S_WU { .. }, _))
            | Some((FMV_W_X { .. }, _))
            | Some((FCVT_L_S { .. }, _))
            | Some((FCVT_LU_S { .. }, _))
            | Some((FCVT_S_L { .. }, _))
            | Some((FCVT_S_LU { .. }, _))
    );

    // Make sure it wasn't a base instruction (LW/SW decoded differently without F)
    let is_base_instr = matches!(decoded_m, Some((LW { .. }, _)) | Some((SW { .. }, _)));

    is_f_instr && !is_base_instr
}

/// Check if an instruction belongs to the D extension (double-precision floating-point)
fn is_d_extension(bytes: &[u8]) -> bool {
    use riscv_isa::Instruction::*;

    // Decode with M extension first to check for base ISA instructions
    let target_m = riscv_isa::Target {
        m: true,
        c: true,
        ..Default::default()
    };
    let decoded_m = riscv_isa::decode_le_bytes(bytes, &target_m);

    // Decode with F extension to check for F instructions
    let target_f = riscv_isa::Target {
        f: true,
        c: true,
        ..Default::default()
    };
    let decoded_f = riscv_isa::decode_le_bytes(bytes, &target_f);

    // Decode with D extension
    let target_d = riscv_isa::Target {
        d: true,
        f: true, // D requires F
        c: true,
        ..Default::default()
    };
    let decoded_d = riscv_isa::decode_le_bytes(bytes, &target_d);

    // Check if it's a D instruction
    let is_d_instr = matches!(
        decoded_d,
        Some((FLD { .. }, _))
            | Some((FSD { .. }, _))
            | Some((FMADD_D { .. }, _))
            | Some((FMSUB_D { .. }, _))
            | Some((FNMSUB_D { .. }, _))
            | Some((FNMADD_D { .. }, _))
            | Some((FADD_D { .. }, _))
            | Some((FSUB_D { .. }, _))
            | Some((FMUL_D { .. }, _))
            | Some((FDIV_D { .. }, _))
            | Some((FSQRT_D { .. }, _))
            | Some((FSGNJ_D { .. }, _))
            | Some((FSGNJN_D { .. }, _))
            | Some((FSGNJX_D { .. }, _))
            | Some((FMIN_D { .. }, _))
            | Some((FMAX_D { .. }, _))
            | Some((FCVT_S_D { .. }, _))
            | Some((FCVT_D_S { .. }, _))
            | Some((FEQ_D { .. }, _))
            | Some((FLT_D { .. }, _))
            | Some((FLE_D { .. }, _))
            | Some((FCLASS_D { .. }, _))
            | Some((FCVT_W_D { .. }, _))
            | Some((FCVT_WU_D { .. }, _))
            | Some((FCVT_D_W { .. }, _))
            | Some((FCVT_D_WU { .. }, _))
            | Some((FCVT_L_D { .. }, _))
            | Some((FCVT_LU_D { .. }, _))
            | Some((FMV_X_D { .. }, _))
            | Some((FCVT_D_L { .. }, _))
            | Some((FCVT_D_LU { .. }, _))
            | Some((FMV_D_X { .. }, _))
    );

    // Make sure it wasn't a base or F instruction
    let is_base_instr = matches!(decoded_m, Some((LD { .. }, _)) | Some((SD { .. }, _)));
    let is_f_instr = matches!(decoded_f, Some((FLW { .. }, _)) | Some((FSW { .. }, _)));

    is_d_instr && !is_base_instr && !is_f_instr
}

/// Check if an instruction belongs to the Q extension (quad-precision floating-point)
fn is_q_extension(bytes: &[u8]) -> bool {
    use riscv_isa::Instruction::*;

    let target = riscv_isa::Target {
        q: true,
        d: true,
        f: true,
        c: true,
        ..Default::default()
    };

    let decoded = riscv_isa::decode_le_bytes(bytes, &target);

    matches!(
        decoded,
        Some((FLQ { .. }, _))
            | Some((FSQ { .. }, _))
            | Some((FMADD_Q { .. }, _))
            | Some((FMSUB_Q { .. }, _))
            | Some((FNMSUB_Q { .. }, _))
            | Some((FNMADD_Q { .. }, _))
            | Some((FADD_Q { .. }, _))
            | Some((FSUB_Q { .. }, _))
            | Some((FMUL_Q { .. }, _))
            | Some((FDIV_Q { .. }, _))
            | Some((FSQRT_Q { .. }, _))
            | Some((FSGNJ_Q { .. }, _))
            | Some((FSGNJN_Q { .. }, _))
            | Some((FSGNJX_Q { .. }, _))
            | Some((FMIN_Q { .. }, _))
            | Some((FMAX_Q { .. }, _))
            | Some((FCVT_S_Q { .. }, _))
            | Some((FCVT_Q_S { .. }, _))
            | Some((FCVT_D_Q { .. }, _))
            | Some((FCVT_Q_D { .. }, _))
            | Some((FEQ_Q { .. }, _))
            | Some((FLT_Q { .. }, _))
            | Some((FLE_Q { .. }, _))
            | Some((FCLASS_Q { .. }, _))
            | Some((FCVT_W_Q { .. }, _))
            | Some((FCVT_WU_Q { .. }, _))
            | Some((FCVT_Q_W { .. }, _))
            | Some((FCVT_Q_WU { .. }, _))
            | Some((FCVT_L_Q { .. }, _))
            | Some((FCVT_LU_Q { .. }, _))
            | Some((FCVT_Q_L { .. }, _))
            | Some((FCVT_Q_LU { .. }, _))
    )
}

/// Check if an instruction belongs to the Zicsr extension
fn is_zicsr_extension(bytes: &[u8]) -> bool {
    use riscv_isa::Instruction::*;

    let target = riscv_isa::Target {
        zicsr: true,
        c: true,
        ..Default::default()
    };

    let decoded = riscv_isa::decode_le_bytes(bytes, &target);

    matches!(
        decoded,
        Some((CSRRW { .. }, _))
            | Some((CSRRS { .. }, _))
            | Some((CSRRC { .. }, _))
            | Some((CSRRWI { .. }, _))
            | Some((CSRRSI { .. }, _))
            | Some((CSRRCI { .. }, _))
    )
}

/// Check if an instruction belongs to the Zifencei extension
fn is_zifencei_extension(bytes: &[u8]) -> bool {
    use riscv_isa::Instruction::*;

    let target = riscv_isa::Target {
        zifencei: true,
        c: true,
        ..Default::default()
    };

    let decoded = riscv_isa::decode_le_bytes(bytes, &target);

    matches!(decoded, Some((FENCE_I, _)))
}

/// Check if an instruction belongs to the Zawrs extension
fn is_zawrs_extension(bytes: &[u8]) -> bool {
    use riscv_isa::Instruction::*;

    let target = riscv_isa::Target {
        zawrs: true,
        c: true,
        ..Default::default()
    };

    let decoded = riscv_isa::decode_le_bytes(bytes, &target);

    matches!(decoded, Some((WRS_NTO, _)) | Some((WRS_STO, _)))
}

/// Check if an instruction belongs to the Zfh extension
fn is_zfh_extension(bytes: &[u8]) -> bool {
    use riscv_isa::Instruction::*;

    let target = riscv_isa::Target {
        zfh: true,
        c: true,
        ..Default::default()
    };

    let decoded = riscv_isa::decode_le_bytes(bytes, &target);

    matches!(
        decoded,
        Some((FLH { .. }, _))
            | Some((FSH { .. }, _))
            | Some((FMADD_H { .. }, _))
            | Some((FMSUB_H { .. }, _))
            | Some((FNMSUB_H { .. }, _))
            | Some((FNMADD_H { .. }, _))
            | Some((FADD_H { .. }, _))
            | Some((FSUB_H { .. }, _))
            | Some((FMUL_H { .. }, _))
            | Some((FDIV_H { .. }, _))
            | Some((FSQRT_H { .. }, _))
            | Some((FSGNJ_H { .. }, _))
            | Some((FSGNJN_H { .. }, _))
            | Some((FSGNJX_H { .. }, _))
            | Some((FMIN_H { .. }, _))
            | Some((FMAX_H { .. }, _))
            | Some((FCVT_S_H { .. }, _))
            | Some((FCVT_H_S { .. }, _))
            | Some((FCVT_D_H { .. }, _))
            | Some((FCVT_H_D { .. }, _))
            | Some((FCVT_Q_H { .. }, _))
            | Some((FCVT_H_Q { .. }, _))
            | Some((FEQ_H { .. }, _))
            | Some((FLT_H { .. }, _))
            | Some((FLE_H { .. }, _))
            | Some((FCLASS_H { .. }, _))
            | Some((FCVT_W_H { .. }, _))
            | Some((FCVT_WU_H { .. }, _))
            | Some((FMV_X_H { .. }, _))
            | Some((FCVT_H_W { .. }, _))
            | Some((FCVT_H_WU { .. }, _))
            | Some((FMV_H_X { .. }, _))
            | Some((FCVT_L_H { .. }, _))
            | Some((FCVT_LU_H { .. }, _))
            | Some((FCVT_H_L { .. }, _))
            | Some((FCVT_H_LU { .. }, _))
    )
}

/// Check if an instruction belongs to the Zba extension
fn is_zba_extension(bytes: &[u8]) -> bool {
    use riscv_isa::Instruction::*;

    let target = riscv_isa::Target {
        zba: true,
        c: true,
        ..Default::default()
    };

    let decoded = riscv_isa::decode_le_bytes(bytes, &target);

    matches!(
        decoded,
        Some((SH1ADD { .. }, _))
            | Some((SH2ADD { .. }, _))
            | Some((SH3ADD { .. }, _))
            | Some((ADD_UW { .. }, _))
            | Some((SH1ADD_UW { .. }, _))
            | Some((SH2ADD_UW { .. }, _))
            | Some((SH3ADD_UW { .. }, _))
            | Some((SLLI_UW { .. }, _))
    )
}

/// Check if an instruction belongs to the Zbb extension
fn is_zbb_extension(bytes: &[u8]) -> bool {
    use riscv_isa::Instruction::*;

    let target = riscv_isa::Target {
        zbb: true,
        c: true,
        ..Default::default()
    };

    let decoded = riscv_isa::decode_le_bytes(bytes, &target);

    matches!(
        decoded,
        Some((ANDN { .. }, _))
            | Some((ORN { .. }, _))
            | Some((XNOR { .. }, _))
            | Some((CLZ { .. }, _))
            | Some((CTZ { .. }, _))
            | Some((CPOP { .. }, _))
            | Some((MAX { .. }, _))
            | Some((MAXU { .. }, _))
            | Some((MIN { .. }, _))
            | Some((MINU { .. }, _))
            | Some((SEXT_B { .. }, _))
            | Some((SEXT_H { .. }, _))
            | Some((ZEXT_H { .. }, _))
            | Some((CLZW { .. }, _))
            | Some((CTZW { .. }, _))
            | Some((CPOPW { .. }, _))
            | Some((ROL { .. }, _))
            | Some((ROR { .. }, _))
            | Some((RORI { .. }, _))
            | Some((ORC_B { .. }, _))
            | Some((REV8 { .. }, _))
            | Some((ROLW { .. }, _))
            | Some((RORIW { .. }, _))
            | Some((RORW { .. }, _))
    )
}

/// Check if an instruction belongs to the Zbc extension
fn is_zbc_extension(bytes: &[u8]) -> bool {
    use riscv_isa::Instruction::*;

    let target = riscv_isa::Target {
        zbc: true,
        c: true,
        ..Default::default()
    };

    let decoded = riscv_isa::decode_le_bytes(bytes, &target);

    matches!(
        decoded,
        Some((CLMUL { .. }, _)) | Some((CLMULH { .. }, _)) | Some((CLMULR { .. }, _))
    )
}

/// Check if an instruction belongs to the Zbkb extension
fn is_zbkb_extension(bytes: &[u8]) -> bool {
    use riscv_isa::Instruction::*;

    let target = riscv_isa::Target {
        zbkb: true,
        c: true,
        ..Default::default()
    };

    let decoded = riscv_isa::decode_le_bytes(bytes, &target);

    matches!(
        decoded,
        Some((PACK { .. }, _))
            | Some((PACKH { .. }, _))
            | Some((BREV8 { .. }, _))
            | Some((ZIP { .. }, _))
            | Some((UNZIP { .. }, _))
            | Some((PACKW { .. }, _))
    )
}

/// Check if an instruction belongs to the Zbs extension
fn is_zbs_extension(bytes: &[u8]) -> bool {
    use riscv_isa::Instruction::*;

    let target = riscv_isa::Target {
        zbs: true,
        c: true,
        ..Default::default()
    };

    let decoded = riscv_isa::decode_le_bytes(bytes, &target);

    matches!(
        decoded,
        Some((BCLR { .. }, _))
            | Some((BCLRI { .. }, _))
            | Some((BEXT { .. }, _))
            | Some((BEXTI { .. }, _))
            | Some((BINV { .. }, _))
            | Some((BINVI { .. }, _))
            | Some((BSET { .. }, _))
            | Some((BSETI { .. }, _))
    )
}

struct ExecSection {
    offset: u64,
    size: u64,
    addr: u64,
}

fn executable_sections(data: &[u8]) -> Vec<ExecSection> {
    use goblin::elf::section_header::SHF_EXECINSTR;

    let elf = goblin::elf::Elf::parse(data)
        .unwrap_or_else(|err| panic!("Failed to parse ELF for instruction scan: {}", err));

    elf.section_headers
        .iter()
        .filter(|shdr| shdr.sh_flags & SHF_EXECINSTR as u64 != 0)
        .filter(|shdr| shdr.sh_size > 0)
        .map(|shdr| ExecSection {
            offset: shdr.sh_offset,
            size: shdr.sh_size,
            addr: shdr.sh_addr,
        })
        .collect()
}

/// Find which extensions are present in an ELF binary
fn find_extensions_in_elf(data: &[u8]) -> Vec<RiscvExtension> {
    let mut found = Vec::new();
    let mut has_c = false;
    let mut has_i = false;
    let mut has_m = false;
    let mut has_a = false;
    let mut has_f = false;
    let mut has_d = false;
    let mut has_q = false;
    let mut has_zicsr = false;
    let mut has_zifencei = false;
    let mut has_zawrs = false;
    let mut has_zfh = false;
    let mut has_zba = false;
    let mut has_zbb = false;
    let mut has_zbc = false;
    let mut has_zbkb = false;
    let mut has_zbs = false;

    for section in executable_sections(data) {
        let mut i = section.offset as usize;
        let end = (i + section.size as usize).min(data.len());

        while i + 2 <= end && i + 2 <= data.len() {
            let (ext, len) = classify_instruction(&data[i..end]);

            if let Some(ext) = ext {
                match ext {
                    RiscvExtension::C if !has_c => {
                        has_c = true;
                        found.push(RiscvExtension::C);
                    }
                    RiscvExtension::I if !has_i => {
                        has_i = true;
                        found.push(RiscvExtension::I);
                    }
                    RiscvExtension::M if !has_m => {
                        has_m = true;
                        found.push(RiscvExtension::M);
                    }
                    RiscvExtension::A if !has_a => {
                        has_a = true;
                        found.push(RiscvExtension::A);
                    }
                    RiscvExtension::F if !has_f => {
                        has_f = true;
                        found.push(RiscvExtension::F);
                    }
                    RiscvExtension::D if !has_d => {
                        has_d = true;
                        found.push(RiscvExtension::D);
                    }
                    RiscvExtension::Q if !has_q => {
                        has_q = true;
                        found.push(RiscvExtension::Q);
                    }
                    RiscvExtension::Zicsr if !has_zicsr => {
                        has_zicsr = true;
                        found.push(RiscvExtension::Zicsr);
                    }
                    RiscvExtension::Zifencei if !has_zifencei => {
                        has_zifencei = true;
                        found.push(RiscvExtension::Zifencei);
                    }
                    RiscvExtension::Zawrs if !has_zawrs => {
                        has_zawrs = true;
                        found.push(RiscvExtension::Zawrs);
                    }
                    RiscvExtension::Zfh if !has_zfh => {
                        has_zfh = true;
                        found.push(RiscvExtension::Zfh);
                    }
                    RiscvExtension::Zba if !has_zba => {
                        has_zba = true;
                        found.push(RiscvExtension::Zba);
                    }
                    RiscvExtension::Zbb if !has_zbb => {
                        has_zbb = true;
                        found.push(RiscvExtension::Zbb);
                    }
                    RiscvExtension::Zbc if !has_zbc => {
                        has_zbc = true;
                        found.push(RiscvExtension::Zbc);
                    }
                    RiscvExtension::Zbkb if !has_zbkb => {
                        has_zbkb = true;
                        found.push(RiscvExtension::Zbkb);
                    }
                    RiscvExtension::Zbs if !has_zbs => {
                        has_zbs = true;
                        found.push(RiscvExtension::Zbs);
                    }
                    _ => {}
                }
            }

            i += len;
        }
    }

    found
}

/// Find which extensions are actually used in a compiled binary
pub fn find_used_extensions(binary_path: &std::path::Path) -> Result<Vec<RiscvExtension>, String> {
    let binary_data = std::fs::read(binary_path)
        .map_err(|e| format!("Failed to read binary {}: {}", binary_path.display(), e))?;

    Ok(find_extensions_in_elf(&binary_data))
}

/// Check ELF binary data for forbidden instructions
fn check_elf_instructions(
    data: &[u8],
    forbidden: &[RiscvExtension],
) -> Vec<(u64, u32, RiscvExtension)> {
    let mut violations = Vec::new();

    for section in executable_sections(data) {
        let mut i = section.offset as usize;
        let end = (i + section.size as usize).min(data.len());

        while i + 2 <= end && i + 2 <= data.len() {
            let (ext, len) = classify_instruction(&data[i..end]);

            if let Some(ext) = ext {
                if forbidden.contains(&ext) {
                    let instr = if len == 4 && i + 4 <= data.len() {
                        u32::from_le_bytes([data[i], data[i + 1], data[i + 2], data[i + 3]])
                    } else {
                        u32::from(u16::from_le_bytes([data[i], data[i + 1]]))
                    };
                    let addr = section.addr + (i as u64).saturating_sub(section.offset);
                    violations.push((addr, instr, ext));
                }
            }

            i += len;
        }
    }

    violations
}

/// Format violation messages for display
fn format_violations(violations: &[(u64, u32, RiscvExtension)], max_show: usize) -> String {
    let mut output = String::new();
    for (addr, instr, ext) in violations.iter().take(max_show) {
        output.push_str(&format!(
            "  0x{:08x}: 0x{:08x}  [{:?} extension]\n",
            addr, instr, ext
        ));
    }
    if violations.len() > max_show {
        output.push_str(&format!("  ... and {} more\n", violations.len() - max_show));
    }
    output
}

/// Check a compiled RISC-V binary for forbidden instruction extensions.
///
/// Returns Ok(()) if no forbidden instructions are found, or Err with a detailed message.
pub fn forbid_riscv_instructions_in_binary(
    binary_path: &std::path::Path,
    forbidden_extensions: &[RiscvExtension],
) -> Result<(), String> {
    let binary_data = std::fs::read(binary_path)
        .map_err(|e| format!("Failed to read binary {}: {}", binary_path.display(), e))?;

    let violations = check_elf_instructions(&binary_data, forbidden_extensions);

    if !violations.is_empty() {
        let extension_names: Vec<_> = forbidden_extensions
            .iter()
            .map(|ext| format!("{:?}", ext))
            .collect();

        return Err(format!(
            "\n\nFORBIDDEN RISC-V INSTRUCTIONS DETECTED\n\
            Binary: {}\n\
            Forbidden extensions: {}\n\
            Found {} violations\n\n\
            {}\n\
            This CPU does not support these instruction extensions.\n\
            Please review the code to avoid using these instructions.\n",
            binary_path.display(),
            extension_names.join(", "),
            violations.len(),
            format_violations(&violations, 5)
        ));
    }

    Ok(())
}

/// Check a compiled RISC-V binary for expected instruction extensions.
///
/// Returns Ok(()) if all expected instructions are found, or Err with a warning message.
pub fn require_riscv_instructions_in_binary(
    binary_path: &std::path::Path,
    expected_extensions: &[RiscvExtension],
) -> Result<(), String> {
    let binary_data = std::fs::read(binary_path)
        .map_err(|e| format!("Failed to read binary {}: {}", binary_path.display(), e))?;

    let found_extensions = find_extensions_in_elf(&binary_data);

    // Check which expected extensions are missing
    let missing: Vec<_> = expected_extensions
        .iter()
        .filter(|ext| !found_extensions.contains(ext))
        .collect();

    if !missing.is_empty() {
        let extension_names: Vec<_> = missing.iter().map(|ext| format!("{:?}", ext)).collect();

        return Err(format!(
            "\nEXPECTED RISC-V INSTRUCTIONS NOT FOUND\n\
            Binary: {}\n\
            Expected extensions: {}\n\
            Missing: {}\n\n\
            This CPU supports these extensions but they are not being used.\n\
            This may indicate missed optimization opportunities.",
            binary_path.display(),
            expected_extensions
                .iter()
                .map(|e| format!("{:?}", e))
                .collect::<Vec<_>>()
                .join(", "),
            extension_names.join(", ")
        ));
    }

    Ok(())
}
