// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::{
    path::{Path, PathBuf},
    process::Command,
};

use proptest::prelude::*;
use test_strategy::proptest;

use bittide_sys::program_stream::*;

mod elf_common;
use elf_common::*;

fn find_cabal_root() -> Option<PathBuf> {
    let current_path = std::env::current_dir().ok()?;

    fn find_root(path: PathBuf) -> Option<PathBuf> {
        let manifest = path.join("cabal.project");
        if manifest.exists() {
            return Some(path);
        }
        let parent = path.parent()?;

        find_root(parent.to_owned())
    }

    find_root(current_path)
}

fn build_program_stream_exec(root_path: &Path) -> bool {
    let status = Command::new("cabal")
        .args(["build", "program-stream"])
        .current_dir(root_path)
        .status();

    let Ok(status) = status else { return false };

    status.success()
}

fn program_stream_exec_path() -> Option<PathBuf> {
    let root = find_cabal_root()?;
    let compile_success = build_program_stream_exec(&root);

    if !compile_success {
        eprintln!("building of program-stream executable not successful!!");
        return None;
    }

    let output = Command::new("cabal")
        .arg("list-bin")
        .arg("program-stream")
        .current_dir(&root)
        .output()
        .ok()?;

    if !output.status.success() {
        eprintln!("list-bin failed");
        return None;
    }

    let path_str = String::from_utf8(output.stdout).ok()?;

    Some(PathBuf::from(path_str.trim_end()))
}

fn stream_for_elf(elf: &Path) -> Option<Vec<u8>> {
    let output = Command::new(&*PROG_STREAM_EXEC).arg(elf).output().ok()?;

    if !output.status.success() {
        eprintln!("ERROR");
        return None;
    }

    Some(output.stdout)
}

lazy_static::lazy_static! {
    static ref PROG_STREAM_EXEC: PathBuf = program_stream_exec_path().unwrap();
}

#[test]
fn find_cabal_stuff() {
    dbg!(&*PROG_STREAM_EXEC);
}

unsafe fn write_program_to_memory(input: &mut impl Iterator<Item = u8>) -> Option<()> {
    let prog_hd = read_program_header(input)?;

    for _ in 0..prog_hd.num_segments {
        let seg_hd = read_segment_header(input)?;
        write_segment_data(&seg_hd, input)?;
        write_padding(&seg_hd);
    }

    Some(())
}

unsafe fn verify_program_contents(info: &ElfCreateInfo) {
    for seg in &info.segments {
        let addr = seg.addr as usize as *mut u8;

        let data_slice =
            std::slice::from_raw_parts(addr, seg.data.len() + seg.zero_padding as usize);

        assert_eq!(data_slice[0..seg.data.len()], seg.data);
        assert!(data_slice[seg.data.len()..].iter().all(|x| *x == 0));
    }
}

// Generate valid ELF files and convert them into a streaming format.
#[proptest(ProptestConfig { cases: 5000, max_shrink_iters: 1000, ..ProptestConfig::default() })]
fn all_elfs_can_be_converted_to_streaming(#[strategy(gen_valid_elf_file())] info: ElfCreateInfo) {
    use std::io::Write;
    let mut file = tempfile::NamedTempFile::new().unwrap();
    let elf = create_elf_file(&info);
    file.write(&elf).unwrap();

    let _ = stream_for_elf(file.path()).expect("creating program-stream works");
}

// Generate valid ELF files and make sure that all streams are smaller than the ELF.
#[proptest(ProptestConfig { cases: 5000, max_shrink_iters: 1000, ..ProptestConfig::default() })]
fn all_streams_are_smaller_than_elfs(#[strategy(gen_valid_elf_file())] info: ElfCreateInfo) {
    use std::io::Write;
    let mut file = tempfile::NamedTempFile::new().unwrap();
    let elf = create_elf_file(&info);
    file.write(&elf).unwrap();

    let stream = stream_for_elf(file.path()).expect("creating program-stream works");

    assert!(elf.len() >= stream.len());
}

// Generate valid ELF files, convert them to a streaming format and make sure
// the segment data is loaded into the buffer properly.
#[proptest(ProptestConfig { cases: 5000, max_shrink_iters: 1000, ..ProptestConfig::default() })]
fn all_elfs_are_loaded_properly(#[strategy(gen_valid_elf_file())] mut info: ElfCreateInfo) {
    use std::io::Write;

    let buffer_size = elf_loaded_buffer_size(&info);

    unsafe {
        with_32bit_addr_buffer(u32::try_from(buffer_size).unwrap(), |buf| {
            let base_addr = buf.as_mut_ptr() as usize as u64;

            elf_info_set_base_addr(&mut info, base_addr);

            let mut file = tempfile::NamedTempFile::new().unwrap();
            let elf = create_elf_file(&info);
            file.write(&elf).unwrap();

            let stream = stream_for_elf(file.path()).expect("creating program-stream works");

            write_program_to_memory(&mut stream.into_iter()).unwrap();

            verify_program_contents(&info);
        });
    }
}
