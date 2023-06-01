// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Types and functions to read a program using a streaming format

/// Information about the available memory for loaded programs.
pub struct MemoryConfiguration {
    /// Range of valid instruction memory (writeable and executable)
    pub instruction_memory: core::ops::Range<u32>,
    /// Range of valid data memory
    pub data_memory: core::ops::Range<u32>,
}

/// Header of the program being sent.
pub struct ProgramHeader {
    /// Address of the entry point to the program.
    ///
    /// Should point into executable memory.
    pub entry: u32,
    /// Number of segments that this program consists of and will be sent.
    pub num_segments: u32,
}

impl ProgramHeader {
    /// Validate the program header in the context of a [`MemoryConfiguration`].
    pub fn is_valid(&self, conf: &MemoryConfiguration) -> bool {
        conf.instruction_memory.contains(&self.entry)
    }
}

/// Header of a segment being sent.
pub struct SegmentHeader {
    /// Flag whether the segment contains executable instructions or data.
    pub is_executable: bool,
    /// The address at which the segment should be loaded in memory.
    pub addr: u32,
    /// The length of the streaming data to be received.
    pub data_len: u32,
    /// The amount of zero-padding to add to the end of the segment data.
    pub zero_padding: u32,
}

impl SegmentHeader {
    /// Validate the segment header in the context of a [`MemoryConfiguration`].
    pub fn is_valid(&self, conf: &MemoryConfiguration) -> bool {
        let end = self.addr + self.data_len + self.zero_padding;

        if self.is_executable {
            conf.instruction_memory.contains(&self.addr) && conf.instruction_memory.contains(&end)
        } else {
            conf.data_memory.contains(&self.addr) && conf.instruction_memory.contains(&end)
        }
    }
}

/// Read the program header from a stream
///
/// Return `None` when there is no more data in the stream.
pub fn read_program_header(input: &mut impl Iterator<Item = u8>) -> Option<ProgramHeader> {
    let entry = read_u32_le(input)?;
    let num_segments = read_u32_le(input)?;

    Some(ProgramHeader {
        entry,
        num_segments,
    })
}

/// Read the next segment header from a stream
///
/// Return `None` when there is no more data in the stream.
pub fn read_segment_header(input: &mut impl Iterator<Item = u8>) -> Option<SegmentHeader> {
    let is_executable = input.next()? == 1;
    let addr = read_u32_le(input)?;
    let data_len = read_u32_le(input)?;
    let zero_padding = read_u32_le(input)?;

    Some(SegmentHeader {
        is_executable,
        addr,
        data_len,
        zero_padding,
    })
}

/// Reads the segment data from a stream and directly writes it to the appropriate
/// address.
///
/// Return `None` when there is no more data in the stream.
///
/// # Safety
///
/// The [`SegmentHeader`]`::addr` must be a valid memory chunk of at least
/// [`SegmentHeader`]`::data_len` bytes.
pub unsafe fn write_segment_data(
    seg: &SegmentHeader,
    input: &mut impl Iterator<Item = u8>,
) -> Option<()> {
    let addr = seg.addr as *mut u8;

    for offset in 0..seg.data_len {
        let b = input.next()?;

        addr.offset(offset as isize).write(b);
    }

    Some(())
}

/// Writes the zero padding of a segment in memory.
///
/// # Safety
///
/// The [`SegmentHeader`]`::addr` must be a valid memory chunk of at least
/// [`SegmentHeader`]`::data_len` + [`SegmentHeader`]`::zero_padding` bytes.
pub unsafe fn write_padding(seg: &SegmentHeader) {
    let start_addr = (seg.addr as *mut u8).offset(seg.data_len as isize);

    core::ptr::write_bytes(start_addr, 0, seg.zero_padding as usize);
}

fn read_u32_le(input: &mut impl Iterator<Item = u8>) -> Option<u32> {
    let bytes = [input.next()?, input.next()?, input.next()?, input.next()?];
    Some(u32::from_le_bytes(bytes))
}
