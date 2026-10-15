// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
//! IPMI FRU image for the Trenz TEF0008 FMC card, as required by ANSI/VITA 57.1.
//!
//! The KCU105 system controller reads this EEPROM at power-on, and picks a VADJ
//! voltage that the card supports (UG917, "FMC VADJ_1V8 Power Rail"). The card
//! states which voltages it supports in the DC load record of VADJ. The other
//! records are there for completeness, as required by VITA 57.1.
//!
//! Formats are defined by the "IPMI Platform Management FRU Information Storage
//! Definition" v1.0 (sections 8, 11, 16 and 18) and ANSI/VITA 57.1 (section
//! 5.5). Multi-byte numbers are little endian.
//!
//! Card facts are taken from the TEF0008 technical reference manual and the
//! REV02 schematic:
//! https://wiki.trenz-electronic.de/display/PD/TEF0008+TRM

/// FMC power rail numbers, as defined by VITA 57.1
const P1_VADJ: u8 = 0;
const P1_3P3V: u8 = 1;
const P1_12P0V: u8 = 2;
const P1_VIO_B_M2C: u8 = 3;
const P1_VREF_A_M2C: u8 = 4;
const P1_VREF_B_M2C: u8 = 5;

/// Voltage the card needs on a carrier supplied rail. Voltages in units of 10 mV,
/// currents in mA.
struct DcLoad {
    output: u8,
    nominal: u16,
    min: u16,
    max: u16,
    /// Maximum ripple and noise, peak to peak, in mV
    ripple: u16,
    min_current: u16,
    max_current: u16,
}

/// Rail the card supplies to the carrier. Voltages in units of 10 mV, currents
/// in mA.
struct DcOutput {
    output: u8,
    nominal: u16,
    max_negative_deviation: u16,
    max_positive_deviation: u16,
    /// Ripple and noise, peak to peak, in mV
    ripple: u16,
    min_current: u16,
    max_current: u16,
}

fn dc_loads() -> [DcLoad; 3] {
    [
        // VADJ only powers the I/O bank of the card's MAX10 that connects to
        // the LA pins, which we use at 1.8 V. The KCU105 supports 1.2, 1.5 and
        // 1.8 V, so only allow 1.8 V (+/- 5%). Trenz does not specify the
        // current, so use the maximum VITA 57.1 allows on VADJ.
        DcLoad {
            output: P1_VADJ,
            nominal: 180,
            min: 171,
            max: 189,
            // Not specified by Trenz. Set permissively, so carriers that check
            // it do not reject the card.
            ripple: 100,
            min_current: 0,
            max_current: 4000,
        },
        // Main supply of the card, 3.3 V +/- 5%. Trenz does not specify the
        // current, so use the maximum VITA 57.1 allows on 3P3V.
        DcLoad {
            output: P1_3P3V,
            nominal: 330,
            min: 314,
            max: 346,
            ripple: 100,
            min_current: 0,
            max_current: 3000,
        },
        // Not used by the card, so it draws no current.
        DcLoad {
            output: P1_12P0V,
            nominal: 1200,
            min: 1140,
            max: 1260,
            ripple: 100,
            min_current: 0,
            max_current: 0,
        },
    ]
}

/// The card does not supply VIO_B_M2C, VREF_A_M2C or VREF_B_M2C.
fn dc_outputs() -> [DcOutput; 3] {
    [P1_VIO_B_M2C, P1_VREF_A_M2C, P1_VREF_B_M2C].map(|output| DcOutput {
        output,
        nominal: 0,
        max_negative_deviation: 0,
        max_positive_deviation: 0,
        ripple: 0,
        min_current: 0,
        max_current: 0,
    })
}

/// VITA 57.1 "FMC main definition" OEM record payload.
fn fmc_main_definition() -> Vec<u8> {
    // Single width module, HPC P1 connector, no P2 connector. The bidirectional
    // clocks are driven by the carrier (carrier to mezzanine): the card uses
    // CLK2_BIDIR as an input of its Si5345, and pulls CLK_DIR high.
    let module_size: u8 = 0b00;
    let p1_connector_size: u8 = 0b01;
    let p2_connector_size: u8 = 0b11;
    let clock_direction: u8 = 0b1;
    vec![
        // VITA's IANA private enterprise number 0x0012A2
        0xA2,
        0x12,
        0x00,
        // Subtype: FMC main definition, version 0
        0x00,
        (module_size << 6)
            | (p1_connector_size << 4)
            | (p2_connector_size << 2)
            | (clock_direction << 1),
        // P1 bank A signals: LA00 to LA09, P and N
        20,
        // P1 bank B signals: none
        0,
        // P2 bank A and B signals: no P2 connector
        0,
        0,
        // Transceivers on P1 (upper nibble) and P2 (lower nibble): one per SFP+
        // cage
        4 << 4,
        // Maximum TCK frequency in MHz for the JTAG chain of the card (MAX10).
        // Not specified by Trenz, so pick a conservative value.
        10,
    ]
}

/// Checksum such that all bytes, including the checksum, sum to zero.
fn zero_checksum(bytes: &[u8]) -> u8 {
    bytes
        .iter()
        .fold(0u8, |acc, b| acc.wrapping_add(*b))
        .wrapping_neg()
}

/// Type/length byte followed by an 8-bit ASCII string
fn ascii_field(s: &str) -> Vec<u8> {
    assert!(s.is_ascii() && s.len() < 64);
    let mut field = vec![0b1100_0000 | s.len() as u8];
    field.extend_from_slice(s.as_bytes());
    field
}

fn board_info_area() -> Vec<u8> {
    let format_version = 0x01;
    // Area length in multiples of 8 bytes, filled in below
    let length = 0x00;
    let language_english = 0x00;
    // Minutes since 1996-01-01, zero means unspecified
    let manufacturing_date = [0x00, 0x00, 0x00];
    let mut area = vec![format_version, length, language_english];
    area.extend(manufacturing_date);
    area.extend(ascii_field("Trenz Electronic"));
    area.extend(ascii_field("TEF0008"));
    // Serial number
    area.extend(ascii_field(""));
    // Part number
    area.extend(ascii_field("TEF0008"));
    // FRU file ID
    area.extend(ascii_field("bittide"));
    // End of fields
    area.push(0xC1);
    // Pad, leaving room for the checksum
    while (area.len() + 1) % 8 != 0 {
        area.push(0x00);
    }
    area[1] = ((area.len() + 1) / 8) as u8;
    area.push(zero_checksum(&area));
    area
}

/// A multirecord with its header
fn multirecord(record_type: u8, payload: &[u8], last: bool) -> Vec<u8> {
    let format_version = 0x02;
    let end_of_list = if last { 0x80 } else { 0x00 };
    let mut header = vec![
        record_type,
        end_of_list | format_version,
        payload.len() as u8,
        zero_checksum(payload),
    ];
    header.push(zero_checksum(&header));
    header.extend_from_slice(payload);
    header
}

fn multirecord_area() -> Vec<u8> {
    let mut records: Vec<(u8, Vec<u8>)> = vec![];
    for load in dc_loads() {
        let mut payload = vec![load.output];
        for v in [
            load.nominal,
            load.min,
            load.max,
            load.ripple,
            load.min_current,
            load.max_current,
        ] {
            payload.extend(v.to_le_bytes());
        }
        records.push((0x02, payload));
    }
    for output in dc_outputs() {
        let mut payload = vec![output.output];
        for v in [
            output.nominal,
            output.max_negative_deviation,
            output.max_positive_deviation,
            output.ripple,
            output.min_current,
            output.max_current,
        ] {
            payload.extend(v.to_le_bytes());
        }
        records.push((0x01, payload));
    }
    // OEM record
    records.push((0xFA, fmc_main_definition()));

    let n = records.len();
    records
        .iter()
        .enumerate()
        .flat_map(|(i, (record_type, payload))| multirecord(*record_type, payload, i == n - 1))
        .collect()
}

/// The complete FRU image, starting at address 0 of the EEPROM.
pub fn fru_image() -> Vec<u8> {
    let board = board_info_area();
    let multi = multirecord_area();

    // Offsets in multiples of 8 bytes. The common header is 8 bytes.
    let board_offset = 1;
    let multi_offset = board_offset + board.len() / 8;
    let mut header = vec![
        // Format version
        0x01,
        // Internal use, chassis info: not present
        0x00,
        0x00,
        board_offset as u8,
        // Product info: not present
        0x00,
        multi_offset as u8,
        // Pad
        0x00,
    ];
    header.push(zero_checksum(&header));

    let mut image = header;
    image.extend(board);
    image.extend(multi);
    image
}
