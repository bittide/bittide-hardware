// SPDX-FileCopyrightText: 2023-2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::derive::uDebug;
use ufmt::uDebug;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ParserState {
    PreambleHeader,
    Preamble,
    PostPreambleDelay,
    Config,
    PostambleHeader,
    Postamble,
    Done,
}

#[derive(uDebug)]
pub struct ConfigEntry {
    pub page: u8,
    pub addr: u8,
    pub data: u8,
}

#[derive(uDebug)]
pub enum ClockConfigParseError {
    AddressMissing,
    DataMissing,
    AddressOutOfRange,
    DataOutOfRange,
    EndOfConfig,
    InvalidHexNumber,
}

/// Create a class that implements the statemachine for parsing the clock configuration file.
pub struct ClockConfigParser {
    pub state: ParserState,
}

impl ClockConfigParser {
    /// Create a new parser.
    pub fn new() -> Self {
        Self {
            state: ParserState::PreambleHeader,
        }
    }

    /// Parse a line of the clock configuration file.
    pub fn parse_line(&mut self, line: &str) -> Result<Option<ConfigEntry>, ClockConfigParseError> {
        let (next_state, result) = parse_line(self.state, line);
        self.state = next_state;
        result
    }

    /// Check if the parser is done.
    pub fn is_done(&self) -> bool {
        self.state == ParserState::Done
    }
}

impl Default for ClockConfigParser {
    fn default() -> Self {
        Self::new()
    }
}

fn parse_line(
    state: ParserState,
    line: &str,
) -> (
    ParserState,
    Result<Option<ConfigEntry>, ClockConfigParseError>,
) {
    let next_state = match (state, line) {
        (ParserState::PreambleHeader, _) if line.contains("Start configuration preamble") => {
            ParserState::Preamble
        }
        (ParserState::Preamble, _) if line.contains("End configuration preamble") => {
            ParserState::PostPreambleDelay
        }
        (ParserState::PostPreambleDelay, _) if line.contains("Start configuration registers") => {
            ParserState::Config
        }
        (ParserState::Config, _) if line.contains("End configuration registers") => {
            ParserState::PostambleHeader
        }
        (ParserState::PostambleHeader, _) if line.contains("Start configuration postamble") => {
            ParserState::Postamble
        }
        (ParserState::Postamble, _) if line.contains("End configuration postamble") => {
            ParserState::Done
        }
        _ => state,
    };
    if next_state != state {
        return (next_state, Ok(None));
    }
    let result = match state {
        ParserState::Preamble | ParserState::Config | ParserState::Postamble => {
            parse_address_data(line).map(Some)
        }
        ParserState::Done => Err(ClockConfigParseError::EndOfConfig),
        _ => Ok(None),
    };
    (next_state, result)
}

fn parse_address_data(line: &str) -> Result<ConfigEntry, ClockConfigParseError> {
    let mut parts = line.split(',');

    let address_str = parts.next().ok_or(ClockConfigParseError::AddressMissing)?;
    let full_address: u16 = parse_hex(address_str)?
        .try_into()
        .map_err(|_| ClockConfigParseError::AddressOutOfRange)?;
    let page = (full_address >> 8) as u8;
    let addr = (full_address & 0xff) as u8;

    let data_str = parts.next().ok_or(ClockConfigParseError::DataMissing)?;
    let data: u8 = parse_hex(data_str)?
        .try_into()
        .map_err(|_| ClockConfigParseError::DataOutOfRange)?;

    let config_entry = ConfigEntry { page, addr, data };
    Ok(config_entry)
}

fn parse_hex(hex_string: &str) -> Result<u32, ClockConfigParseError> {
    if let Some(hex_stripped) = hex_string.strip_prefix("0x") {
        u32::from_str_radix(hex_stripped, 16).map_err(|_| ClockConfigParseError::InvalidHexNumber)
    } else {
        Err(ClockConfigParseError::InvalidHexNumber)
    }
}

impl ufmt::uDisplay for ClockConfigParseError {
    fn fmt<W>(&self, f: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        <ClockConfigParseError as uDebug>::fmt(self, f)
    }
}
