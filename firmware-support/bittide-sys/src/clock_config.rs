#[derive(Debug, Copy, Clone, PartialEq)]
enum FilePart {
    PreambleHeader,
    Preamble,
    PostPreambleDelay,
    Config,
    PostambleHeader,
    Postamble,
    Done,
}

/// Create a class that implements the statemachine for parsing the clock configuration file.
pub struct ClockConfigParser {
    part: FilePart,
}
impl ClockConfigParser {
    /// Create a new parser.
    pub fn new() -> Self {
        Self {
            part: FilePart::PreambleHeader,
        }
    }

    /// Parse a line of the clock configuration file.
    pub fn parse_line(&mut self, line: &str) -> Result<Option<(u16, u8)>, &'static str> {
        let (next_state, result) = parse_line(self.part, line);
        self.part = next_state;
        result
    }

    /// Check if the parser is done.
    pub fn is_done(&self) -> bool {
        self.part == FilePart::Done
    }
}
fn parse_line(part: FilePart, line: &str) -> (FilePart, Result<Option<(u16, u8)>, &'static str>) {
    let next_state = match (part, line) {
        (FilePart::PreambleHeader, _) if line.contains("Start configuration preamble") => {
            FilePart::Preamble
        }
        (FilePart::Preamble, _) if line.contains("End configuration preamble") => {
            FilePart::PostPreambleDelay
        }
        (FilePart::PostPreambleDelay, _) if line.contains("Start configuration registers") => {
            FilePart::Config
        }
        (FilePart::Config, _) if line.contains("End configuration registers") => FilePart::PostambleHeader,
        (FilePart::PostambleHeader, _) if line.contains("Start configuration postamble") => {
            FilePart::Postamble
        }
        (FilePart::Postamble, _) if line.contains("End configuration postamble") => {
            FilePart::Postamble
        }
        _ => part.clone(),
    };
    if next_state != part {
        return (next_state, Ok(None));
    }
    let result = match part {
        FilePart::Preamble | FilePart::Config | FilePart::Postamble => {
            parse_address_data(line).map(Some)
        }
        FilePart::Done => Err("State machine is already done."),
        _ => Ok(None),
    };
    (next_state, result)
}

fn parse_address_data(line: &str) -> Result<(u16, u8), &'static str> {
    let mut parts = line.split(',');
    let address = parts.next().ok_or("Missing address")?;
    let data = parts.next().ok_or("Missing data")?;
    let address = parse_hex(address)?.try_into().map_err(|_| "Address out of range")?;
    let data = parse_hex(data)?.try_into().map_err(|_| "Data out of range")?;
    Ok((address, data))
}

fn parse_hex(hex_string: &str) -> Result<u32, &'static str> {
    if hex_string.starts_with("0x") {
        u32::from_str_radix(&hex_string[2..], 16).map_err(|_| "Invalid hex number")
    } else {
        Err("Hex number must start with '0x'")
    }
}
