// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use clap::{Args, Parser};
// use nusb::DeviceInfo;
// use parking_lot::FairMutex;
use probe_rs::{
    // config::{Core, ScanChainElement, TargetSelector},
    config::TargetSelector,
    probe::{
        // ftdi::{FtdiProbe, FtdiProbeFactory},
        ftdi::FtdiProbeFactory,
        // DebugProbe, DebugProbeError, DebugProbeInfo, DebugProbeKind, DebugProbeSelector,
        DebugProbeInfo,
        DebugProbeKind,
        ProbeFactory,
        UsbFilters,
        WireProtocol,
    },
    // CoreType, MemoryInterface, Permissions,
    MemoryInterface,
    Permissions,
};
// use probe_rs_target::{CoreAccessOptions, RawFlashAlgorithm, RiscvCoreAccessOptions};
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    ffi::OsString,
    fmt::{self, Debug, Display},
    fs::read_to_string,
    io::{Error as IoError, ErrorKind as IoErrorKind, Result as IoResult},
    // net::{Ipv4Addr, SocketAddr, SocketAddrV4},
    num::ParseIntError,
    os::unix::ffi::OsStrExt,
    path::PathBuf,
    time::Duration,
};

#[derive(Parser)]
struct Clap {
    #[arg(short, long, value_parser = parse_addr)]
    scan_address: u32,
    #[arg(short, long)]
    usb_port: OsString,
    #[command(flatten)]
    device_json: DeviceJson,
}

#[derive(Args)]
#[group(required = true, multiple = false)]
struct DeviceJson {
    #[arg(short = 'p', long)]
    json_path: Option<PathBuf>,
    #[arg(short, long)]
    json: Option<String>,
}

fn parse_addr(addr: &str) -> Result<u32, ParseIntError> {
    const BIN_PREFIXES: [&str; 4] = ["0b", "0B", "b", "B"];
    const OCT_PREFIXES: [&str; 4] = ["0o", "0O", "o", "O"];
    const HEX_PREFIXES: [&str; 4] = ["0x", "0X", "x", "X"];
    if let Some(pfx) = BIN_PREFIXES.into_iter().find(|&pfx| addr.starts_with(pfx)) {
        u32::from_str_radix(&addr[pfx.len()..], 2)
    } else if let Some(pfx) = OCT_PREFIXES.into_iter().find(|&pfx| addr.starts_with(pfx)) {
        u32::from_str_radix(&addr[pfx.len()..], 8)
    } else if let Some(pfx) = HEX_PREFIXES.into_iter().find(|&pfx| addr.starts_with(pfx)) {
        u32::from_str_radix(&addr[pfx.len()..], 16)
    } else {
        addr.parse()
    }
}

#[derive(Deserialize, Serialize)]
struct Cpu {
    whoami: WhoAmID,
    gdb_port: u16,
}

#[derive(Deserialize, Serialize)]
enum WhoAmID {
    String(String),
    Literal(u32),
}

impl WhoAmID {
    fn is_valid(&self) -> bool {
        match self {
            WhoAmID::String(s) => s.len() == 4 && s.is_ascii(),
            WhoAmID::Literal(_) => true,
        }
    }

    fn raw(&self) -> u32 {
        match self {
            WhoAmID::String(s) => {
                let mut bytes = [0; 4];
                for (i, c) in s.chars().enumerate() {
                    bytes[i] = c as u8;
                }
                u32::from_be_bytes(bytes)
            }
            WhoAmID::Literal(num) => *num,
        }
    }
}

impl Display for WhoAmID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WhoAmID::String(s) => write!(f, "'{s}'"),
            WhoAmID::Literal(num) => {
                let [a, b, c, d] = num.to_be_bytes().map(|b| b as char);
                write!(f, "'{a}{b}{c}{d}' (0x{num:08x})")
            }
        }
    }
}

macro_rules! other_io_error {
    ($litstr:literal) => {
        IoError::new(IoErrorKind::Other, $litstr)
    };
    (f$litstr:literal) => {
        IoError::new(IoErrorKind::Other, format!($litstr))
    };
    (format!($($other:tt)*)$(,)?) => {
        IoError::new(IoErrorKind::Other, format!($($other)*))
    };
    ($($other:tt)*) => {
        IoError::new(IoErrorKind::Other, format!($($other)*))
    };
}

trait IntoIoResult<T> {
    fn into_io_result(self, prefix: impl AsRef<str>) -> IoResult<T>;
}

impl<T, E: Debug + Display> IntoIoResult<T> for Result<T, E> {
    fn into_io_result(self, prefix: impl AsRef<str>) -> IoResult<T> {
        self.map_err(|err| other_io_error!("{}\nShort: {err}\nLong: {err:?}", prefix.as_ref()))
    }
}

fn init_logging() {
    use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, Layer};
    let stdout_subscriber = tracing_subscriber::fmt::layer()
        .compact()
        .without_time()
        .with_filter(
            // No default, use RUST_LOG or fall back to WARN.
            tracing_subscriber::EnvFilter::builder()
                .with_default_directive(tracing_subscriber::filter::LevelFilter::WARN.into())
                .from_env_lossy(),
        );

    tracing_subscriber::registry()
        .with(stdout_subscriber)
        .init();
}

// #[derive(Clone)]
// struct SpecificProbeFactory {
//     device: DeviceInfo,
// }

// impl ProbeFactory for SpecificProbeFactory {
//     fn list_probes(&self) -> Vec<DebugProbeInfo> {
//         let tmp: Box<dyn ProbeFactory + 'static> = Box::new(self.clone());
//         let tmp: &'static (dyn ProbeFactory + 'static) = Box::leak(tmp);
//         vec![DebugProbeInfo::new(
//             self.device.product_string().unwrap_or("FTDI").to_string(),
//             self.device.vendor_id(),
//             self.device.product_id(),
//             self.device.serial_number().map(Into::into),
//             tmp,
//             None,
//         )]
//     }
//
//     fn open(&self, _: &DebugProbeSelector) -> Result<Box<dyn DebugProbe>, DebugProbeError> {
//         let probe = FtdiProbe::from(value)
//     }
// }

fn main() -> IoResult<()> {
    // env_logger::Builder::from_default_env()
    //     .filter(None, log::LevelFilter::Trace)
    //     .target(env_logger::Target::Stdout)
    //     .init();
    init_logging();
    let _ = FtdiProbeFactory.list_probes();
    let Clap {
        scan_address,
        usb_port,
        device_json: DeviceJson { json_path, json },
    } = Clap::parse();
    println!("Successfully parsed command line arguments.");
    let json_string = match (json_path, json) {
        (Some(path), _) => read_to_string(path)?,
        (_, Some(string)) => string,
        _ => unreachable!(),
    };
    println!("Successfully parsed CPU map JSON");
    let cpus: Vec<Cpu> = serde_json::from_str(&json_string)?;
    if cpus.is_empty() {
        return Err(other_io_error!("CPUs map is empty!"));
    }
    let mut map = HashMap::with_capacity(cpus.len());
    let mut ports = HashSet::with_capacity(cpus.len());
    for cpu in cpus {
        let Cpu { whoami, gdb_port } = cpu;
        if !whoami.is_valid() {
            return Err(other_io_error!(
                "\
                Config contains invalid whoami ID: {whoami}. \
                Identifiers must be 4 ASCII characters long.\
                "
            ));
        }
        let whoami = whoami.raw();
        if map.contains_key(&whoami) {
            return Err(other_io_error!(
                "Config contains duplicate whoami ID: {whoami}"
            ));
        }
        if ports.contains(&gdb_port) {
            return Err(other_io_error!(
                "Config contains duplicate GDB port {gdb_port} for CPU {whoami}"
            ));
        }
        map.insert(whoami, gdb_port);
        ports.insert(gdb_port);
    }

    let Some(device) = nusb::list_devices()?.find(|device| {
        let syspath = device.sysfs_path();
        let last = syspath.file_name().unwrap();
        let upbytes = usb_port.as_bytes();
        let lbytes = last.as_bytes();
        upbytes.len() > lbytes.len()
            && &upbytes[0..lbytes.len()] == lbytes
            && upbytes[lbytes.len()] == b':'
    }) else {
        return Err(other_io_error!(
            "Could not find device ID matching request: {}",
            usb_port.display(),
        ));
    };
    let dbg_probe = DebugProbeInfo::new(
        device.product_string().unwrap_or("FTDI").to_string(),
        DebugProbeKind::Usb {
            vendor_id: device.vendor_id(),
            product_id: device.product_id(),
            filters: UsbFilters::default().with_sysfs_path(device.sysfs_path()),
        },
        &FtdiProbeFactory {
            // init_override: None,
            // dir_override: None,
            // init_override: Some(0x0028),
            // dir_override: Some(0x002b),
        },
    );
    // let mut probe = dbg_probe
    //     .open()
    //     .into_io_result("Failed to open debug probe to get chain length.")?;
    // if !probe.has_riscv_interface() {
    //     return Err(other_io_error!("Device has no RISC-V interface!"));
    // }
    // println!("Probe has RISC-V interface!");
    // match probe.try_as_jtag_probe() {
    //     Some(jtag) => {
    //         println!("Probe has JTAG interface!");
    //         jtag.set_idle_cycles(7)
    //             .into_io_result("Failed to set idle cycles for chain scan!")?;
    //         jtag.set_speed(1)
    //             .into_io_result("Failed to set speed for chain scan!")?;
    //         let chain_len = jtag
    //             .scan_chain()
    //             .into_io_result("Failed to scan JTAG chain!")?
    //             .len();
    //         if chain_len != map.len() {
    //             return Err(other_io_error!(
    //                 "CPU map length does not match chain length! Found {chain_len}, expected {}",
    //                 map.len()
    //             ));
    //         }
    //         println!("Probe has JTAG chain with appropriate number of elements! ({chain_len})");
    //     }
    //     None => return Err(other_io_error!("Cannot open JTAG interface for device!")),
    // }
    // drop(probe);
    println!("Successfully found debug probe!");
    println!("Probe info: {dbg_probe:?}");
    println!("Probe info neat: {dbg_probe}");
    let mut sessions = Vec::new();
    for i in 0..map.len() {
        let mut probe = dbg_probe
            .open()
            .into_io_result(format!("Failed to open debug probe {i}!"))?;
        probe
            .select_protocol(WireProtocol::Jtag)
            .into_io_result(format!("Failed to select JTAG for probe {i}"))?;
        println!("Successfully set probe protocol as JTAG.");
        let jtag_probe = probe.try_as_jtag_probe().unwrap();
        jtag_probe
            .set_idle_cycles(7)
            .into_io_result("Failed to set JTAG idle cycles.")?;
        jtag_probe
            .set_speed(1)
            .into_io_result("Failed to set JTAG speed.")?;
        let chain = jtag_probe
            .scan_chain()
            .into_io_result("Failed to scan JTAG chain!")?;
        println!("Got chain: {chain:?}");
        jtag_probe
            .select_target(i)
            .into_io_result(format!("Failed to set probe {i}'s JTAG tap."))?;
        println!("Successfuly set target to index {i}");
        let session = probe
            .attach(
                TargetSelector::Unspecified("riscv".to_string()),
                Permissions::default(),
            )
            .into_io_result(format!("Failed to attach session for probe {i}!"))?;
        println!("Successfully attached session for probe {i}!");
        sessions.push(session);
    }
    std::thread::scope(|_scope| {
        // let mut threads = Vec::with_capacity(sessions.len());
        for (i, mut session) in sessions.into_iter().enumerate() {
            println!("Begin handling session: {i}");
            println!("Available CPUs:");
            for (id, core) in session.list_cores() {
                println!("  - ID: {id}, core: {core:?}");
            }
            println!("Assuming session should use CPU 0. Attaching...");
            let mut cpu = session
                .core(0)
                .into_io_result(format!("Failed to attach to CPU 0 on session {i}"))?;
            let whoami = cpu
                .read_word_32(scan_address as u64)
                .into_io_result("Failed to read WhoAmI address (0xE000_0000)!")?;
            let Some(&gdb_port) = map.get(&whoami) else {
                return Err(other_io_error!(
                    "Encountered unknown WhoAmID: {}!",
                    WhoAmID::Literal(whoami)
                ));
            };
            println!(
                "Mapped whoamID {} to GDB port {gdb_port}",
                WhoAmID::Literal(whoami)
            );
            // let config = [GdbInstanceConfiguration {
            //     core_type: cpu.core_type(),
            //     cores: vec![0],
            //     socket_addrs: vec![SocketAddr::V4(SocketAddrV4::new(
            //         Ipv4Addr::LOCALHOST,
            //         gdb_port,
            //     ))],
            // }];
            cpu.halt(Duration::from_secs(5)).into_io_result(format!(
                "Failed to halt CPU for WhoAmID: {}",
                WhoAmID::Literal(whoami)
            ))?;
            drop(cpu);
            // let session_mutex = FairMutex::new(session);
            // let handle =
            //     scope.spawn(move || probe_rs::gdb_server::run(&session_mutex, config.iter()));
            // threads.push(handle);
        }
        eprintln!("All sessions halted");
        // while threads.iter().any(|thread| !thread.is_finished()) {}
        // for (i, thread) in threads.into_iter().enumerate() {
        //     match thread.join() {
        //         Ok(result) => result.into_io_result(format!("Thread {i} returned an error!"))?,
        //         Err(err) => {
        //             return Err(other_io_error!(
        //                 f "Thread {i} panicked!\nLong: {err:?}"
        //             ));
        //         }
        //     }
        // }
        Ok(())
    })
}
