#![no_std]
#![no_main]

use core::fmt::Write;
#[cfg(not(test))]
use riscv_rt::entry;

mod axi_buffers;
use axi_buffers::{AxiRxBuffer, AxiTxBuffer};

use bittide_sys::uart::Uart;
use bittide_sys::panic_handler::set_panic_handler_uart;

const UART_ADDR:usize = 0x4000_0000;

#[cfg_attr(not(test), entry)]

fn main() -> ! {
    let mut uart = unsafe { Uart::new(UART_ADDR as *mut u8) };
    let panic_uart = unsafe { Uart::new(UART_ADDR as *mut u8) };
    unsafe{ set_panic_handler_uart(panic_uart)};

    _ = writeln!(uart, "Testing AxiTxBuffer and AxiRxBuffer...");

    // Initialize the AxiRxBuffer and AxiTxBuffer structs
    let tx_fifo_depth = 16;
    let rx_fifo_depth = 64 * 4;
    let axi_tx_buffer = AxiTxBuffer::new(0x6000_0000 as *mut u8, tx_fifo_depth);
    let axi_rx_buffer: AxiRxBuffer = AxiRxBuffer::new(0x8000_0000 as *mut u8, rx_fifo_depth);

    writeln!(uart, "AxiTxBuffer: {:#?}", axi_tx_buffer).unwrap();
    writeln!(uart, "AxiRxBuffer: {:#?}", axi_rx_buffer).unwrap();

    // Send a packet using the AxiTxBuffer
    let packet_data = b"Hello world!?";
    writeln!(uart, "RX status: {}", axi_rx_buffer.read_status()).unwrap();
    writeln!(uart, "Sending packet").unwrap();
    axi_tx_buffer.write_packet(packet_data);
    writeln!(uart, "Done").unwrap();

    writeln!(uart, "Receiving packet").unwrap();
    // Poll the AxiRxBuffer status register to see if a packet has been received
    while !axi_rx_buffer.is_packet_available()
        {
        write!(uart, ".").unwrap();
        };
    writeln!(uart, "RX status: {}", axi_rx_buffer.read_status()).unwrap();
    let packet_length = axi_rx_buffer.packet_length();
    writeln!(uart, "Packet available: {} bytes.",packet_length).unwrap();

    // Read the packet data from the AxiRxBuffer
    let mut received_packet = [0u8; 65 * 4];
    // axi_rx_buffer.read_packet(&mut received_packet);
    let packet = axi_rx_buffer.read_packet(&mut received_packet);
    writeln!(uart, "Captured packet.").unwrap();
    writeln!(uart, "RX status: {}", axi_rx_buffer.read_status()).unwrap();

    match packet {
        Some(received_bytes) =>
            if *packet_data == received_packet[..received_bytes as usize] {
                writeln!(uart, "Packet received successfully!").unwrap()
            } else {
                writeln!(uart, "Error: Received packet does not match sent packet!").unwrap();
                writeln!(uart, "Expected").unwrap();
                for b in packet_data.iter() {
                    uart.send(*b);
                }
                writeln!(uart, "").unwrap();
                writeln!(uart, "Received:").unwrap();
                for b in received_packet[..received_bytes].iter() {
                    uart.send(*b);
                    writeln!(uart, "{}", *b as u8).unwrap();
                }
            },
        None =>
            writeln!(uart, "Error: Packet not received!").unwrap(),
    }
    // Check if the received packet matches the sent packet
    writeln!(uart, "End of program!").unwrap();
    loop {}
}
