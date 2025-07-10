// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::axi::{AxiRx, AxiTx};
use heapless::String;
use rand::rngs::SmallRng;
use rand::{RngCore, SeedableRng};
use ufmt::uwrite;

use bittide_hal::shared::devices::uart::Uart;
use ufmt::uwriteln;
const STRING_SIZE: usize = 1024;

/// Tests for the axi module.
#[allow(dead_code)]
pub fn self_test<const BUF_SIZE: usize>(mut uart: Uart, tx: AxiTx, rx: AxiRx<BUF_SIZE>) {
    type TestFn<const BUF_SIZE: usize> =
        fn(AxiTx, AxiRx<BUF_SIZE>, &mut String<STRING_SIZE>) -> bool;

    // Construct a list of tests with their names.
    let tests: &[(TestFn<BUF_SIZE>, &str)] = &[
        (read_rx_status, "read_rx_status"),
        (clear_rx_status, "clear_rx_status"),
        (clear_rx_packet_register, "clear_rx_packet_register"),
        (send_empty_packet, "send_empty_packet"),
        (send_static_packet, "send_static_packet"),
        (send_receive_empty_packet, "send_receive_empty_packet"),
        (read_rx_packet_length, "read_rx_packet_length"),
        (send_receive_random_packet, "send_receive_random_packet"),
        (
            send_multiple_packets_receive_all,
            "send_multiple_packets_receive_all",
        ),
    ];
    let mut str: String<STRING_SIZE> = String::new();

    uwriteln!(uart, "Start axi self test").unwrap();
    for (test, name) in tests {
        str.clear();
        let result = test(tx, rx, &mut str);
        if result {
            uwriteln!(uart, "{}: Some({})", name, str.as_str()).unwrap();
        } else {
            uwriteln!(uart, "{}: None", name).unwrap();
        }
    }
    uwriteln!(uart, "Done").unwrap();
    loop {
        continue;
    }
}

fn send_empty_packet<const BUF_SIZE: usize>(
    mut tx: AxiTx,
    rx: AxiRx<BUF_SIZE>,
    _str: &mut String<STRING_SIZE>,
) -> bool {
    let packet = [];
    tx.send(&packet);
    while !rx.read_status().packet_complete {}
    rx.clear_packet();
    false
}

fn send_static_packet<const BUF_SIZE: usize>(
    mut tx: AxiTx,
    rx: AxiRx<BUF_SIZE>,
    _str: &mut String<STRING_SIZE>,
) -> bool {
    let packet = [0x01, 0x02, 0x03, 0x04];
    tx.send(&packet);
    while !rx.read_status().packet_complete {}
    rx.clear_packet();
    false
}

fn read_rx_status<const BUF_SIZE: usize>(
    _tx: AxiTx,
    rx: AxiRx<BUF_SIZE>,
    str: &mut String<STRING_SIZE>,
) -> bool {
    let status = rx.read_status();
    if status.packet_complete || status.buffer_full {
        uwrite!(
            str,
            "Packet complete and buffer full are {} and {}",
            status.packet_complete,
            status.buffer_full
        )
        .unwrap();
        return true;
    }
    false
}

fn clear_rx_status<const BUF_SIZE: usize>(
    _tx: AxiTx,
    rx: AxiRx<BUF_SIZE>,
    _str: &mut String<STRING_SIZE>,
) -> bool {
    rx.clear_status();
    false
}
fn clear_rx_packet_register<const BUF_SIZE: usize>(
    _tx: AxiTx,
    rx: AxiRx<BUF_SIZE>,
    _str: &mut String<STRING_SIZE>,
) -> bool {
    rx.clear_packet_register();
    false
}
fn read_rx_packet_length<const BUF_SIZE: usize>(
    _tx: AxiTx,
    rx: AxiRx<BUF_SIZE>,
    str: &mut String<STRING_SIZE>,
) -> bool {
    if rx_clear_and_verify(rx, str) {
        return true;
    }
    let len = rx.packet_length();
    if len > 0 {
        uwrite!(str, "Packet length is not 0, but {}", len).unwrap();
        return true;
    }
    false
}

fn send_receive_empty_packet<const BUF_SIZE: usize>(
    mut tx: AxiTx,
    rx: AxiRx<BUF_SIZE>,
    str: &mut String<STRING_SIZE>,
) -> bool {
    if rx_clear_and_verify(rx, str) {
        return true;
    }
    let packet: [u8; 0] = [0; 0];
    tx.send(&packet);
    let mut received = [0; 32];

    match rx.receive_with_timeout(&mut received, 10) {
        Some(i) => {
            if i != 0 {
                uwrite!(str, "Received packet length is not 0, but {}", i as u32).unwrap();
                return true;
            }
        }
        None => {
            uwrite!(str, "Packet not received").unwrap();
            return true;
        }
    }
    false
}

const N_PACKETS: usize = 5;
/// Generate a random packet with a random length, send it over AxiTx and verify that it is received on AxiRx<BUF_SIZE>.
fn send_receive_random_packet<const BUF_SIZE: usize>(
    mut tx: AxiTx,
    rx: AxiRx<BUF_SIZE>,
    str: &mut String<STRING_SIZE>,
) -> bool {
    let mut rng = SmallRng::seed_from_u64(0x0DDB1A5E5BAD5EED);
    const MAX_LEN: usize = 128;
    let mut tx_buffer = [0; MAX_LEN];
    let mut rx_buffer = [0; MAX_LEN];
    if BUF_SIZE != MAX_LEN {
        uwrite!(str, "Buffer size is {}, bust must be {}", BUF_SIZE, MAX_LEN).unwrap();
        return true;
    }
    for _ in 0..N_PACKETS {
        // Make sure there is no packet in the rx buffer.
        if rx_clear_and_verify(rx, str) {
            return true;
        }

        // Generate a random packet.
        let len = (rng.next_u32() % MAX_LEN as u32) as usize;
        for dst in tx_buffer.iter_mut() {
            *dst = rng.next_u32() as u8;
        }
        let tx_packet = &tx_buffer[0..len];

        // Send the packet.
        tx.send(tx_packet);

        // Receive the packet.
        match rx.receive_with_timeout(&mut rx_buffer, 10) {
            Some(i) => {
                let rx_packet = &rx_buffer[0..i];
                let status = rx.read_status();
                if !status.packet_complete {
                    uwriteln!(str, "Packet complete: {}", status.packet_complete).unwrap();
                    uwriteln!(str, "Buffer full: {}", status.buffer_full).unwrap();
                    return true;
                }
                if i != len {
                    uwriteln!(str, "Packet length: sent: {}, received:{}", len, i).unwrap();
                    return true;
                }
                if rx_packet != tx_packet {
                    uwriteln!(str, "Received packet does not match").unwrap();
                    uwriteln!(str, "Sent: ").unwrap();
                    for d in tx_packet {
                        uwrite!(str, "{:02X} ", *d).unwrap();
                    }
                    uwriteln!(str, "Received: ").unwrap();
                    for d in rx_packet {
                        uwrite!(str, "{:02X} ", *d).unwrap();
                    }
                    return true;
                }
            }
            None => {
                uwrite!(str, "Packet not received").unwrap();
                return true;
            }
        }
    }
    false
}

fn send_multiple_packets_receive_all<const BUF_SIZE: usize>(
    mut tx: AxiTx,
    rx: AxiRx<BUF_SIZE>,
    str: &mut String<STRING_SIZE>,
) -> bool {
    let mut rng = SmallRng::seed_from_u64(0x0DDB1A5E5BAD5EED);
    const MAX_LEN: usize = 128;
    if BUF_SIZE != MAX_LEN {
        uwrite!(str, "Buffer size is {}, but must be {}", BUF_SIZE, MAX_LEN).unwrap();
        return true;
    }

    // Make sure there is no packet in the rx buffer.
    if rx_clear_and_verify(rx, str) {
        return true;
    }

    let mut tx_buffers = [[0; MAX_LEN]; N_PACKETS];
    let mut tx_packets: [&[u8]; N_PACKETS] = [&[]; N_PACKETS];
    for (i, tx_buffer) in tx_buffers.iter_mut().enumerate() {
        // Generate a random packet.
        let len = generate_random_packet(&mut rng, tx_buffer);
        tx_packets[i] = &tx_buffer[0..len];
        // Send the packet.
        tx.send(tx_packets[i]);
    }
    let mut rx_buffer = [0; MAX_LEN];
    for tx_packet in tx_packets {
        // Receive the packet.
        match rx.receive_with_timeout(&mut rx_buffer, 10) {
            Some(i) => {
                let rx_packet = &rx_buffer[0..i];
                let status = rx.read_status();
                if !status.packet_complete {
                    uwriteln!(str, "Packet complete: {}", status.packet_complete).unwrap();
                    uwriteln!(str, "Buffer full: {}", status.buffer_full).unwrap();
                    return true;
                }
                if rx_packet != tx_packet {
                    uwriteln!(str, "Received packet does not match").unwrap();
                    uwriteln!(str, "Sent: ").unwrap();
                    for d in tx_packet {
                        uwrite!(str, "{:02X} ", *d).unwrap();
                    }
                    uwriteln!(str, "Received: ").unwrap();
                    for d in rx_packet {
                        uwrite!(str, "{:02X} ", *d).unwrap();
                    }
                    return true;
                }
                rx.clear_packet();
                rx.clear_status();
            }
            None => {
                uwrite!(str, "Packet not received").unwrap();
                return true;
            }
        }
    }
    false
}
fn rx_clear_and_verify<const BUF_SIZE: usize>(
    rx: AxiRx<BUF_SIZE>,
    str: &mut String<STRING_SIZE>,
) -> bool {
    rx.clear_packet();
    rx.clear_status();
    let status = rx.read_status();
    if status.packet_complete {
        uwrite!(str, "Packet complete is set").unwrap();
        return true;
    }
    if status.buffer_full {
        uwrite!(str, "Buffer full is set").unwrap();
        return true;
    }
    let len = rx.packet_length();
    if len != 0 {
        uwrite!(str, "Packet length is not 0, but {}", len).unwrap();
        return true;
    }
    false
}

fn generate_random_packet(rng: &mut SmallRng, buffer: &mut [u8]) -> usize {
    let len = (rng.next_u32() % buffer.len() as u32) as usize;
    for dst in buffer.iter_mut() {
        *dst = rng.next_u32() as u8;
    }
    len
}
