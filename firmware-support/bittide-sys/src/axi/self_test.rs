// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::axi::{AxiRx, AxiTx};
use heapless::String;
use rand::rngs::SmallRng;
use rand::{RngCore, SeedableRng};
use ufmt::uwrite;

use crate::uart::Uart;
use ufmt::uwriteln;

/// Tests for the axi module.
#[allow(dead_code)]
pub fn self_test(mut uart: Uart, tx: AxiTx, rx: AxiRx) {
    // Construct a list of tests with their names.
    let tests = [
        (
            read_rx_status as fn(AxiTx, AxiRx, &mut String<256>) -> bool,
            "read_rx_status",
        ),
        (
            clear_rx_status as fn(AxiTx, AxiRx, &mut String<256>) -> bool,
            "clear_rx_status",
        ),
        (
            clear_rx_packet_register as fn(AxiTx, AxiRx, &mut String<256>) -> bool,
            "clear_rx_packet_register",
        ),
        (
            send_empty_packet as fn(AxiTx, AxiRx, &mut String<256>) -> bool,
            "send_empty_packet",
        ),
        (
            send_static_packet as fn(AxiTx, AxiRx, &mut String<256>) -> bool,
            "send_static_packet",
        ),
        (
            send_receive_empty_packet as fn(AxiTx, AxiRx, &mut String<256>) -> bool,
            "send_receive_empty_packet",
        ),
        (
            send_receive_static_packet as fn(AxiTx, AxiRx, &mut String<256>) -> bool,
            "send_receive_static_packet",
        ),
        (
            read_rx_packet_length as fn(AxiTx, AxiRx, &mut String<256>) -> bool,
            "read_rx_packet_length",
        ),
        (
            send_receive_random_packet as fn(AxiTx, AxiRx, &mut String<256>) -> bool,
            "send_receive_random_packet",
        ),
    ];
    let mut str = String::new();

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

fn send_empty_packet(tx: AxiTx, rx: AxiRx, _str: &mut String<256>) -> bool {
    let packet = [];
    tx.send(&packet);
    while !rx.read_status().packet_complete {}
    rx.clear_packet();
    false
}

fn send_static_packet(tx: AxiTx, rx: AxiRx, _str: &mut String<256>) -> bool {
    let packet = [0x01, 0x02, 0x03, 0x04];
    tx.send(&packet);
    while !rx.read_status().packet_complete {}
    rx.clear_packet();
    false
}

fn read_rx_status(_tx: AxiTx, rx: AxiRx, str: &mut String<256>) -> bool {
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

fn clear_rx_status(_tx: AxiTx, rx: AxiRx, _str: &mut String<256>) -> bool {
    rx.clear_status();
    false
}
fn clear_rx_packet_register(_tx: AxiTx, rx: AxiRx, _str: &mut String<256>) -> bool {
    rx.clear_packet_register();
    false
}
fn read_rx_packet_length(_tx: AxiTx, rx: AxiRx, str: &mut String<256>) -> bool {
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

fn send_receive_empty_packet(tx: AxiTx, rx: AxiRx, str: &mut String<256>) -> bool {
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

fn send_receive_static_packet(tx: AxiTx, rx: AxiRx, str: &mut String<256>) -> bool {
    if rx_clear_and_verify(rx, str) {
        return true;
    }
    let packet = [0x01, 0x02, 0x03, 0x04, 0x5];
    tx.send(&packet);
    let mut received = [0; 32];
    match rx.receive_with_timeout(&mut received, 1) {
        Some(i) => {
            let status = rx.read_status();
            if !(status.packet_complete && !status.buffer_full) {
                uwriteln!(str, "Packet complete: {}", status.packet_complete).unwrap();
                uwriteln!(str, "Buffer full: {}", status.buffer_full).unwrap();
                return true;
            }
            if i != packet.len() {
                uwriteln!(str, "Packet length: {}", i).unwrap();
                return true;
            }
            let received_packet = &received[0..packet.len()];
            if received_packet != packet {
                uwriteln!(str, "Received packet does not match").unwrap();
                uwriteln!(str, "Sent: ").unwrap();
                for d in packet {
                    uwrite!(str, "{:02X}", d).unwrap();
                }
                uwriteln!(str, "Received: ").unwrap();
                for d in received_packet {
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
    false
}

//// Generate a random packet with a random length, send it over AxiTx and verify that it is received on AxiRx.
fn send_receive_random_packet(tx: AxiTx, rx: AxiRx, str: &mut String<256>) -> bool {
    if rx_clear_and_verify(rx, str) {
        return true;
    }
    let mut rng = SmallRng::seed_from_u64(0x0DDB1A5E5BAD5EED);
    const MAX_LEN: usize = 128;
    if rx.buffer_size != MAX_LEN as usize {
        uwrite!(
            str,
            "Buffer size is {}, bust must be {}",
            rx.buffer_size,
            MAX_LEN
        )
        .unwrap();
        return true;
    }
    let mut tx_buffer = [0; MAX_LEN];
    let mut rx_buffer = [0; 4 + MAX_LEN];

    let len = (rng.next_u32() % MAX_LEN as u32) as usize;
    for i in 0..len {
        tx_buffer[i] = rng.next_u32() as u8;
    }
    let tx_packet = &tx_buffer[0..len];
    tx.send(tx_packet);

    match rx.receive_with_timeout(&mut rx_buffer, 2 + len) {
        Some(i) => {
            let rx_packet = &rx_buffer[0..i];
            let status = rx.read_status();
            if !(status.packet_complete && !status.buffer_full) {
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
                    uwrite!(str, "{:02X}", *d).unwrap();
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
    false
}

fn rx_clear_and_verify(rx: AxiRx, str: &mut String<256>) -> bool {
    rx.clear_packet();
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
