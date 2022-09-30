#![no_std]
#![no_main]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_sys::gppe::gather_unit;
use riscv_rt::entry;

const NUM_GPPES: usize = 2;
const NUM_LINKS: usize = 1;

const NUM_RX_TX_UNITS: usize = NUM_GPPES + NUM_LINKS + 1;

const FRAME_SIZE: usize =
    GPPE_FRAME_SIZE * 2 + core::mem::size_of::<(u64, u64)>() * NUM_RX_TX_UNITS;
const GPPE_FRAME_SIZE: usize = 8;

/*
Different APIs for Scatter/Gather unit for GPPEs and Management unit
 => different modules, `gppe`/`management`

GPPEs deal with scatter/gather units, management unit deals with calendars

- GPPEs don't have access to timing oracles
- mmu has access to all calendars
    - all S/G units
    - switch
- MMU has access to timing oracles
    - rx/tx units

    - tx unit
        - start state machine
        - stop state machine
            - GU::send_sequence_counter right now

    - rx unit
        - first address 32bit
        - 0 is stopped
        - non zero is doing-something :tm:
        - start state machine
        - stop state machine

        - tuple of local and remote sequence counter

        - special value of record_remote_sequence_counter register
          to indicate whether sequence counters have been captured or not
        - polling to wait for captured counters
        - array, poll and wait for all to terminate
            - if timeout then fatal error
            - maybe "turn off" recording?

        - optional?: show progress with printing
            - send counters over a link?
*/

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let init = unsafe { bittide_sys::Initialiser::new().unwrap() };
    let _scatter_unit = unsafe {
        init.initialise_scatter_unit::<FRAME_SIZE>("scatter-unit")
            .unwrap()
    };
    let mut gather_unit = unsafe {
        init.initialise_gather_unit::<FRAME_SIZE>("gather-unit")
            .unwrap()
    };

    let rx_unit = unsafe { init.initialise_rx_unit("rx-unit").unwrap() };
    let tx_unit = unsafe { init.initialise_tx_unit("tx-unit").unwrap() };

    let switch_0_rx_unit = unsafe { init.initialise_rx_unit("switch-0-rx-unit").unwrap() };
    let switch_0_tx_unit = unsafe { init.initialise_tx_unit("switch-0-tx-unit").unwrap() };

    let gppe_0_rx_unit = unsafe { init.initialise_rx_unit("gppe-0-rx-unit").unwrap() };
    let gppe_0_tx_unit = unsafe { init.initialise_tx_unit("gppe-0-tx-unit").unwrap() };

    let gppe_1_rx_unit = unsafe { init.initialise_rx_unit("gppe-1-rx-unit").unwrap() };
    let gppe_1_tx_unit = unsafe { init.initialise_tx_unit("gppe-1-tx-unit").unwrap() };

    let mut rxs = [rx_unit, switch_0_rx_unit, gppe_0_rx_unit, gppe_1_rx_unit];
    let mut txs = [tx_unit, switch_0_tx_unit, gppe_0_tx_unit, gppe_1_tx_unit];

    for rx in &mut rxs {
        rx.record_remote_sequence_counter(true);
    }

    for tx in &mut txs {
        tx.send_sequence_counter(true);
    }

    let mut counters = [None; 4];

    const TIMEOUT: usize = 0x1000_0000;

    let mut counter = 0;

    loop {
        for (i, rx) in rxs.iter_mut().enumerate() {
            match rx.sequence_counters() {
                None => {}
                Some(counts) => {
                    counters[i] = Some(counts);
                    txs[i].send_sequence_counter(false);
                }
            }
        }

        if counters.iter().all(Option::is_some) {
            break;
        }

        if counter == TIMEOUT {
            panic!("Timeout when waiting for timing oracles");
        }

        counter += 1;
    }

    gather_unit.wait_for_new_metacycle();
    notify_link(&mut gather_unit, &counters);

    // write_calendars(); // TODO

    start_pes(&mut gather_unit);

    loop {
        continue;
    }
}

fn notify_link(
    gu: &mut gather_unit::GatherUnit<FRAME_SIZE>,
    counters: &[Option<(u64, u64)>; NUM_RX_TX_UNITS],
) {
    let data = counters.map(|x| {
        let (a, b) = x.unwrap();
        [a, b]
    });

    gu.write_frame_memory(GPPE_FRAME_SIZE * 2, data);
}

fn start_pes(gu: &mut gather_unit::GatherUnit<FRAME_SIZE>) {
    gu.write_frame_memory(0, 1u64);
    gu.write_frame_memory(GPPE_FRAME_SIZE, 1u64);
}
