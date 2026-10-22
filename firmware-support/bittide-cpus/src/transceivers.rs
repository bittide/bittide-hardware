// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::manual_additions::timer::Duration;
use bittide_hal::shared_devices::{Timer, Transceivers, Uart};
use ufmt::derive::uDebug;
use ufmt::uwriteln;

/// Reasons why the transceiver links are not (or no longer) considered up.
#[derive(uDebug, Clone, Copy)]
pub enum LinkError {
    /// Not all links came up within the given timeout.
    Timeout,
    /// A link that was up went down. Contains the link index.
    LinkDown { link: usize },
    /// The reset manager of a link detected a failure after the link was up.
    /// Contains the link index and the number of failures seen.
    FailAfterUp { link: usize, fail_after_ups: u32 },
}

/// Whether both the receive and the transmit side of the given link are ready to
/// send and receive user data.
pub fn link_up(transceivers: &Transceivers, link: usize) -> bool {
    transceivers.rx_data_init_dones(link) == Some(true)
        && transceivers.tx_data_init_dones(link) == Some(true)
}

/// Whether all links are up. See [`link_up`].
pub fn all_links_up(transceivers: &Transceivers) -> bool {
    (0..Transceivers::RX_DATA_INIT_DONES_LEN).all(|link| link_up(transceivers, link))
}

/// Wait until all links are up, or until `timeout` has passed.
pub fn wait_links_up(
    transceivers: &Transceivers,
    timer: &Timer,
    timeout: Duration,
) -> Result<(), LinkError> {
    let deadline = timer.now() + timeout;
    while !all_links_up(transceivers) {
        if timer.now() > deadline {
            return Err(LinkError::Timeout);
        }
    }
    Ok(())
}

/// Check that all links stay up for `duration`. Fails as soon as a link goes down
/// or the number of failures after link-up of any link increases.
pub fn monitor_links(
    transceivers: &Transceivers,
    timer: &Timer,
    duration: Duration,
) -> Result<(), LinkError> {
    let mut start_fail_after_ups = [0; Transceivers::STATISTICS_LEN];
    for (link, start) in start_fail_after_ups.iter_mut().enumerate() {
        *start = fail_after_ups(transceivers, link);
    }

    let deadline = timer.now() + duration;
    loop {
        for (link, start) in start_fail_after_ups.iter().enumerate() {
            if !link_up(transceivers, link) {
                return Err(LinkError::LinkDown { link });
            }
            let current = fail_after_ups(transceivers, link);
            if current != *start {
                return Err(LinkError::FailAfterUp {
                    link,
                    fail_after_ups: current,
                });
            }
        }
        if timer.now() > deadline {
            return Ok(());
        }
    }
}

/// Print the reset manager statistics of all links.
pub fn print_statistics(transceivers: &Transceivers, uart: &mut Uart) {
    for (link, stats) in transceivers.statistics_volatile_iter().enumerate() {
        uwriteln!(uart, "Link {}: {:?}", link, stats).unwrap();
    }
}

fn fail_after_ups(transceivers: &Transceivers, link: usize) -> u32 {
    transceivers
        .statistics(link)
        .map(|s| s.fail_after_ups.into_inner())
        .unwrap_or(0)
}
