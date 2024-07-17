// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
pub mod axi;
pub mod loopback;

use smoltcp::wire::EthernetAddress;

/// Clears the multicast bit in the MAC address.
/// This is the most significant bit of the first byte.
/// ```
/// use smoltcp::wire::EthernetAddress;
/// use bittide_sys::smoltcp::set_unicast;
/// let mut mac = EthernetAddress::from_bytes(&[0xFF; 6]);
/// assert!(!mac.is_unicast());
/// set_unicast(&mut mac);
/// assert!(mac.is_unicast());
/// ```
pub fn set_unicast(addr: &mut EthernetAddress) {
    (addr).0[0] &= 0b11111110;
}

/// Set the locally administered bit in the MAC address.
/// This is the second least significant bit of the first byte.
/// ```
/// use smoltcp::wire::EthernetAddress;
/// use bittide_sys::smoltcp::set_local;
/// let mut mac = EthernetAddress::from_bytes(&[0; 6]);
/// assert!(!mac.is_local());
/// set_local(&mut mac);
/// assert!(mac.is_local());
/// ```
pub fn set_local(addr: &mut EthernetAddress) {
    (addr).0[0] |= 0b00000010;
}
