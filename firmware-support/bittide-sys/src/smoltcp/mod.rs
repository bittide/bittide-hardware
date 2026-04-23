// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Networking stack built on smoltcp.
//!
//! The main entry point is [`link_interface::LinkInterface`], which combines a
//! [`ringbuffer::RingbufferDevice`] with a TCP socket to provide reliable
//! bidirectional communication with a direct neighbor.
//!
//! Wire format types live in [`link_protocol`].

pub mod axi;
pub mod link_interface;
pub mod link_protocol;
pub mod mac;
pub mod ringbuffer;
