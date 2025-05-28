# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

set_property CLOCK_DEDICATED_ROUTE BACKBONE [get_pins -hier -filter {NAME=~ */u_ddr4_infrastructure/gen_mmcme*.u_mmcme_adv_inst/CLKIN1}]

set_property BOARD_PIN {c0_ddr4_dm_dbi_n0} [get_ports c0_ddr4_dm_dbi_n]

# BitVector 8 and there is are dq0 to dq63
set_property BOARD_PIN {c0_ddr4_dq0}       [get_ports {c0_ddr4_dq[0]}]
set_property BOARD_PIN {c0_ddr4_dq1}       [get_ports {c0_ddr4_dq[1]}]
set_property BOARD_PIN {c0_ddr4_dq2}       [get_ports {c0_ddr4_dq[2]}]
set_property BOARD_PIN {c0_ddr4_dq3}       [get_ports {c0_ddr4_dq[3]}]
set_property BOARD_PIN {c0_ddr4_dq4}       [get_ports {c0_ddr4_dq[4]}]
set_property BOARD_PIN {c0_ddr4_dq5}       [get_ports {c0_ddr4_dq[5]}]
set_property BOARD_PIN {c0_ddr4_dq6}       [get_ports {c0_ddr4_dq[6]}]
set_property BOARD_PIN {c0_ddr4_dq7}       [get_ports {c0_ddr4_dq[7]}]

set_property BOARD_PIN {c0_ddr4_dqs_t0}    [get_ports c0_ddr4_dqs_t]
set_property BOARD_PIN {c0_ddr4_dqs_c0}    [get_ports c0_ddr4_dqs_c]

# BitVector 17 and there are adr0 to adr16
set_property BOARD_PIN {c0_ddr4_adr0}      [get_ports {c0_ddr4_adr[0]}]
set_property BOARD_PIN {c0_ddr4_adr1}      [get_ports {c0_ddr4_adr[1]}]
set_property BOARD_PIN {c0_ddr4_adr2}      [get_ports {c0_ddr4_adr[2]}]
set_property BOARD_PIN {c0_ddr4_adr3}      [get_ports {c0_ddr4_adr[3]}]
set_property BOARD_PIN {c0_ddr4_adr4}      [get_ports {c0_ddr4_adr[4]}]
set_property BOARD_PIN {c0_ddr4_adr5}      [get_ports {c0_ddr4_adr[5]}]
set_property BOARD_PIN {c0_ddr4_adr6}      [get_ports {c0_ddr4_adr[6]}]
set_property BOARD_PIN {c0_ddr4_adr7}      [get_ports {c0_ddr4_adr[7]}]
set_property BOARD_PIN {c0_ddr4_adr8}      [get_ports {c0_ddr4_adr[8]}]
set_property BOARD_PIN {c0_ddr4_adr9}      [get_ports {c0_ddr4_adr[9]}]
set_property BOARD_PIN {c0_ddr4_adr10}     [get_ports {c0_ddr4_adr[10]}]
set_property BOARD_PIN {c0_ddr4_adr11}     [get_ports {c0_ddr4_adr[11]}]
set_property BOARD_PIN {c0_ddr4_adr12}     [get_ports {c0_ddr4_adr[12]}]
set_property BOARD_PIN {c0_ddr4_adr13}     [get_ports {c0_ddr4_adr[13]}]
set_property BOARD_PIN {c0_ddr4_adr14}     [get_ports {c0_ddr4_adr[14]}]
set_property BOARD_PIN {c0_ddr4_adr15}     [get_ports {c0_ddr4_adr[15]}]
set_property BOARD_PIN {c0_ddr4_adr16}     [get_ports {c0_ddr4_adr[16]}]
# BitVector 2 and there are ba0 and ba1
set_property BOARD_PIN {c0_ddr4_ba0}       [get_ports {c0_ddr4_ba[0]}]
set_property BOARD_PIN {c0_ddr4_ba1}       [get_ports {c0_ddr4_ba[1]}]
set_property BOARD_PIN {c0_ddr4_bg}        [get_ports c0_ddr4_bg]

# All one bit ports map to their identically named BOARD_PINs
set_property BOARD_PIN {c0_ddr4_cke}       [get_ports c0_ddr4_cke]
set_property BOARD_PIN {c0_ddr4_odt}       [get_ports c0_ddr4_odt]
set_property BOARD_PIN {c0_ddr4_cs_n}      [get_ports c0_ddr4_cs_n]
set_property BOARD_PIN {c0_ddr4_ck_t}      [get_ports c0_ddr4_ck_t]
set_property BOARD_PIN {c0_ddr4_ck_c}      [get_ports c0_ddr4_ck_c]
set_property BOARD_PIN {c0_ddr4_act_n}     [get_ports c0_ddr4_act_n]
set_property BOARD_PIN {c0_ddr4_reset_n}   [get_ports c0_ddr4_reset_n]
