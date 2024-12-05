# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set logging file ./_build/hitl/gdb-out.log
set logging overwrite on
set logging enabled on
file "./_build/cargo/firmware-binaries/riscv32imc-unknown-none-elf/release/smoltcp_client"

break ExceptionHandler
break smoltcp_client::gdb_panic

target extended-remote :3333
load

define hook-stop
printf "!!! program stopped executing !!!"
i r
bt
quit 1
end

continue
