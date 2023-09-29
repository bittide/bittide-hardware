// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

package example

import spinal.core._
import spinal.lib._
import spinal.lib.com.jtag.Jtag
import spinal.lib.bus.wishbone.{Wishbone, WishboneConfig}

import vexriscv.plugin._
import vexriscv.{VexRiscv, VexRiscvConfig, plugin}
import vexriscv.ip.{DataCacheConfig}
import vexriscv.ip.fpu.FpuParameter

object ExampleCpu extends App {
  def cpu() : VexRiscv = {

    val config = VexRiscvConfig(
      plugins = List(
        new IBusSimplePlugin(
          resetVector = 0x20000000l,
          cmdForkOnSecondStage = false,
          cmdForkPersistence = false,
          prediction = NONE,
          catchAccessFault = true,
          compressedGen = true // C extension
        ),

        // new DBusSimplePlugin(
        //   catchAddressMisaligned = true,
        //   catchAccessFault = true
        // ),

        new DBusCachedPlugin(
          /*
          config = new DataCacheConfig(
            cacheSize        = 2048,
            bytePerLine      = 32,
            wayCount         = 1,
            addressWidth     = 32,
            cpuDataWidth     = 32,
            memDataWidth     = 32,
            catchAccessError = true,
            catchIllegal     = true,
            catchUnaligned   = true
          )
          */
          config = new DataCacheConfig(
            cacheSize        = 8,
            bytePerLine      = 8,
            wayCount         = 1,
            addressWidth     = 32,
            cpuDataWidth     = 32,
            memDataWidth     = 32,
            catchAccessError = true,
            catchIllegal     = true,
            catchUnaligned   = true
          )
        ),

        new StaticMemoryTranslatorPlugin(
          ioRange = _ => True
        ),

        new CsrPlugin(
          CsrPluginConfig.smallest.copy(ebreakGen = true, mtvecAccess = CsrAccess.READ_WRITE)
        ),
        new DecoderSimplePlugin(
          catchIllegalInstruction = true
        ),
        new RegFilePlugin(
          regFileReadyKind = plugin.SYNC,
          zeroBoot = false
        ),
        new IntAluPlugin,
        new SrcPlugin(
          separatedAddSub = false,
          executeInsertion = false
        ),

        // M extension
        new MulPlugin,
        new DivPlugin,

        // F extension
        new FpuPlugin(
          p = FpuParameter(
            withDouble = false // enable for D extension
          )
        ),

        new LightShifterPlugin,
        new HazardSimplePlugin(
          bypassExecute           = false,
          bypassMemory            = false,
          bypassWriteBack         = false,
          bypassWriteBackBuffer   = false,
          pessimisticUseSrc       = false,
          pessimisticWriteRegFile = false,
          pessimisticAddressMatch = false
        ),
        new BranchPlugin(
          earlyBranch = false,
          catchAddressMisaligned = true
        ),
        new DebugPlugin(ClockDomain.current),
        new YamlPlugin("ExampleCpu.yaml")
      )
    )

    val cpu = new VexRiscv(config)

    cpu.rework {
      for (plugin <- config.plugins) plugin match {
        case plugin: IBusSimplePlugin => {
          plugin.iBus.setAsDirectionLess() //Unset IO properties of iBus
          master(plugin.iBus.toWishbone()).setName("iBusWishbone")
        }
        case plugin: DBusSimplePlugin => {
          plugin.dBus.setAsDirectionLess()
          master(plugin.dBus.toWishbone()).setName("dBusWishbone")
        }

        case plugin: DBusCachedPlugin => {
          plugin.dBus.setAsDirectionLess()
          master(plugin.dBus.toWishbone()).setName("dBusWishbone")
        }
        
        case plugin: DebugPlugin => plugin.debugClockDomain {
          plugin.io.bus.setAsDirectionLess() 
          val jtag = slave(new Jtag()).setName("jtag")
          jtag <> plugin.io.bus.fromJtag()
        }
        case _ =>
      }
    }

    return cpu
  }

  SpinalVerilog(cpu())
}
