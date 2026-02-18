// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

package cpu

import spinal.core._
import spinal.lib._
import spinal.lib.com.jtag.Jtag
import spinal.lib.cpu.riscv.debug._
import vexriscv.plugin._
import vexriscv.{VexRiscv, VexRiscvConfig, plugin}

object Riscv32imc0 extends App {
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

        new DBusSimplePlugin(
          catchAddressMisaligned = true,
          catchAccessFault = true
        ),

        new StaticMemoryTranslatorPlugin(
          ioRange = _ => True
        ),

        new CsrPlugin(
            CsrPluginConfig.smallest.copy(
              ebreakGen = true,
              mtvecAccess = CsrAccess.READ_WRITE,
              withPrivilegedDebug = true
              )
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
        new EmbeddedRiscvJtag(
          p = DebugTransportModuleParameter(
            addressWidth = 7,
            version      = 1,
            idle         = 7
          ),
          debugCd = ClockDomain.current,
          // PART: (QB)ayLogic
          // V:    version, we use this to enumerate different cores
          // MMM:  manufacturer ID (0x00 = unknown), last bit set imposed by JTAG spec
          //
          //         VPARTMMM
          jtagId = 0x0514C001
        )
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

        case _ =>
      }
    }

    return cpu
  }
  val prefix = getClass.getSimpleName.replace("$", "")

  SpinalConfig(
    globalPrefix = prefix
  ).generateVerilog(cpu())
}
