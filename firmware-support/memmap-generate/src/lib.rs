// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

pub mod generators;
pub mod parse;

pub use crate::generators::{
    generate_rust_wrappers, DebugDerive, GenerateConfig, ItemScopeMode, RustWrappers,
};
pub use crate::parse::{parse, MemoryMapDesc};

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_generators() {
        let mm = parse(TEST_SRC).unwrap();
        let config = GenerateConfig {
            debug_derive_mode: DebugDerive::Ufmt,
            item_scope_mode: ItemScopeMode::OneFile,
        };
        let wrappers = generate_rust_wrappers(&mm, &config);

        for (dev_name, dev_src) in &wrappers.device_defs {
            // println!("==========");
            // println!("{dev_name}.rs");
            // println!("==========");
            // println!();

            println!("{}", dev_src.to_string());
        }

        for (type_name, ty_src) in &wrappers.type_defs {
            // println!("==========");
            // println!("{type_name}.rs");
            // println!("==========");
            // println!();

            println!("{}", ty_src.to_string());
        }
    }

    const TEST_SRC: &'static str = r#"
{
  "devices": {
    "StatusReg": {
      "description": "",
      "name": "StatusReg",
      "registers": [
        {
          "access": "write_only",
          "address": 0,
          "description": "",
          "name": "status",
          "reset": null,
          "size": 1,
          "src_location": {
            "end_col": 44,
            "end_line": 77,
            "file": "src/Bittide/Instances/Hitl/VexRiscv.hs",
            "module": "Bittide.Instances.Hitl.VexRiscv",
            "package": "bittide-instances-0.1-inplace",
            "start_col": 34,
            "start_line": 77
          },
          "type": [
            "reference",
            "TestStatus",
            []
          ]
        }
      ],
      "src_location": {
        "end_col": 44,
        "end_line": 77,
        "file": "src/Bittide/Instances/Hitl/VexRiscv.hs",
        "module": "Bittide.Instances.Hitl.VexRiscv",
        "package": "bittide-instances-0.1-inplace",
        "start_col": 34,
        "start_line": 77
      }
    },
    "Storage_Data": {
      "description": "",
      "name": "Storage_Data",
      "registers": [
        {
          "access": "read_write",
          "address": 0,
          "description": "",
          "name": "data",
          "reset": null,
          "size": 65536,
          "src_location": {
            "end_col": 35,
            "end_line": 234,
            "file": "src/Bittide/ProcessingElement.hs",
            "module": "Bittide.ProcessingElement",
            "package": "bittide-0.1-inplace",
            "start_col": 25,
            "start_line": 234
          },
          "type": [
            "vector",
            16384,
            [
              "bitvector",
              32
            ]
          ]
        }
      ],
      "src_location": {
        "end_col": 35,
        "end_line": 234,
        "file": "src/Bittide/ProcessingElement.hs",
        "module": "Bittide.ProcessingElement",
        "package": "bittide-0.1-inplace",
        "start_col": 25,
        "start_line": 234
      }
    },
    "Storage_Instruction": {
      "description": "",
      "name": "Storage_Instruction",
      "registers": [
        {
          "access": "read_write",
          "address": 0,
          "description": "",
          "name": "data",
          "reset": null,
          "size": 65536,
          "src_location": {
            "end_col": 40,
            "end_line": 236,
            "file": "src/Bittide/ProcessingElement.hs",
            "module": "Bittide.ProcessingElement",
            "package": "bittide-0.1-inplace",
            "start_col": 28,
            "start_line": 236
          },
          "type": [
            "vector",
            16384,
            [
              "bitvector",
              32
            ]
          ]
        }
      ],
      "src_location": {
        "end_col": 40,
        "end_line": 236,
        "file": "src/Bittide/ProcessingElement.hs",
        "module": "Bittide.ProcessingElement",
        "package": "bittide-0.1-inplace",
        "start_col": 28,
        "start_line": 236
      }
    },
    "Timer": {
      "description": "Circuit that contains a free running 64 bit counter. We can observe this counter to get a sense of time, overflows should be accounted for by the master.",
      "name": "Timer",
      "registers": [
        {
          "access": "write_only",
          "address": 0,
          "description": "",
          "name": "freeze_count",
          "reset": null,
          "size": 1,
          "src_location": {
            "end_col": 0,
            "end_line": 0,
            "file": "",
            "module": "",
            "package": "",
            "start_col": 0,
            "start_line": 0
          },
          "type": [
            "bitvector",
            1
          ]
        },
        {
          "access": "read_only",
          "address": 8,
          "description": "",
          "name": "counter",
          "reset": null,
          "size": 8,
          "src_location": {
            "end_col": 0,
            "end_line": 0,
            "file": "",
            "module": "",
            "package": "",
            "start_col": 0,
            "start_line": 0
          },
          "type": [
            "bitvector",
            64
          ]
        },
        {
          "access": "read_only",
          "address": 16,
          "description": "",
          "name": "frequency",
          "reset": null,
          "size": 8,
          "src_location": {
            "end_col": 0,
            "end_line": 0,
            "file": "",
            "module": "",
            "package": "",
            "start_col": 0,
            "start_line": 0
          },
          "type": [
            "bitvector",
            64
          ]
        }
      ],
      "src_location": {
        "end_col": 26,
        "end_line": 74,
        "file": "src/Bittide/Instances/Hitl/VexRiscv.hs",
        "module": "Bittide.Instances.Hitl.VexRiscv",
        "package": "bittide-instances-0.1-inplace",
        "start_col": 21,
        "start_line": 74
      }
    },
    "UART": {
      "description": "",
      "name": "UART",
      "registers": [
        {
          "access": "read_write",
          "address": 0,
          "description": "",
          "name": "data",
          "reset": null,
          "size": 1,
          "src_location": {
            "end_col": 0,
            "end_line": 0,
            "file": "",
            "module": "",
            "package": "",
            "start_col": 0,
            "start_line": 0
          },
          "type": [
            "bitvector",
            8
          ]
        },
        {
          "access": "read_only",
          "address": 4,
          "description": "",
          "name": "status",
          "reset": null,
          "size": 1,
          "src_location": {
            "end_col": 0,
            "end_line": 0,
            "file": "",
            "module": "",
            "package": "",
            "start_col": 0,
            "start_line": 0
          },
          "type": [
            "bitvector",
            2
          ]
        }
      ],
      "src_location": {
        "end_col": 11,
        "end_line": 76,
        "file": "src/Bittide/Instances/Hitl/VexRiscv.hs",
        "module": "Bittide.Instances.Hitl.VexRiscv",
        "package": "bittide-instances-0.1-inplace",
        "start_col": 6,
        "start_line": 76
      }
    }
  },
  "tree": {
    "interconnect": {
      "absolute_address": 0,
      "components": [
        {
          "relative_address": 2147483648,
          "size": 536870911,
          "tree": {
            "device_instance": {
              "absolute_address": 2147483648,
              "device_name": "Storage_Instruction",
              "instance_name": "Instruction",
              "src_location": {
                "end_col": 62,
                "end_line": 71,
                "file": "src/Bittide/Instances/Hitl/VexRiscv.hs",
                "module": "Bittide.Instances.Hitl.VexRiscv",
                "package": "bittide-instances-0.1-inplace",
                "start_col": 44,
                "start_line": 71
              }
            }
          }
        },
        {
          "relative_address": 1073741824,
          "size": 536870911,
          "tree": {
            "device_instance": {
              "absolute_address": 1073741824,
              "device_name": "Storage_Data",
              "instance_name": "Data",
              "src_location": {
                "end_col": 24,
                "end_line": 435,
                "file": "src/Protocols/MemoryMap.hs",
                "module": "Protocols.MemoryMap",
                "package": "clash-protocols-memmap-0.1-inplace",
                "start_col": 15,
                "start_line": 435
              }
            }
          }
        },
        {
          "relative_address": 2684354560,
          "size": 536870911,
          "tree": {
            "device_instance": {
              "absolute_address": 2684354560,
              "device_name": "Timer",
              "instance_name": "Timer",
              "src_location": {
                "end_col": 24,
                "end_line": 435,
                "file": "src/Protocols/MemoryMap.hs",
                "module": "Protocols.MemoryMap",
                "package": "clash-protocols-memmap-0.1-inplace",
                "start_col": 15,
                "start_line": 435
              }
            }
          }
        },
        {
          "relative_address": 3221225472,
          "size": 536870911,
          "tree": {
            "device_instance": {
              "absolute_address": 3221225472,
              "device_name": "UART",
              "instance_name": "UART",
              "src_location": {
                "end_col": 60,
                "end_line": 45,
                "file": "src/Bittide/Instances/MemoryMapLogic.hs",
                "module": "Bittide.Instances.MemoryMapLogic",
                "package": "bittide-instances-0.1-inplace",
                "start_col": 50,
                "start_line": 45
              }
            }
          }
        },
        {
          "relative_address": 3758096384,
          "size": 536870911,
          "tree": {
            "device_instance": {
              "absolute_address": 3758096384,
              "device_name": "StatusReg",
              "instance_name": "StatusReg",
              "src_location": {
                "end_col": 24,
                "end_line": 435,
                "file": "src/Protocols/MemoryMap.hs",
                "module": "Protocols.MemoryMap",
                "package": "clash-protocols-memmap-0.1-inplace",
                "start_col": 15,
                "start_line": 435
              }
            }
          }
        }
      ],
      "src_location": {
        "end_col": 48,
        "end_line": 233,
        "file": "src/Bittide/ProcessingElement.hs",
        "module": "Bittide.ProcessingElement",
        "package": "bittide-0.1-inplace",
        "start_col": 23,
        "start_line": 233
      }
    }
  },
  "types": {
    "TestStatus": {
      "definition": {
        "meta": {
          "is_newtype": false,
          "module": "Bittide.Instances.Hitl.VexRiscv",
          "package": "bittide-instances-0.1-inplace"
        },
        "name": "TestStatus",
        "variants": [
          {
            "fields": [],
            "name": "Running"
          },
          {
            "fields": [],
            "name": "Success"
          },
          {
            "fields": [],
            "name": "Fail"
          }
        ]
      },
      "generics": 0,
      "meta": {
        "is_newtype": false,
        "module": "Bittide.Instances.Hitl.VexRiscv",
        "package": "bittide-instances-0.1-inplace"
      },
      "name": "TestStatus"
    }
  }
}

    "#;
}
