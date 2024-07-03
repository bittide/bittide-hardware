// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::collections::HashMap;

use serde::Deserialize;
use serde_json::Value;

#[derive(Debug, Clone, Deserialize)]
pub struct MemoryMapDesc {
    pub devices: HashMap<String, DeviceDesc>,
    pub types: HashMap<String, TypeDefinition>,
    pub tree: MemoryMapTree,
}

#[derive(Debug, Clone, Deserialize)]
pub struct DeviceDesc {
    pub name: String,
    pub description: String,
    pub registers: Vec<RegisterDesc>,
    pub src_location: SourceLocation,
}

#[derive(Debug, Clone, Deserialize)]
pub struct TypeDefinition {
    pub name: String,
    pub meta: TypeMeta,
    pub generics: u64,
    pub definition: Type,
}

#[derive(Debug, Clone, Deserialize)]
// #[serde(rename = "snake_case")]
pub enum MemoryMapTree {
    #[serde(rename = "interconnect")]
    Interconnect {
        absolute_address: u64,
        components: Vec<InterconnectComponent>,
        src_location: SourceLocation,
    },
    #[serde(rename = "device_instance")]
    DeviceInstance {
        absolute_address: u64,
        device_name: String,
        instance_name: String,
        src_location: SourceLocation,
    },
}

#[derive(Debug, Clone, Deserialize)]
pub struct InterconnectComponent {
    pub relative_address: u64,
    pub size: u64,
    pub tree: MemoryMapTree,
}

#[derive(Debug, Clone, Deserialize)]
pub struct RegisterDesc {
    pub name: String,
    pub description: String,
    pub reset: Option<u64>,
    pub access: RegisterAccess,
    pub address: u64,
    pub size: u64,
    #[serde(rename = "type")]
    pub reg_type: Type,
    pub src_location: SourceLocation,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RegisterAccess {
    ReadWrite,
    ReadOnly,
    WriteOnly,
}

#[derive(Debug, Clone, Deserialize)]
pub struct TypeMeta {
    pub module: String,
    pub package: String,
    pub is_newtype: bool,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(try_from = "Value")]
pub enum Type {
    Bool,
    BitVector(u64),
    Signed(u64),
    Unsigned(u64),
    Vec(u64, Box<Type>),
    Reference(String, Vec<Type>),
    Variable(u64),
    SumOfProducts {
        name: String,
        meta: TypeMeta,
        variants: Vec<VariantDesc>,
    },
}

impl TryFrom<Value> for Type {
    type Error = String;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value.clone() {
            Value::String(x) if x == "bool" => Ok(Self::Bool),
            Value::Array(a) if a.len() == 2 => {
                let Ok([a, b]) = <[Value; 2]>::try_from(a) else { panic!() };
                match (a.as_str(), b.as_u64()) {
                    (Some("bitvector"), Some(n)) => Ok(Self::BitVector(n)),
                    (Some("signed"), Some(n)) => Ok(Self::Signed(n)),
                    (Some("unsigned"), Some(n)) => Ok(Self::Unsigned(n)),
                    (Some("variable"), Some(n)) => Ok(Self::Variable(n)),
                    _ => Err(format!("Invalid type, found [{a:?}, {b:?}]")),
                }
            }
            Value::Array(a) if a.len() == 3 => {
                let Ok([a, b, c]) = <[Value; 3]>::try_from(a) else { panic!() };
                match a.as_str() {
                    Some("vector") => {
                        let Some(n) = b.as_u64() else {
                            return Err(format!("Invalid type, expected [\"vector\", <integer>, <another type>], found {value:?}"))
                        };
                        let inner = c.try_into()?;
                        Ok(Self::Vec(n, Box::new(inner)))
                    }
                    Some("reference") => {
                        let Some(name) = b.as_str() else {
                            return Err(format!("Invalid type, expected [\"reference\", <name>, <array of type>], found {value:?}"))
                        };
                        let args = match serde_json::from_value(c) {
                            Ok(val) => val,
                            Err(err) => {
                                return Err(format!(
                                    "error when parsing type reference arguments: {err}"
                                ))
                            }
                        };
                        Ok(Self::Reference(name.to_string(), args))
                    }
                    _ => Err(format!("Invalid type, found [{a:?}, {b:?}, {c:?}]")),
                }
            }
            x @ Value::Object(_) => {
                #[derive(Debug, Clone, Deserialize)]
                pub struct SopDefintion {
                    pub name: String,
                    pub meta: TypeMeta,
                    pub variants: Vec<VariantDesc>,
                }

                match serde_json::from_value::<SopDefintion>(x) {
                    Ok(def) => Ok(Self::SumOfProducts {
                        name: def.name,
                        meta: def.meta,
                        variants: def.variants,
                    }),
                    Err(err) => Err(format!("Invalid sum-of-products type definition: {err}")),
                }
            }
            x => Err(format!("Invalid type, found {x:?}")),
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct VariantDesc {
    pub name: String,
    pub fields: Vec<VariantFieldDesc>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct VariantFieldDesc {
    pub name: String,
    #[serde(rename = "type")]
    pub type_: Type,
}

#[derive(Debug, Clone, Deserialize)]
pub struct SourceLocation {
    pub file: String,
    pub module: String,
    pub start_line: u64,
    pub end_line: u64,
    pub start_col: u64,
    pub end_col: u64,
}

/// Parse a memory map description from JSON.
pub fn parse(src: &str) -> Result<MemoryMapDesc, serde_json::Error> {
    serde_json::from_str(src)
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_SRC: &'static str = r#"
{
  "devices": {
    "StatusReg": {
      "description": "",
      "name": "StatusReg",
      "registers": [],
      "src_location": {
        "end_col": 44,
        "end_line": 75,
        "file": "src/Bittide/Instances/Hitl/VexRiscv.hs",
        "module": "Bittide.Instances.Hitl.VexRiscv",
        "package": "bittide-instances-0.1-inplace",
        "start_col": 34,
        "start_line": 75
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
        "end_line": 72,
        "file": "src/Bittide/Instances/Hitl/VexRiscv.hs",
        "module": "Bittide.Instances.Hitl.VexRiscv",
        "package": "bittide-instances-0.1-inplace",
        "start_col": 21,
        "start_line": 72
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
        "end_line": 74,
        "file": "src/Bittide/Instances/Hitl/VexRiscv.hs",
        "module": "Bittide.Instances.Hitl.VexRiscv",
        "package": "bittide-instances-0.1-inplace",
        "start_col": 6,
        "start_line": 74
      }
    }
  },
  "types": {},
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
                "end_line": 69,
                "file": "src/Bittide/Instances/Hitl/VexRiscv.hs",
                "module": "Bittide.Instances.Hitl.VexRiscv",
                "package": "bittide-instances-0.1-inplace",
                "start_col": 44,
                "start_line": 69
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
  }
}

"#;

    #[test]
    fn register_access() {
        let src = "\"read_only\"";
        let x: RegisterAccess = serde_json::from_str(src).unwrap();
        assert_eq!(x, RegisterAccess::ReadOnly);
    }

    #[test]
    fn test_deserialise_memmap() {
        let memmap: MemoryMapDesc = serde_json::from_str(TEST_SRC).unwrap();

        dbg!(&memmap);
    }
}
