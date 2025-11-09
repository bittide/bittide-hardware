// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::collections::HashMap;

use derivative::Derivative;
use serde::Deserialize;
use serde_json::Value;

#[derive(Debug, Clone, Deserialize)]
pub struct MemoryMapDesc {
    pub devices: HashMap<String, DeviceDesc>,
    pub types: HashMap<String, TypeDescription>,
    pub tree: MemoryMapTree,
    pub src_locations: Option<Vec<SourceLocation>>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(try_from = "Value")]
pub enum LocationRef {
    Inline(SourceLocation),
    Separate(u64),
}

impl TryFrom<Value> for LocationRef {
    type Error = String;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Some(idx) = value.as_u64() {
            Ok(LocationRef::Separate(idx))
        } else {
            let loc = serde_json::from_value(value).map_err(|err| err.to_string())?;
            Ok(LocationRef::Inline(loc))
        }
    }
}

#[derive(Debug, Clone, Deserialize, Derivative)]
#[derivative(PartialEq, Hash)]
pub struct DeviceDesc {
    pub name: String,
    pub description: String,
    pub registers: Vec<RegisterDesc>,
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub src_location: LocationRef,
    pub tags: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Hash)]
pub struct TypeDescription {
    pub name: TypeName,
    pub type_args: Vec<TypeArg>,
    pub definition: TypeDefinition,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Hash)]
#[serde(try_from = "Value")]
pub enum TypeArg {
    Type { name: String },
    Number { name: String },
}

impl TryFrom<Value> for TypeArg {
    type Error = String;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let obj = value.as_object().ok_or("TypeArg must be a JSON object")?;

        let Some(kind) = obj.get("kind") else {
            return Err("TypeArg must have a \"kind\"".to_string());
        };

        let Some(name) = obj.get("name") else {
            return Err("TypeArg must have a \"name\"".to_string());
        };

        let Some(kind) = kind.as_str() else {
            return Err("TypeArg \"kind\" must be a string".to_string());
        };
        let Some(name) = name.as_str() else {
            return Err("TypeArg \"name\" must be a string".to_string());
        };

        match kind {
            "number" => Ok(Self::Number {
                name: name.to_string(),
            }),
            "type" => Ok(TypeArg::Type {
                name: name.to_string(),
            }),
            _ => Err("TypeArg \"name\" must be either \"number\" or \"type\"".to_string()),
        }
    }
}

#[derive(Debug, Clone, Deserialize, PartialEq, Hash)]
#[serde(try_from = "Value")]
pub enum TypeDefinition {
    DataType(Vec<NamedConstructor>),
    Newtype(NamedConstructor),
    Builtin(BuiltinType),
    Synonym(TypeRef),
}

impl TryFrom<Value> for TypeDefinition {
    type Error = String;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let obj = value
            .as_object()
            .ok_or("TypeDefinition must be a JSON object")?;

        if let Some(cons) = obj.get("datatype") {
            let cons = cons.as_array().unwrap();
            let mut parsed_cons = Vec::new();
            for con in cons {
                parsed_cons.push(NamedConstructor::try_from(con.clone())?);
            }
            if obj.len() != 1 {
                return Err("TypeDefinition::DataType must have exactly one field".into());
            }
            Ok(TypeDefinition::DataType(parsed_cons))
        } else if let Some(con) = obj.get("newtype") {
            let con = NamedConstructor::try_from(con.clone())?;
            if obj.len() != 1 {
                return Err("TypeDefinition::Newtype must have exactly one field".into());
            }
            Ok(TypeDefinition::Newtype(con))
        } else if let Some(builtin) = obj.get("builtin") {
            let b = BuiltinType::try_from(builtin.clone())?;
            if obj.len() != 1 {
                return Err("TypeDefinition::Builtin must have exactly one field".into());
            }
            Ok(TypeDefinition::Builtin(b))
        } else if let Some(val) = obj.get("type_synonym") {
            let ty = TypeRef::try_from(val.clone())?;
            if obj.len() != 1 {
                return Err("TypeDefinition::Synonym must have exactly one field".into());
            }
            Ok(TypeDefinition::Synonym(ty))
        } else {
            Err(
                "Only \"datatype\", \"newtype\", \"builtin\" and \"type_synonym\" are supported"
                    .into(),
            )
        }
    }
}

#[derive(Debug, Clone, Deserialize, PartialEq, Hash)]
#[serde(try_from = "Value")]
pub enum BuiltinType {
    BitVector,
    Vector,
    Bool,
    Float,
    Double,
    Signed,
    Unsigned,
    Index,
}

impl TryFrom<Value> for BuiltinType {
    type Error = String;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let s = value
            .as_str()
            .ok_or_else(|| "Builtin must be a string".to_string())?;

        match s {
            "bitvector" => Ok(BuiltinType::BitVector),
            "vector" => Ok(BuiltinType::Vector),
            "bool" => Ok(BuiltinType::Bool),
            "float" => Ok(BuiltinType::Float),
            "double" => Ok(BuiltinType::Double),
            "signed" => Ok(BuiltinType::Signed),
            "unsigned" => Ok(BuiltinType::Unsigned),
            "index" => Ok(BuiltinType::Index),
            s => Err(format!("Unsupported builin type: {s:}")),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Constructor {
    Nameless { fields: Vec<TypeRef> },
    Record { fields: Vec<(String, TypeRef)> },
}

#[derive(Debug, Clone, Deserialize, PartialEq, Hash)]
#[serde(try_from = "Value")]
pub struct NamedConstructor(pub String, pub Constructor);

impl TryFrom<Value> for NamedConstructor {
    type Error = String;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let obj = value
            .as_object()
            .ok_or_else(|| "constructor must be an object".to_string())?;

        let Some(name) = obj.get("name") else {
            return Err("Constructor must have a \"name\" field".to_string());
        };

        let name = name
            .as_str()
            .expect("\"name\" must be a string")
            .to_string();

        if obj.len() != 2 {
            return Err("Constructor must have exactly two fields".into());
        }

        if let Some(fields) = obj.get("nameless") {
            let arr = fields.as_array().unwrap();
            let mut parsed_fields = Vec::new();
            for val in arr {
                parsed_fields.push(TypeRef::try_from(val.clone())?);
            }
            Ok(NamedConstructor(
                name,
                Constructor::Nameless {
                    fields: parsed_fields,
                },
            ))
        } else if let Some(fields) = obj.get("record") {
            let arr = fields.as_array().unwrap();
            let mut parsed_fields = Vec::new();
            for val in arr {
                let field_obj = val.as_object().unwrap();
                let name = field_obj.get("fieldname").unwrap().as_str().unwrap();
                let ty_raw = field_obj.get("type").unwrap();
                let ty = TypeRef::try_from(ty_raw.clone())?;
                parsed_fields.push((name.to_string(), ty));
            }
            Ok(NamedConstructor(
                name,
                Constructor::Record {
                    fields: parsed_fields,
                },
            ))
        } else {
            Err("Constructor must be \"nameless\" or \"record\"".into())
        }
    }
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(try_from = "Value")]
pub enum TypeRef {
    TypeReference { base: TypeName, args: Vec<TypeRef> },
    Variable(String),
    Nat(u64),
    Tuple(Vec<TypeRef>),
}

impl TryFrom<Value> for TypeRef {
    type Error = String;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let obj = value.as_object().ok_or("TypeRef must be a JSON object")?;

        if let Some(reference) = obj.get("type_reference") {
            let name = TypeName::try_from(reference.clone())?;
            let args = obj.get("args").unwrap().as_array().unwrap();

            if obj.len() != 2 {
                return Err("TypeRef::TypeReference must have exactly two fields".into());
            }

            let mut parsed_args = Vec::new();
            for val in args {
                parsed_args.push(TypeRef::try_from(val.clone())?);
            }
            Ok(TypeRef::TypeReference {
                base: name,
                args: parsed_args,
            })
        } else if let Some(var) = obj.get("variable") {
            let name = var.as_str().unwrap().to_string();
            if obj.len() != 1 {
                return Err("TypeRef::Variable must have exactly one field".into());
            }
            Ok(TypeRef::Variable(name))
        } else if let Some(val) = obj.get("nat") {
            let num = val.as_u64().unwrap();
            if obj.len() != 1 {
                return Err("TypeRef::Nat must have exactly one field".into());
            }
            Ok(TypeRef::Nat(num))
        } else if let Some(args) = obj.get("tuple") {
            let mut parsed_args = Vec::new();
            for arg in args.as_array().unwrap() {
                parsed_args.push(TypeRef::try_from(arg.clone())?);
            }

            if obj.len() != 1 {
                return Err("TypeRef::Tuple must have exactly one array field".into());
            }

            Ok(TypeRef::Tuple(parsed_args))
        } else {
            todo!()
        }
    }
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[serde(try_from = "Value")]
pub struct TypeName {
    pub base: String,
    pub module: String,
    pub package: String,
}

impl TryFrom<Value> for TypeName {
    type Error = String;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let obj = value.as_object().ok_or("TypeName must be a JSON object")?;

        let base = obj
            .get("name_base")
            .ok_or("TypeName must have field \"name_base\"")?
            .as_str()
            .unwrap()
            .to_string();

        let module = obj
            .get("name_module")
            .ok_or("TypeName must have field \"name_module\"")?
            .as_str()
            .unwrap()
            .to_string();

        let package = obj
            .get("name_package")
            .ok_or("TypeName must have field \"name_package\"")?
            .as_str()
            .unwrap()
            .to_string();

        if obj.len() != 3 {
            return Err("TypeName must have exactly three fields".into());
        }

        Ok(TypeName {
            base,
            module,
            package,
        })
    }
}

pub type Path = Vec<PathComp>;

pub fn path_name(path: &Path) -> Option<(LocationRef, &str)> {
    path.last().and_then(|comp| {
        if let PathComp::Name { src_location, name } = comp {
            Some((src_location.clone(), name.as_str()))
        } else {
            None
        }
    })
}

#[derive(Debug, Clone, Deserialize, Derivative)]
#[serde(try_from = "Value")]
#[derivative(PartialEq, Hash)]
pub enum PathComp {
    Name {
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        src_location: LocationRef,
        name: String,
    },
    Unnamed(u64),
}

impl TryFrom<Value> for PathComp {
    type Error = String;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(number) => Ok(PathComp::Unnamed(number.as_u64().unwrap())),
            x @ Value::Object(_) => {
                #[derive(Debug, Clone, Deserialize)]
                pub struct NamedPath {
                    pub name: String,
                    pub src_location: LocationRef,
                }

                match serde_json::from_value::<NamedPath>(x) {
                    Ok(def) => Ok(PathComp::Name {
                        src_location: def.src_location,
                        name: def.name,
                    }),
                    Err(err) => Err(format!("Invalid named path component: {err}")),
                }
            }
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone, Deserialize, Derivative)]
#[derivative(PartialEq, Hash)]
pub struct Tag {
    pub tag: String,
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub src_location: LocationRef,
}

#[derive(Debug, Clone, Deserialize, Derivative)]
#[derivative(PartialEq, Hash)]
pub enum MemoryMapTree {
    #[serde(rename = "interconnect")]
    Interconnect {
        path: Path,
        tags: Vec<Tag>,
        absolute_address: u64,
        components: Vec<InterconnectComponent>,
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        src_location: LocationRef,
    },
    #[serde(rename = "device_instance")]
    DeviceInstance {
        path: Path,
        tags: Vec<Tag>,
        device_name: String,
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        src_location: LocationRef,
        absolute_address: u64,
    },
}

#[derive(Debug, Clone, Deserialize, PartialEq, Hash)]
pub struct InterconnectComponent {
    pub relative_address: u64,
    pub tree: MemoryMapTree,
}

#[derive(Debug, Clone, Deserialize, Derivative)]
#[derivative(PartialEq, Hash)]
pub struct RegisterDesc {
    pub name: String,
    pub description: String,
    // TODO: There is a field called reset in the JSON, but we don't use it yet.
    //       It is commented out for now, because:
    //
    //         1. Its type is wrong: registers can be much larger than u64
    //         2. The JSON parser interprets "large" numbers (correctly) as
    //            floating point numbers, which is not what we want.
    //
    // pub reset: Option<u64>,
    pub access: RegisterAccess,
    pub address: u64,
    pub size: u64,
    #[serde(rename = "type")]
    pub reg_type: TypeRef,
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub src_location: LocationRef,
    pub tags: Vec<String>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
#[serde(rename_all = "snake_case")]
pub enum RegisterAccess {
    ReadWrite,
    ReadOnly,
    WriteOnly,
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
    use std::path::PathBuf;

    use super::*;

    fn memmap_dir() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("_build")
            .join("memory_maps")
            .into()
    }

    #[test]
    fn register_access() {
        let src = "\"read_only\"";
        let x: RegisterAccess = serde_json::from_str(src).unwrap();
        assert_eq!(x, RegisterAccess::ReadOnly);
    }

    #[test]
    fn test_deserialise_memmap() {
        let path = memmap_dir().join("VexRiscv.json");

        let source = std::fs::read_to_string(&path).unwrap();
        let _memmap: MemoryMapDesc = serde_json::from_str(&source).unwrap();
    }
}
