#![allow(dead_code)]

pub mod analysis;
pub mod codegen;
pub mod parsing;
pub mod types;

use std::collections::HashMap;

pub struct Module {
    pub name: String,
    functions: HashMap<String, Executable>,
    init_code: Executable,
    static_variables: HashMap<String, Data>,
}

pub type Executable = Vec<codegen::opcode::Opcode>;

pub enum Data {}
