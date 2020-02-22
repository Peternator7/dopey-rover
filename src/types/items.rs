use super::{Assignment, BasicPattern, ImportStatement};
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "type")]
pub enum Item {
    Assignment(Assignment<BasicPattern>),
    TraitDeclaration(TraitDeclaration),
    ModuleImport(ImportStatement),
}

#[derive(Debug, Clone, Serialize)]
pub struct TraitDeclaration {
    pub name: String,
    pub items: Vec<TraitItem>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize)]
#[serde(tag = "type")]
pub enum TraitItem {
    Property { name: String },
    Function { name: String, args: usize },
}
