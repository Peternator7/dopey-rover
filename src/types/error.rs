pub enum CompilationError {
    SymbolNotFound { module: String, symbol: String },
    TraitNotFound { module: String, trait_name: String },
    SubModuleNotFound { module: String, sub_module: String },
}
