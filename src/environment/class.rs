use tower_lsp::lsp_types::{DocumentSymbol, Range, SymbolKind};

#[derive(Debug, Default)]
pub struct Class {
    pub name: String,

    /// Methods in this class
    pub methods: Vec<Method>,

    /// Constants in this class
    pub constants: Vec<String>,

    /// Properties of the class
    pub properties: Vec<Property>,

    // Range of the class
    pub range: Range,
}

impl Class {
    pub fn new(name: &str, range: Range) -> Self {
        Self {
            name: name.to_owned(),
            range,
            ..Class::default()
        }
    }
}

impl From<&Class> for DocumentSymbol {
    fn from(class: &Class) -> DocumentSymbol {
        DocumentSymbol {
            name: class.name.clone(),
            detail: None,
            kind: SymbolKind::Class,
            deprecated: None,
            range: class.range,
            selection_range: class.range,
            children: Some(
                class
                    .methods
                    .iter()
                    .map(|method| DocumentSymbol::from(method))
                    .chain(
                        class
                            .properties
                            .iter()
                            .map(|property| DocumentSymbol::from(property)),
                    )
                    .collect(),
            ),
        }
    }
}

#[derive(Debug, Default)]
pub struct Method {
    pub name: String,

    // Range of the method
    pub range: Range,
}

impl Method {
    pub fn new(name: &str, range: Range) -> Self {
        Self {
            name: name.to_owned(),
            range,
            ..Method::default()
        }
    }
}

impl From<&Method> for DocumentSymbol {
    fn from(method: &Method) -> DocumentSymbol {
        DocumentSymbol {
            name: method.name.clone(),
            detail: None,
            kind: SymbolKind::Method,
            deprecated: None,
            range: method.range,
            selection_range: method.range,
            children: None,
        }
    }
}

#[derive(Debug, Default)]
pub struct Property {
    pub name: String,

    // Range of the property
    pub range: Range,
}

impl Property {
    pub fn new(name: &str, range: Range) -> Self {
        Self {
            name: name.to_owned(),
            range,
            ..Property::default()
        }
    }
}

impl From<&Property> for DocumentSymbol {
    fn from(property: &Property) -> DocumentSymbol {
        DocumentSymbol {
            name: property.name.clone(),
            detail: None,
            kind: SymbolKind::Method,
            deprecated: None,
            range: property.range,
            selection_range: property.range,
            children: None,
        }
    }
}
