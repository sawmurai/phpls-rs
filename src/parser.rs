struct Parser {

}

impl Parser {

    // Returns the correct TokenType for a keyword in a cast context
    fn map_cast(&self, ident: &str) -> TokenType {
        match ident {
            "bool" | "boolean" => TokenType::BoolCast,
            "int" | "integer" => TokenType::IntCast,
            "string" | "binary" => TokenType::StringCast,
            "array" => TokenType::ArrayCast,
            "object" => TokenType::ObjectCast,
            "unset" => TokenType::UnsetCast,
            "double" | "float" | "real" => TokenType::DoubleCast,
            _ => TokenType::BadCast,
        }
    }
}