use super::backend::NodeMapMutex;

pub fn suggest(source: &str, line: u64, col: u64, global_symbols: NodeMapMutex) -> Vec<String> {
    vec![String::from("Hello")]
}

#[cfg(test)]
mod tests {
    use super::super::backend::NodeMapMutex;
    use super::super::parser::scanner::Scanner;
    use super::super::parser::Parser;
    use super::suggest;

    #[test]
    fn test_suggest_global_symbol_for_start_of_new_token() {
        assert!(false)
    }
}
