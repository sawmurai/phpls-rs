workspace "phpls-rs" "Documentation of the architecture of phpls-rs" {
    model {
        developer = person "The developer using the editor that uses phpls-rs"
    
        editor = softwaresystem "Editor" "Any text editor that implements a language server client"

        phplsRs = softwaresystem "phpls-rs" "The language server implementation for PHP" {

            parser = container "PHP Parser" "The parser component that turns a source file into an AST. It contains of a parser and a scanner." {
                scanner = component "Token Scanner" "The token scanner turns a source file into a stream of Tokens"
                parserParser = component "Parser" "The actual parser that turns a stream of tokens into an AST"
            }

            formatter = container "Formatter" "The formatter uses the AST and the token stream to reformat a given source file."

            environment = container "Environment" "The environment is aware of the available symbols in the code base." {
                NameResolver = component "NameResolver" "The NameResolver resolves an individual symbol to its definition"
                NameResolveVisitor = component "NameResolveVisitor" "The NameResolveVisitor resolves references to symbols by walking the AST recursively"
                WorkspaceSymbolVisitor = component "WorkspaceSymbolVisitor" "The WorkspaceSymbolVisitor walks the AST recursivly and collects symbols that are relevant from an outside-scope"
            }

            backend = container "Backend" "The backend contains handlers for the individual language server features" {
                source_to_ast = component "source_to_ast" "Turns a source string into an AST"
                initialize = component "initialize" "Initializes a workspace"
            }

            backend -> environment "Retrieve information about symbols and their references to each other"
            backend -> parser "Turn source files into ASTs"
            backend -> formatter "Turn ASTs (+ TokenStreams) back into source strings"
        }

        developer -> editor "Write and read code"
        editor -> phplsRs "Get semantic information about the code"
    }

    views {
        systemlandscape "systemlandscape" {
            include *
            autoLayout
        }

        container phplsRs "containers" {
            include *
        }

        component parser "php-parser-components" {
            include *
        }

        component backend "backend-components" {
            include *
        }

        component environment "environment-components" {
            include *
        }

        dynamic parser "backend-process-source_to_ast" "Shows how the parser returns an AST from a source file" {
            source_to_ast -> scanner "Calls to return a token stream from a source string"
            source_to_ast -> parserParser "Calls to return an AST from a token stream"
        }

        dynamic backend "backend-process-initialize" "Boots the language server" {
            initialize -> parser "Instructs to parse a file and return its AST"
            initialize -> environment "Instructs to find symbols"
        }
    }
}