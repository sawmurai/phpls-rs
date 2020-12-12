rm -rf ./target *.prof*

# Export the flags needed to instrument the program to collect code coverage.
export RUSTFLAGS="-Zinstrument-coverage"

# Build the program
cargo build

# Run the program (you can replace this with `cargo test` if you want to collect code coverage for your tests).
cargo test

# Generate a HTML report in the coverage/ directory.
grcov . --binary-path ./target/debug/ -s . -t html --branch --ignore-not-existing -o ./target/debug/coverage/
