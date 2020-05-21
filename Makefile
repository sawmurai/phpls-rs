test-coverage: export CARGO_INCREMENTAL = 0
test-coverage: export RUSTFLAGS = -Zprofile -Ccodegen-units=1 -Copt-level=0 -Clink-dead-code -Coverflow-checks=off -Zpanic_abort_tests -Cpanic=abort
test-coverage: export RUSTDOCFLAGS = -Cpanic=abort
test-coverage:
	cargo clean && cargo build && cargo test && \
	grcov ./target/debug/ -s . -t html --llvm --branch --ignore-not-existing -o ./target/debug/coverage/