# phpls-rs

phpls-rs is a PHP language server written in Rust.

## Installation

Currently it is only possible to install the development version manually. In the future there will of course be an installable VSCode extension in the
VSCode marketplace.

```bash
# Clone the repository
git clone https://github.com/sawmurai/phpls-rs.git
cd phpls-rs

# Build the language server binary
cargo build --release

# The binary is now here, you will need the path to it later
ls `pwd`/target/release/phpls-rs

# Build the VSCode extension
cd client
yarn
yarn build
```

Clone the phpstorm stubs
```bash
git clone https://github.com/JetBrains/phpstorm-stubs.git
```

Install the `client/phpls-rs-client-0.0.1.vsix` file manually in VSCode:

1. Open VSCode
2. Open the extension menu
3. Click `...` and select "Install from VSIX
4. Open the extension config (search for phpls)
5. Put the path to the binary (see instructions above) into the "binary" field
6. Put the path to the PHPStorm stubs into the "PHP-stubs" field

### Known issues

This is an early development version! Do not use this in production yet (unless you also like to live dangerously ;) ). You might encounter high CPU usage which usually means that something is running in an infinite loop or, low CPU usage but no more response ... that menas we are looking a deadlock.

Should you be able to isolate the problem I would very much appreciate an issue here :)