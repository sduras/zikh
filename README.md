# zikh

Non-destructive photo organizer. No external dependencies.

Discovers photos in a source directory, extracts capture timestamps from
embedded EXIF metadata, and moves them into a date-organized hierarchy.
No file is moved or deleted until a complete plan has been validated.
Running zikh twice produces no changes on the second run.

## What it does

```
zikh plan ~/Pictures ~/archive
```

```
plan: 34 moves, 0 unparsed, 0 skips, 0 errors

  2009  ████████████████████  12
  2010  █████████████████     10
  2011  ██                     1
  2012  █████                  3
  2013  █████████████          8

```

To see the full per-file operation list:

```
zikh plan -v ~/Pictures ~/archive
```

If the plan looks correct:

```
zikh execute ~/Pictures ~/archive
```

## Guarantees

- No existing file is overwritten
- Source files are removed only after the destination is confirmed written
- EXIF metadata is never modified
- A second run on the same source produces no changes

## Requirements

- Destination filesystem must support hard links (ext4, APFS, UFS; not FAT/exFAT)

## Install

### Linux (x86_64)

```sh
curl -L https://codeberg.org/duras/zikh/releases/latest/download/zikh-linux-amd64 \
  -o ~/.local/bin/zikh && chmod +x ~/.local/bin/zikh
```

Or with wget:

```sh
wget -O ~/.local/bin/zikh \
  https://codeberg.org/duras/zikh/releases/latest/download/zikh-linux-amd64 \
  && chmod +x ~/.local/bin/zikh
```

`~/.local/bin` must be in your `PATH`. On most Linux distributions it is added
automatically if the directory exists. If not: `export PATH="$HOME/.local/bin:$PATH"`.

### Linux (ARM64)

```sh
curl -L https://codeberg.org/duras/zikh/releases/latest/download/zikh-linux-arm64 \
  -o ~/.local/bin/zikh && chmod +x ~/.local/bin/zikh
```

### macOS (Apple Silicon)

```sh
curl -L https://codeberg.org/duras/zikh/releases/latest/download/zikh-macos-arm64 \
  -o /usr/local/bin/zikh && chmod +x /usr/local/bin/zikh
```

### macOS (Intel)

```sh
curl -L https://codeberg.org/duras/zikh/releases/latest/download/zikh-macos-amd64 \
  -o /usr/local/bin/zikh && chmod +x /usr/local/bin/zikh
```

### Build from source

```sh
opam install dune
dune build
dune install
```

## Usage

```
zikh plan    [-v] [--json] source dest   # plan only, no changes
zikh execute [-v] [--json] source dest   # plan and apply
```

| Flag     | Effect |
|----------|--------|
| `-v`     | `plan`: show full per-file list. `execute`: print each operation as it runs |
| `--json` | Write NDJSON to stdout |

Exit codes: `0` success · `1` partial · `2` validation failure, no changes made · `3` all failed · `64` bad arguments · `71` OS error

## Documentation

```sh
man zikh
```

The man page is the authoritative reference for all options, output formats,
exit codes, environment variables, and file naming rules.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## License

ISC — see [LICENSE](LICENSE)
