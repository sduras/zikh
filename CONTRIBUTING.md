# Contributing to zikh

## Development setup

**Requirements:**
- OCaml ≥ 5.2
- opam
- dune (installed via opam)

**Build:**
```sh
opam install dune
dune build
```

**Run tests:**
```sh
dune test
```

**Run locally:**
```sh
dune exec -- zikh --help
```

## Before submitting a patch

- Keep changes focused. One concern per patch.
- Update documentation when behavior changes. The man page is the authoritative
  reference; if CLI behavior changes, `man/zikh.1` must change in the same commit.
- Add or update tests when applicable. Pure logic changes belong in the unit
  tests (`tests/test_planning.ml`, `tests/test_validation.ml`); filesystem
  behavior belongs in the integration tests (`tests/test_integration.ml`).
- Ensure `dune build` and `dune test` both succeed before submitting.

## Submitting changes

1. Fork the repository on [Codeberg](https://codeberg.org/duras/zikh).
2. Create a topic branch.
3. Commit your changes with a clear message.
4. Open a pull request against `main`.

For larger changes, open an issue first to discuss the approach.

## Project goals

- Small and understandable codebase.
- No external runtime dependencies.
- Predictable, auditable CLI behavior.
- Portable Unix-first implementation.

## Non-goals

These are deliberate exclusions, not gaps. Patches adding them will not be accepted:

- Photo editing or pixel manipulation.
- Writing or modifying EXIF metadata.
- RAW format support (`.cr2`, `.nef`, `.arw`, and similar).
- Video file support.
- GUI.
- Cloud synchronization.
- Persistent state or database.
- Duplicate detection.
