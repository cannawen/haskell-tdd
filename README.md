# haskell-tdd

A minimal Haskell project set up for test-driven development using [Stack](https://docs.haskellstack.org/).

## Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) — Haskell build tool and package manager
- hpack

## Project structure

```
app/
  Main.hs     # Executable entry point
src/
  MiscFiles.hs
test/
  Spec.hs     # Test suite
```

## Commands

**Build**
```sh
stack build
```

**Run the executable**
```sh
stack run
```

**Run in REPL**
```sh
stack ghci src/File.hs
```

**Run tests**
```sh
stack test
```

**Watch for changes and rebuild continuously**
```sh
stack build --test --no-run-tests --file-watch
```

**Clean and rebuild**
```sh
stack clean && stack build
```

## VS Code integration

The project includes VS Code tasks (`.vscode/tasks.json`) with keyboard shortcuts:

| Key | Task |
|-----|------|
| F7  | Build |
| F8  | Run tests |

Install the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) for editor support (syntax highlighting, type hints, go-to-definition).

## Adding code

1. Write a failing test in [test/Spec.hs](test/Spec.hs)
2. Run `stack test` to confirm it fails
3. Implement the function in [src/Lib.hs](src/Lib.hs) and export it
4. Run `stack test` again to confirm it passes
