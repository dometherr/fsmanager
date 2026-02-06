## File System Manager

This a project to consolidate Haskell knowledge by creating a
in-memory File System Manager purely functional using (attempted)
advanced Haskell features

## Running

This project can be run in a few commands if you have Nix and direnv
installed in your machine. Follow the steps below to get up and running:

### Setup Environment

```sh
$ direnv allow
# ^ this command should update your terminal with the dependencies in flake.nix

$ type ghc
# ^ this command should return the path to the nix store where ghc is installed

$ type cabal
# ^ similarly this command should return the path to the nix store where cabal is installed
```

### Building and Running

Once everything is installed and working, just build the project using cabal and
run it.

```sh
$ cabal build fsmanager

$ cabal run fsmanager
# Starting fsmanager...
# $/
```

From there on you can run `help` in your terminal to list the available commands
