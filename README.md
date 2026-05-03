# dead_simple_todos

A tiny command-line todo list in OCaml. Todos are stored as tab-separated lines in `~/.todo.txt`.

## Build

```sh
dune build
```

## Usage

The executable is installed as `dst`:

```sh
dst add "buy milk"
dst list
dst done 1
```

Or run without installing:

```sh
dune exec dst -- add "buy milk"
dune exec dst -- list
dune exec dst -- done 1
```

## Test

```sh
dune test
```
