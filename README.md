# Dead Simple Todos (dst)

A tiny command-line todo list in OCaml. Todos are stored as tab-separated lines in `~/.todo.txt`.

## Build

```sh
dune build
```

## Usage

The executable is installed as `dst`:

```sh
dst                     # List existing todos
dst add "buy milk"      # Add a new todo
dst done 1              # Mark a todo as completed
dst clear               # Delete all completed todos
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
