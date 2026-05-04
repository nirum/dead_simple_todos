(* ANSI color escape codes. *)
let reset = "\027[0m"
let green = "\027[32m"
let dim = "\027[2m"
let bold = "\027[1m"
let cyan = "\027[36m"

(* A simple todo object *)
type todo = {
  text : string;
  completed : bool;
}

(* File on disk that holds the todos. *)
let file = "/Users/niru/.todo.txt"

(* Convert todo type to text representation. *)
let serialize_todo todo =
  let status = if todo.completed then "1" else "0" in
  status ^ "\t" ^ todo.text

(* Convert text representation to todo type. *)
let deserialize_todo line =
  match String.split_on_char '\t' line with
    | [ status; text ] -> { completed = status = "1"; text }
    | _ -> { completed = false; text = line }

(* Load todos from file. *)
let load_todos () =
  if Sys.file_exists file then
    In_channel.with_open_text file In_channel.input_lines
    |> List.map deserialize_todo
  else
    []

(* Write todo to the file. *)
let save_todos todos =
  let contents =
    todos
    |> List.map serialize_todo
    |> String.concat "\n"
  in
  Out_channel.with_open_text file (fun oc ->
    output_string oc contents;
    output_string oc "\n")

(* Add todo to the file. *)
let add_todo text =
  let todos = load_todos () in
  save_todos (todos @ [ { text; completed = false } ]);
  Printf.printf "%sAdded:%s %s\n" green reset text

let print_todo index todo =
  if todo.completed then
    Printf.printf "%s%s%d.%s %s[%sx%s] %s%s\n"
      dim cyan index reset dim green dim todo.text reset
  else
    Printf.printf "%s%d.%s [ ] %s\n" cyan index reset todo.text

let list_todos todos =
  todos
  |> List.iteri (fun i todo -> print_todo (i + 1) todo)

let mark_done n =
  let todos = load_todos () in
  let updated =
    todos
    |> List.mapi (fun i todo ->
      if i + 1 = n then { todo with completed = true } else todo)
  in
  save_todos updated;
  Printf.printf "Marked #%d %sdone%s\n" n green reset

let usage () =
  Printf.printf "%sUsage:%s\n" bold reset;
  print_endline "  todo add \"task text\"";
  print_endline "  todo list";
  print_endline "  todo done <number>"
let delete n =
  let todos = load_todos () in
  let updated = todos |> List.filteri (fun i _ -> i + 1 <> n) in
  save_todos updated;
  Printf.printf "Deleted #%d\n" n

let usage () =
  print_endline "Usage:";
  print_endline "  dst add \"task text\"";
  print_endline "  dst list";
  print_endline "  dst done <number>"
  print_endline "  dst delete <number>"

let () =
  match Array.to_list Sys.argv with
  | [ _ ] -> list_todos (load_todos ())
  | [ _; "--help" ] -> usage ()
  | [ _; "add"; text ] -> add_todo text
  | [ _; "list" ] -> list_todos (load_todos ())
  | [ _; "done"; n ] -> (
      match int_of_string_opt n with
      | Some n -> Dead_simple_todos.Todo.mark_done n
      | None -> usage ())
  | [ _; "delete"; n ] -> (
      match int_of_string_opt n with
      | Some n -> delete n
      | None -> usage ())
  | [ _; "clear" ] -> clear_completed ()
  | _ -> usage ()
