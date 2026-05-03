open Dead_simple_todos

(* File on disk that holds the todos. *)
let file = "/Users/niru/.todo.txt"

(* Load todos from file. *)
let load_todos () =
  if Sys.file_exists file then
    In_channel.with_open_text file In_channel.input_lines
    |> List.map Todo.deserialize
  else
    []

(* Write todo to the file. *)
let save_todos todos =
  let contents =
    todos
    |> List.map Todo.serialize
    |> String.concat "\n"
  in
  Out_channel.with_open_text file (fun oc ->
    output_string oc contents;
    output_string oc "\n")

(* Add todo to the file. *)
let add_todo text =
  let todos = load_todos () in
  save_todos (todos @ [ { Todo.text; completed = false } ]);
  Printf.printf "Added: %s\n" text

let print_todo index (todo : Todo.t) =
  let mark = if todo.completed then "x" else " " in
  Printf.printf "%d. [%s] %s\n" index mark todo.text

let list_todos todos =
  todos
  |> List.iteri (fun i todo -> print_todo (i + 1) todo)
  let mark_done n =
  let todos = load_todos () in
  let updated =
    todos
    |> List.mapi (fun i (todo : Todo.t) ->
      if i + 1 = n then { todo with completed = true } else todo)
  in
  save_todos updated;
  Printf.printf "Marked #%d done\n" n

let usage () =
  print_endline "Usage:";
  print_endline "  todo add \"task text\"";
  print_endline "  todo list";
  print_endline "  todo done <number>"

let () =
  match Array.to_list Sys.argv with
  | [ _; "add"; text ] -> add_todo text
  | [ _; "list" ] -> list_todos (load_todos ())
  | [ _; "done"; n ] -> (
      match int_of_string_opt n with
      | Some n -> mark_done n
      | None -> usage ())
  | _ -> usage ()
