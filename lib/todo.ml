(* A simple todo object *)
type t = {
  text : string;
  completed : bool;
}

(* File on disk that holds the todos. *)
let file = "/Users/niru/.todo.txt"

(* Convert todo type to text representation. *)
let serialize todo =
  let status = if todo.completed then "1" else "0" in
  status ^ "\t" ^ todo.text

(* Convert text representation to todo type. *)
let deserialize line =
  match String.split_on_char '\t' line with
    | [ status; text ] -> { completed = status = "1"; text }
    | _ -> { completed = false; text = line }

(* Load todos from file. *)
let load () =
  if Sys.file_exists file then
    In_channel.with_open_text file In_channel.input_lines
    |> List.map deserialize
  else
    []

(* Write todo to the file. *)
let save todos =
  let contents =
    todos
    |> List.map serialize
    |> String.concat "\n"
  in
  Out_channel.with_open_text file (fun oc ->
    output_string oc contents;
    output_string oc "\n")

(* Add todo to the file. *)
let add text =
  let todos = load () in
  save (todos @ [ { text; completed = false } ]);
  Printf.printf "Added: %s\n" text

let print index todo =
  let mark = if todo.completed then "x" else " " in
  Printf.printf "%d. [%s] %s\n" index mark todo.text

let list todos =
  todos
  |> List.iteri (fun i todo -> print (i + 1) todo)

let mark_done n =
  let todos = load () in
  let updated =
    todos
    |> List.mapi (fun i todo ->
      if i + 1 = n then { todo with completed = true } else todo)
  in
  save updated;
  Printf.printf "Marked #%d done\n" n
