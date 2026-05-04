(* A simple todo object. *)
type t = {
  text : string;
  completed : bool;
}

(* File on disk that holds the todos. *)
let home_dir () =
  match Sys.getenv_opt "HOME" with
  | Some home -> home
  | None -> failwith "HOME is not set"
let file = Filename.concat (home_dir ()) ".todo.txt"

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
  Printf.printf "%sAdded:%s %s%s%s\n"
    Colors.green Colors.reset Colors.blue text Colors.reset

let print index todo =
  if todo.completed then
    Printf.printf "%s%s%d.%s %s[%sx%s] %s%s\n"
      Colors.dim Colors.cyan index Colors.reset Colors.dim Colors.green Colors.dim todo.text Colors.reset
  else
    Printf.printf "%s%d.%s [ ] %s\n" Colors.cyan index Colors.reset todo.text

let delete n =
  let todos = load () in
  let updated = todos |> List.filteri (fun i _ -> i + 1 <> n) in
  save updated;
  Printf.printf "Deleted #%d\n" n

let clear () =
  let todos = load () in
  let remaining = todos |> List.filter (fun t -> not t.completed) in
  let cleared = List.length todos - List.length remaining in
  save remaining;
  Printf.printf "Cleared %s%d%s completed task%s\n"
    Colors.green cleared Colors.reset
    (if cleared = 1 then "" else "s")

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
  match List.nth_opt todos (n - 1) with
  | Some todo ->
    Printf.printf "Marked %s#%d: %s%s %sdone%s\n"
      Colors.blue n todo.text Colors.reset Colors.green Colors.reset
  | None ->
    Printf.printf "Marked %s#%d%s %sdone%s\n"
      Colors.blue n Colors.reset Colors.green Colors.reset
