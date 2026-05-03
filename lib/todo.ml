(* A simple todo object. *)
type t = {
  text : string;
  completed : bool;
}

(* Convert todo type to text representation. *)
let serialize todo =
  let status = if todo.completed then "1" else "0" in
  status ^ "\t" ^ todo.text

(* Convert text representation to todo type. *)
let deserialize line =
  match String.split_on_char '\t' line with
  | [ status; text ] -> { completed = status = "1"; text }
  | _ -> { completed = false; text = line }
