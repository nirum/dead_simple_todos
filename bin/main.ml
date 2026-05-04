let usage () =
  print_endline "Usage:";
  print_endline "  dst add \"task text\"";
  print_endline "  dst list";
  print_endline "  dst done <number>"

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
  | [ _; "clear" ] -> clear_completed ()
  | _ -> usage ()
