let usage () =
  print_endline "Usage:";
  print_endline "  todo add \"task text\"";
  print_endline "  todo list";
  print_endline "  todo done <number>"

let () =
  match Array.to_list Sys.argv with
  | [ _; "add"; text ] -> Dead_simple_todos.Todo.add text
  | [ _; "list" ] -> Dead_simple_todos.Todo.list (Dead_simple_todos.Todo.load ())
  | [ _; "done"; n ] -> (
      match int_of_string_opt n with
      | Some n -> Dead_simple_todos.Todo.mark_done n
      | None -> usage ())
  | _ -> usage ()
