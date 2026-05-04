open Dead_simple_todos

let usage () =
  Printf.printf "%sUsage:%s\n" Colors.bold Colors.reset;
  print_endline "  dst  # List tasks";
  print_endline "  dst add \"task text\"  # Adds a new task";
  print_endline "  dst done <number>  # Mark a task as completed";
  print_endline "  dst delete <number>  # Deletes a task";
  print_endline "  dst clear  # Deletes all completed tasks"

let () =
  match Array.to_list Sys.argv with
  | [ _ ] -> Todo.list (Todo.load ())
  | [ _; "--help" ] -> usage ()
  | [ _; "add"; text ] -> Todo.add text
  | [ _; "done"; n ] -> (
      match int_of_string_opt n with
      | Some n -> Todo.mark_done n
      | None -> usage ())
  | [ _; "delete"; n ] -> (
      match int_of_string_opt n with
      | Some n -> Todo.delete n
      | None -> usage ())
  | [ _; "clear" ] -> Todo.clear ()
  | _ -> usage ()
