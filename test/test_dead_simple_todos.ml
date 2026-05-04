open Dead_simple_todos

(* Custom Alcotest testable for Todo.t so failures pretty-print and
   equality is structural. *)
let todo_testable =
  let pp fmt (t : Todo.t) =
    Format.fprintf fmt "{ completed = %b; text = %S }" t.completed t.text
  in
  let eq (a : Todo.t) (b : Todo.t) =
    a.completed = b.completed && a.text = b.text
  in
  Alcotest.testable pp eq

let test_round_trip_completed () =
  let todo = { Todo.text = "buy milk"; completed = true } in
  Alcotest.check todo_testable
    "round-trip preserves completed=true"
    todo
    (Todo.deserialize (Todo.serialize todo))

let test_round_trip_not_completed () =
  let todo = { Todo.text = "write tests"; completed = false } in
  Alcotest.check todo_testable
    "round-trip preserves completed=false"
    todo
    (Todo.deserialize (Todo.serialize todo))

let test_deserialize_completed_with_text () =
  Alcotest.check todo_testable
    "1<TAB>hello world parses as completed=true"
    { Todo.completed = true; text = "hello world" }
    (Todo.deserialize "1\thello world")

let test_deserialize_empty_text () =
  Alcotest.check todo_testable
    "0<TAB> with empty text parses as not-completed empty"
    { Todo.completed = false; text = "" }
    (Todo.deserialize "0\t")

let test_deserialize_no_tab () =
  Alcotest.check todo_testable
    "line without a tab is treated as raw text, not completed"
    { Todo.completed = false; text = "no-tab-line" }
    (Todo.deserialize "no-tab-line")

let test_serialize_completed () =
  Alcotest.(check string)
    "serialize completed prefixes 1<TAB>"
    "1\tdone task"
    (Todo.serialize { Todo.text = "done task"; completed = true })

let () =
  Alcotest.run "dead_simple_todos"
    [
      ( "Todo serialize/deserialize",
        [
          Alcotest.test_case "round-trip completed=true" `Quick
            test_round_trip_completed;
          Alcotest.test_case "round-trip completed=false" `Quick
            test_round_trip_not_completed;
          Alcotest.test_case "deserialize 1<TAB>hello world" `Quick
            test_deserialize_completed_with_text;
          Alcotest.test_case "deserialize 0<TAB>" `Quick
            test_deserialize_empty_text;
          Alcotest.test_case "deserialize no-tab line" `Quick
            test_deserialize_no_tab;
          Alcotest.test_case "serialize completed" `Quick
            test_serialize_completed;
        ] );
    ]
