open Core_kernel
open Async_kernel
open Incr_dom

module Todo = struct
  type t =
    { finished : bool
    ; description : string
    }
  [@@deriving sexp, fields, equal]
end

module Todos = struct
  type t = Todo.t list [@@deriving sexp, equal]

  let of_string_list items =
    List.map ~f:(fun description -> { Todo.finished = false; description }) items
  ;;
end

module Model = struct
  type t = Todos.t

  let cutoff t1 t2 = Todos.equal t1 t2
end

module Action = struct
  type t = Nothing.t [@@deriving sexp]
end

module State = struct
  type t = unit
end

let initial_model = Todos.of_string_list [ "Learn OCaml"; "Build Todo List" ]
let on_startup ~schedule_action:_ _model = Deferred.unit

let view (m : Model.t) ~(inject : Action.t -> Vdom.Event.t) =
  let open Vdom in
  let header =
    Node.header [ Attr.class_ "header" ] [ Node.h1 [] [ Node.text "todos" ] ]
  in
  let todo_app =
    let section =
      let toggle_all =
        Node.input
          [ Attr.id "toggle-all"; Attr.class_ "toggle-all"; Attr.type_ "checkbox" ]
          []
      in
      let label =
        Node.label [ Attr.for_ "toggle-all" ] [ Node.text "Mark all as complete" ]
      in
      let show_todo_item (t : Todo.t) =
        Node.li
          []
          [ Node.div
              [ Attr.class_ "view" ]
              [ Node.input [ Attr.class_ "toggle"; Attr.type_ "checkbox" ] []
              ; Node.label [] [ Node.text t.description ]
              ; Node.button [ Attr.class_ "destroy" ] []
              ]
          ]
      in
      let todo_items =
        Node.ul [ Attr.class_ "todo-list" ] (List.map ~f:show_todo_item m)
      in
      Node.section [ Attr.class_ "main" ] [ toggle_all; label; todo_items ]
    in
    Node.section [ Attr.class_ "todoapp" ] [ header; section ]
  in
  Node.body [] [ todo_app ]
;;

let create model ~old_model:_ ~inject =
  let open Incr.Let_syntax in
  let%map model = model in
  let apply_action action _ ~schedule_action:_ = Nothing.unreachable_code action in
  let view = view model ~inject in
  Component.create ~apply_action model view
;;
