open Core_kernel
open Async_kernel
open Incr_dom

module Todo = struct
  type t =
    { finished : bool
    ; description : string
    ; id : int
    }
  [@@deriving sexp, fields, equal]
end

module Model = struct
  type t =
    { items : Todo.t list
    ; input_field : string
    ; id : int
    }
  [@@deriving sexp, equal]

  let cutoff t1 t2 = equal t1 t2
end

module Action = struct
  type t =
    | AddTodo
    | DeleteTodo of int
    | ToggleSelection of int
    | UpdateInputField of string
  [@@deriving sexp]
end

module State = struct
  type t = unit
end

let initial_model = { Model.items = []; id = 0; input_field = "" }
let on_startup ~schedule_action:_ _model = Deferred.unit

let view (m : Model.t) ~(inject : Action.t -> Vdom.Event.t) =
  let open Vdom in
  let render_todo_item { Todo.finished; description; id } =
    Node.li
      (if finished then [ Attr.class_ "completed" ] else [])
      [ Node.div
          [ Attr.class_ "view" ]
          [ Node.input
              [ Attr.class_ "toggle"
              ; Attr.type_ "checkbox"
              ; Attr.on_click (fun _ev -> inject (Action.ToggleSelection id))
              ]
              []
          ; Node.label [] [ Node.text description ]
          ; Node.button
              [ Attr.class_ "destroy"
              ; Attr.on_click (fun _ev -> inject (Action.DeleteTodo id))
              ]
              []
          ]
      ]
  in
  let render_todo_list model =
    Node.ul [ Attr.class_ "todo-list" ] (List.map ~f:render_todo_item model)
  in
  let header =
    Node.header
      [ Attr.class_ "header" ]
      [ Node.h1 [] [ Node.text "todos" ]
      ; Node.input
          [ Attr.class_ "new-todo"
          ; Attr.placeholder "What needs to be done?"
          ; Attr.value m.input_field
          ; Attr.autofocus true
          ; Attr.on_input (fun _ev text -> inject (Action.UpdateInputField text))
          ; Attr.on_keydown (fun e ->
                if e##.keyCode = 13 then inject Action.AddTodo else Event.Ignore)
          ]
          []
      ]
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
      Node.section [ Attr.class_ "main" ] [ toggle_all; label; render_todo_list m.items ]
    in
    Node.section [ Attr.class_ "todoapp" ] [ header; section ]
  in
  Node.body [] [ todo_app ]
;;

let apply_action ({ Model.id; items; input_field } as model) action _ ~schedule_action:_ =
  match (action : Action.t) with
  | AddTodo ->
    if input_field = ""
    then model
    else (
      let todo = Todo.Fields.create ~finished:false ~id ~description:input_field in
      { Model.id = id + 1; items = todo :: items; input_field = "" })
  | ToggleSelection id ->
    let items =
      List.map
        ~f:(fun t -> if t.id = id then { t with finished = not t.finished } else t)
        items
    in
    { model with items }
  | DeleteTodo id -> { model with items = List.filter ~f:(fun t -> t.id <> id) items }
  | UpdateInputField input_field -> { model with input_field }
;;

let create model ~old_model:_ ~inject =
  let open Incr.Let_syntax in
  let%map model = model in
  let apply_action = apply_action model in
  let view = view model ~inject in
  Component.create ~apply_action model view
;;
