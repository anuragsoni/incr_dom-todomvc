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
    | DeleteFinished
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
              ; Attr.bool_property "checked" finished
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
  let footer =
    let finished, unfinished = List.partition_tf ~f:(fun t -> t.finished) m.items in
    let count, label =
      let unfinished_count = List.length unfinished in
      match unfinished_count with
      | 0 -> 0, " items left"
      | 1 -> 1, " item left"
      | c -> c, " items left"
    in
    let todo_count =
      Node.span
        [ Attr.class_ "todo-count" ]
        [ Node.strong [] [ Node.text (Int.to_string count) ]; Node.text label ]
    in
    let any_finished = List.length finished > 0 in
    Node.footer
      [ Attr.class_ "footer" ]
      (if not any_finished
      then [ todo_count ]
      else
        [ todo_count
        ; Node.button
            [ Attr.class_ "clear-completed"
            ; Attr.on_click (fun _ -> inject DeleteFinished)
            ]
            [ Node.text "Clear completed" ]
        ])
  in
  let show_footer = List.length m.items <> 0 in
  Node.body
    []
    [ Node.section
        [ Attr.class_ "todoapp" ]
        (if show_footer then [ header; section; footer ] else [ header; section ])
    ]
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
  | DeleteFinished ->
    { model with items = List.filter ~f:(fun t -> not t.finished) items }
  | UpdateInputField input_field -> { model with input_field }
;;

let create model ~old_model:_ ~inject =
  let open Incr.Let_syntax in
  let%map model = model in
  let apply_action = apply_action model in
  let view = view model ~inject in
  Component.create ~apply_action model view
;;
