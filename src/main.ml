open Incr_dom

let () =
  Start_app.start
    (module App)
    ~bind_to_element_with_id:"todomvc"
    ~initial_model:App.initial_model
;;
