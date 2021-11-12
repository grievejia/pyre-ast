exception ParsingError of string * int * int * int * int

let _ = Callback.register_exception "parsing_error" (ParsingError ("dummy string", 0, 0, 0, 0))

external initialize_python_runtime : unit -> bool = "initialize_python_runtime"

external finalize_python_runtime : unit -> unit = "finalize_python_runtime"

type raw_module

external cpython_parse_module : string -> bool -> string -> raw_module = "cpython_parse_module"

external cpython_convert_module :
  (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, 'module_, _, _, _, _, _, _) Spec.t ->
  raw_module ->
  'module_ = "cpython_convert_module"

external cpython_parse_expression : string -> raw_module = "cpython_parse_expression"

external cpython_convert_expression :
  (_, _, _, _, _, _, _, _, 'expression, _, _, _, _, _, _, _, _, _, _, _, _, _, _) Spec.t ->
  raw_module ->
  'expression = "cpython_convert_expression"

external cpython_parse_function_type : string -> raw_module = "cpython_parse_function_type"

external cpython_convert_function_type :
  (_, _, _, _, _, _, _, _, _, _, 'function_type, _, _, _, _, _, _, _, _, _, _, _, _) Spec.t ->
  raw_module ->
  'function_type = "cpython_convert_function_type"

external cpython_release_module : raw_module -> unit = "cpython_release_module"

module Context = struct
  (* `Context.t` does not carry any acutal data itself -- it is mainly used as a marker to prevent
     misuse of the APIs (i.e. invoking the parser without initializing CPython runtime first). *)
  type t = unit

  let create () = match initialize_python_runtime () with false -> None | true -> Some ()

  let destroy () = finalize_python_runtime ()
end

module ParseError = struct
  type t = { message : string; line : int; column : int; end_line : int; end_column : int }
end

let with_context
    ?(on_init_failure = fun () -> failwith "Encountered internal error when initializing CPython")
    on_success =
  match Context.create () with
  | None -> on_init_failure ()
  | Some context -> Base.Exn.protectx ~f:on_success context ~finally:Context.destroy

let exception_to_result f =
  try Result.Ok (f ())
  with ParsingError (message, line, column, end_line, end_column) ->
    Result.Error { ParseError.message; line; column; end_line; end_column }

let parse_module ~context:_ ~spec ?(enable_type_comment = false) input =
  let do_parse () =
    let raw_module = cpython_parse_module "<unknown>" enable_type_comment input in
    Base.Exn.protectx ~f:(cpython_convert_module spec) raw_module ~finally:cpython_release_module
  in
  exception_to_result do_parse

let parse_expression ~context:_ ~spec input =
  let do_parse () =
    let raw_module = cpython_parse_expression input in
    Base.Exn.protectx ~f:(cpython_convert_expression spec) raw_module
      ~finally:cpython_release_module
  in
  exception_to_result do_parse

let parse_function_type ~context:_ ~spec input =
  let do_parse () =
    let raw_module = cpython_parse_function_type input in
    Base.Exn.protectx
      ~f:(cpython_convert_function_type spec)
      raw_module ~finally:cpython_release_module
  in
  exception_to_result do_parse
