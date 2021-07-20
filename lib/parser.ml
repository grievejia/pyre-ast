module Context = TaglessFinal.Parsing.Context
module Error = TaglessFinal.Parsing.ParseError

let with_context = TaglessFinal.Parsing.with_context

module TaglessFinal = struct
  let parse_module = TaglessFinal.Parsing.parse_module

  let parse_expression = TaglessFinal.Parsing.parse_expression

  let parse_function_type = TaglessFinal.Parsing.parse_function_type
end

module Concrete = struct
  let spec = Concrete.make_tagless_final ()

  let parse_module ~context = TaglessFinal.parse_module ~context ~spec

  let parse_expression ~context = TaglessFinal.parse_expression ~context ~spec

  let parse_function_type ~context = TaglessFinal.parse_function_type ~context ~spec
end
