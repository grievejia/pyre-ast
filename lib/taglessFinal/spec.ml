module Position = struct
  type 'a t = line:int -> column:int -> 'a
end

module Location = struct
  type ('position, 'location) t = start:'position -> stop:'position -> 'location
end

module Identifier = struct
  type 'a t = string -> 'a
end

module Constant = struct
  type 'a t = {
    none : 'a;
    false_ : 'a;
    true_ : 'a;
    ellipsis : 'a;
    integer : int -> 'a;
    big_integer : string -> 'a;
    float_ : float -> 'a;
    complex : float -> 'a;
    string_ : string -> 'a;
    byte_string : string -> 'a;
  }

  let make ~none ~false_ ~true_ ~ellipsis ~integer ~big_integer ~float_ ~complex ~string_
      ~byte_string () =
    { none; false_; true_; ellipsis; integer; big_integer; float_; complex; string_; byte_string }
end

module ExpressionContext = struct
  type 'a t = { load : 'a; store : 'a; del : 'a }

  let make ~load ~store ~del () = { load; store; del }
end

module BooleanOperator = struct
  type 'a t = { and_ : 'a; or_ : 'a }

  let make ~and_ ~or_ () = { and_; or_ }
end

module BinaryOperator = struct
  type 'a t = {
    add : 'a;
    sub : 'a;
    mult : 'a;
    matmult : 'a;
    div : 'a;
    mod_ : 'a;
    pow : 'a;
    lshift : 'a;
    rshift : 'a;
    bitor : 'a;
    bitxor : 'a;
    bitand : 'a;
    floordiv : 'a;
  }

  let make ~add ~sub ~mult ~matmult ~div ~mod_ ~pow ~lshift ~rshift ~bitor ~bitxor ~bitand ~floordiv
      () =
    { add; sub; mult; matmult; div; mod_; pow; lshift; rshift; bitor; bitxor; bitand; floordiv }
end

module UnaryOperator = struct
  type 'a t = { invert : 'a; not_ : 'a; uadd : 'a; usub : 'a }

  let make ~invert ~not_ ~uadd ~usub () = { invert; not_; uadd; usub }
end

module ComparisonOperator = struct
  type 'a t = {
    eq : 'a;
    noteq : 'a;
    lt : 'a;
    lte : 'a;
    gt : 'a;
    gte : 'a;
    is : 'a;
    isnot : 'a;
    in_ : 'a;
    notin : 'a;
  }

  let make ~eq ~noteq ~lt ~lte ~gt ~gte ~is ~isnot ~in_ ~notin () =
    { eq; noteq; lt; lte; gt; gte; is; isnot; in_; notin }
end

module Comprehension = struct
  type ('expr, 'comprehension) t =
    target:'expr -> iter:'expr -> ifs:'expr list -> is_async:bool -> 'comprehension
end

module Keyword = struct
  type ('expr, 'identifier, 'location, 'keyword) t =
    location:'location -> arg:'identifier option -> value:'expr -> 'keyword
end

module Argument = struct
  type ('expr, 'identifier, 'location, 'arg) t =
    location:'location ->
    identifier:'identifier ->
    annotation:'expr option ->
    type_comment:string option ->
    'arg
end

module Arguments = struct
  type ('arg, 'expr, 'arguments) t =
    posonlyargs:'arg list ->
    args:'arg list ->
    vararg:'arg option ->
    kwonlyargs:'arg list ->
    kw_defaults:'expr option list ->
    kwarg:'arg option ->
    defaults:'expr list ->
    'arguments
end

module Expression = struct
  type ('arguments,
         'bin_op,
         'bool_op,
         'compare,
         'comprehension,
         'constant,
         'expr_context,
         'identifier,
         'keyword,
         'location,
         'unary_op,
         'expr)
       t = {
    bool_op : location:'location -> op:'bool_op -> values:'expr list -> 'expr;
    named_expr : location:'location -> target:'expr -> value:'expr -> 'expr;
    bin_op : location:'location -> left:'expr -> op:'bin_op -> right:'expr -> 'expr;
    unary_op : location:'location -> op:'unary_op -> operand:'expr -> 'expr;
    lambda : location:'location -> args:'arguments -> body:'expr -> 'expr;
    if_exp : location:'location -> test:'expr -> body:'expr -> orelse:'expr -> 'expr;
    dict : location:'location -> keys:'expr option list -> values:'expr list -> 'expr;
    set : location:'location -> elts:'expr list -> 'expr;
    list_comp : location:'location -> elt:'expr -> generators:'comprehension list -> 'expr;
    set_comp : location:'location -> elt:'expr -> generators:'comprehension list -> 'expr;
    dict_comp :
      location:'location -> key:'expr -> value:'expr -> generators:'comprehension list -> 'expr;
    generator_exp : location:'location -> elt:'expr -> generators:'comprehension list -> 'expr;
    await : location:'location -> value:'expr -> 'expr;
    yield : location:'location -> value:'expr option -> 'expr;
    yield_from : location:'location -> value:'expr -> 'expr;
    compare :
      location:'location -> left:'expr -> ops:'compare list -> comparators:'expr list -> 'expr;
    call : location:'location -> func:'expr -> args:'expr list -> keywords:'keyword list -> 'expr;
    formatted_value :
      location:'location -> value:'expr -> conversion:int -> format_spec:'expr option -> 'expr;
    joined_str : location:'location -> values:'expr list -> 'expr;
    constant : location:'location -> value:'constant -> kind:string option -> 'expr;
    attribute : location:'location -> value:'expr -> attr:'identifier -> ctx:'expr_context -> 'expr;
    subscript : location:'location -> value:'expr -> slice:'expr -> ctx:'expr_context -> 'expr;
    starred : location:'location -> value:'expr -> ctx:'expr_context -> 'expr;
    name : location:'location -> id:'identifier -> ctx:'expr_context -> 'expr;
    list : location:'location -> elts:'expr list -> ctx:'expr_context -> 'expr;
    tuple : location:'location -> elts:'expr list -> ctx:'expr_context -> 'expr;
    slice :
      location:'location -> lower:'expr option -> upper:'expr option -> step:'expr option -> 'expr;
  }

  let make ~bool_op ~named_expr ~bin_op ~unary_op ~lambda ~if_exp ~dict ~set ~list_comp ~set_comp
      ~dict_comp ~generator_exp ~await ~yield ~yield_from ~compare ~call ~formatted_value
      ~joined_str ~constant ~attribute ~subscript ~starred ~name ~list ~tuple ~slice () =
    {
      bool_op;
      named_expr;
      bin_op;
      unary_op;
      lambda;
      if_exp;
      dict;
      set;
      list_comp;
      set_comp;
      dict_comp;
      generator_exp;
      await;
      yield;
      yield_from;
      compare;
      call;
      formatted_value;
      joined_str;
      constant;
      attribute;
      subscript;
      starred;
      name;
      list;
      tuple;
      slice;
    }
end

module WithItem = struct
  type ('expr, 'with_item) t = context_expr:'expr -> optional_vars:'expr option -> 'with_item
end

module ImportAlias = struct
  type ('identifier, 'location, 'alias) t =
    location:'location -> name:'identifier -> asname:'identifier option -> 'alias
end

module ExceptionHandler = struct
  type ('expr, 'identifier, 'location, 'stmt, 'except_handler) t =
    location:'location ->
    type_:'expr option ->
    name:'identifier option ->
    body:'stmt list ->
    'except_handler
end

module MatchCase = struct
  type ('expr, 'pattern, 'stmt, 'match_case) t =
    pattern:'pattern -> guard:'expr option -> body:'stmt list -> 'match_case
end

module Pattern = struct
  type ('constant, 'expr, 'identifier, 'location, 'pattern) t = {
    match_value : location:'location -> value:'expr -> 'pattern;
    match_singleton : location:'location -> value:'constant -> 'pattern;
    match_sequence : location:'location -> patterns:'pattern list -> 'pattern;
    match_mapping :
      location:'location ->
      keys:'expr list ->
      patterns:'pattern list ->
      rest:'identifier option ->
      'pattern;
    match_class :
      location:'location ->
      cls:'expr ->
      patterns:'pattern list ->
      kwd_attrs:'identifier list ->
      kwd_patterns:'pattern list ->
      'pattern;
    match_star : location:'location -> name:'identifier option -> 'pattern;
    match_as : location:'location -> pattern:'pattern option -> name:'identifier option -> 'pattern;
    match_or : location:'location -> patterns:'pattern list -> 'pattern;
  }

  let make ~match_value ~match_singleton ~match_sequence ~match_mapping ~match_class ~match_star
      ~match_as ~match_or () =
    {
      match_value;
      match_singleton;
      match_sequence;
      match_mapping;
      match_class;
      match_star;
      match_as;
      match_or;
    }
end

module Statement = struct
  type ('alias,
         'arguments,
         'bin_op,
         'except_handler,
         'expr,
         'identifier,
         'keyword,
         'location,
         'match_case,
         'with_item,
         'stmt)
       t = {
    function_def :
      location:'location ->
      name:'identifier ->
      args:'arguments ->
      body:'stmt list ->
      decorator_list:'expr list ->
      returns:'expr option ->
      type_comment:string option ->
      'stmt;
    async_function_def :
      location:'location ->
      name:'identifier ->
      args:'arguments ->
      body:'stmt list ->
      decorator_list:'expr list ->
      returns:'expr option ->
      type_comment:string option ->
      'stmt;
    class_def :
      location:'location ->
      name:'identifier ->
      bases:'expr list ->
      keywords:'keyword list ->
      body:'stmt list ->
      decorator_list:'expr list ->
      'stmt;
    return : location:'location -> value:'expr option -> 'stmt;
    delete : location:'location -> targets:'expr list -> 'stmt;
    assign :
      location:'location -> targets:'expr list -> value:'expr -> type_comment:string option -> 'stmt;
    aug_assign : location:'location -> target:'expr -> op:'bin_op -> value:'expr -> 'stmt;
    ann_assign :
      location:'location ->
      target:'expr ->
      annotation:'expr ->
      value:'expr option ->
      simple:bool ->
      'stmt;
    for_ :
      location:'location ->
      target:'expr ->
      iter:'expr ->
      body:'stmt list ->
      orelse:'stmt list ->
      type_comment:string option ->
      'stmt;
    async_for :
      location:'location ->
      target:'expr ->
      iter:'expr ->
      body:'stmt list ->
      orelse:'stmt list ->
      type_comment:string option ->
      'stmt;
    while_ : location:'location -> test:'expr -> body:'stmt list -> orelse:'stmt list -> 'stmt;
    if_ : location:'location -> test:'expr -> body:'stmt list -> orelse:'stmt list -> 'stmt;
    with_ :
      location:'location ->
      items:'with_item list ->
      body:'stmt list ->
      type_comment:string option ->
      'stmt;
    async_with :
      location:'location ->
      items:'with_item list ->
      body:'stmt list ->
      type_comment:string option ->
      'stmt;
    match_ : location:'location -> subject:'expr -> cases:'match_case list -> 'stmt;
    raise_ : location:'location -> exc:'expr option -> cause:'expr option -> 'stmt;
    try_ :
      location:'location ->
      body:'stmt list ->
      handlers:'except_handler list ->
      orelse:'stmt list ->
      finalbody:'stmt list ->
      'stmt;
    assert_ : location:'location -> test:'expr -> msg:'expr option -> 'stmt;
    import : location:'location -> names:'alias list -> 'stmt;
    import_from :
      location:'location -> module_:'identifier option -> names:'alias list -> level:int -> 'stmt;
    global : location:'location -> names:'identifier list -> 'stmt;
    nonlocal : location:'location -> names:'identifier list -> 'stmt;
    expr : location:'location -> value:'expr -> 'stmt;
    pass : location:'location -> 'stmt;
    break : location:'location -> 'stmt;
    continue : location:'location -> 'stmt;
  }

  let make ~function_def ~async_function_def ~class_def ~return ~delete ~assign ~aug_assign
      ~ann_assign ~for_ ~async_for ~while_ ~if_ ~with_ ~async_with ~match_ ~raise_ ~try_ ~assert_
      ~import ~import_from ~global ~nonlocal ~expr ~pass ~break ~continue () =
    {
      function_def;
      async_function_def;
      class_def;
      return;
      delete;
      assign;
      aug_assign;
      ann_assign;
      for_;
      async_for;
      while_;
      if_;
      with_;
      async_with;
      match_;
      raise_;
      try_;
      assert_;
      import;
      import_from;
      global;
      nonlocal;
      expr;
      pass;
      break;
      continue;
    }
end

module TypeIgnore = struct
  type 'a t = lineno:int -> tag:string -> 'a
end

module Module = struct
  type ('stmt, 'type_ignore, 'module_) t =
    body:'stmt list -> type_ignores:'type_ignore list -> 'module_
end

module FunctionType = struct
  type ('expr, 'function_type) t = argtypes:'expr list -> returns:'expr -> 'function_type
end

type ('argument,
       'arguments,
       'binary_operator,
       'boolean_operator,
       'comparison_operator,
       'comprehension,
       'constant,
       'exception_handler,
       'expression,
       'expression_context,
       'function_type,
       'identifier,
       'import_alias,
       'keyword,
       'location,
       'match_case,
       'module_,
       'pattern,
       'position,
       'statement,
       'type_ignore,
       'unary_operator,
       'with_item)
     t = {
  argument : ('expression, 'identifier, 'location, 'argument) Argument.t;
  arguments : ('argument, 'expression, 'arguments) Arguments.t;
  binary_operator : 'binary_operator BinaryOperator.t;
  boolean_operator : 'boolean_operator BooleanOperator.t;
  comparison_operator : 'comparison_operator ComparisonOperator.t;
  comprehension : ('expression, 'comprehension) Comprehension.t;
  constant : 'constant Constant.t;
  exception_handler :
    ('expression, 'identifier, 'location, 'statement, 'exception_handler) ExceptionHandler.t;
  expression :
    ( 'arguments,
      'binary_operator,
      'boolean_operator,
      'comparison_operator,
      'comprehension,
      'constant,
      'expression_context,
      'identifier,
      'keyword,
      'location,
      'unary_operator,
      'expression )
    Expression.t;
  expression_context : 'expression_context ExpressionContext.t;
  function_type : ('expression, 'function_type) FunctionType.t;
  identifier : 'identifier Identifier.t;
  import_alias : ('identifier, 'location, 'import_alias) ImportAlias.t;
  keyword : ('expression, 'identifier, 'location, 'keyword) Keyword.t;
  location : ('position, 'location) Location.t;
  match_case : ('expression, 'pattern, 'statement, 'match_case) MatchCase.t;
  module_ : ('statement, 'type_ignore, 'module_) Module.t;
  pattern : ('constant, 'expression, 'identifier, 'location, 'pattern) Pattern.t;
  position : 'position Position.t;
  statement :
    ( 'import_alias,
      'arguments,
      'binary_operator,
      'exception_handler,
      'expression,
      'identifier,
      'keyword,
      'location,
      'match_case,
      'with_item,
      'statement )
    Statement.t;
  type_ignore : 'type_ignore TypeIgnore.t;
  unary_operator : 'unary_operator UnaryOperator.t;
  with_item : ('expression, 'with_item) WithItem.t;
}

let make ~argument ~arguments ~binary_operator ~boolean_operator ~comparison_operator ~comprehension
    ~constant ~exception_handler ~expression ~expression_context ~function_type ~identifier
    ~import_alias ~keyword ~location ~match_case ~module_ ~pattern ~position ~statement ~type_ignore
    ~unary_operator ~with_item () =
  {
    argument;
    arguments;
    binary_operator;
    boolean_operator;
    comparison_operator;
    comprehension;
    constant;
    exception_handler;
    expression;
    expression_context;
    function_type;
    identifier;
    import_alias;
    keyword;
    location;
    match_case;
    module_;
    pattern;
    position;
    statement;
    type_ignore;
    unary_operator;
    with_item;
  }
