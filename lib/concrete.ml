open Base

module Position = struct
  type t = { line : int; column : int } [@@deriving sexp, compare, hash, make]
end

module Location = struct
  type t = { start : Position.t; stop : Position.t } [@@deriving sexp, compare, hash, make]
end

module Identifier = struct
  type t = string [@@deriving sexp, compare, hash]

  let make_t x () = x
  let to_string x = x
end

module Constant = struct
  type t =
    | None
    | False
    | True
    | Ellipsis
    | Integer of int
    | BigInteger of string
    | Float of float
    | Complex of float
    | String of string
    | ByteString of string
  [@@deriving sexp, compare, hash]

  let make_none_of_t () = None
  let make_false_of_t () = False
  let make_true_of_t () = True
  let make_ellipsis_of_t () = Ellipsis
  let make_integer_of_t i = Integer i
  let make_big_integer_of_t i = BigInteger i
  let make_float_of_t f = Float f
  let make_complex_of_t c = Complex c
  let make_string_of_t s = String s
  let make_byte_string_of_t s = ByteString s
end

module ExpressionContext = struct
  type t = Load | Store | Del [@@deriving sexp, compare, hash]

  let make_load_of_t () = Load
  let make_store_of_t () = Store
  let make_del_of_t () = Del
end

module BooleanOperator = struct
  type t = And | Or [@@deriving sexp, compare, hash]

  let make_and_of_t () = And
  let make_or_of_t () = Or
end

module BinaryOperator = struct
  type t =
    | Add
    | Sub
    | Mult
    | MatMult
    | Div
    | Mod
    | Pow
    | LShift
    | RShift
    | BitOr
    | BitXor
    | BitAnd
    | FloorDiv
  [@@deriving sexp, compare, hash]

  let make_add_of_t () = Add
  let make_sub_of_t () = Sub
  let make_mult_of_t () = Mult
  let make_matmult_of_t () = MatMult
  let make_div_of_t () = Div
  let make_mod_of_t () = Mod
  let make_pow_of_t () = Pow
  let make_lshift_of_t () = LShift
  let make_rshift_of_t () = RShift
  let make_bitor_of_t () = BitOr
  let make_bitxor_of_t () = BitXor
  let make_bitand_of_t () = BitAnd
  let make_floordiv_of_t () = FloorDiv
end

module UnaryOperator = struct
  type t = Invert | Not | UAdd | USub [@@deriving sexp, compare, hash]

  let make_invert_of_t () = Invert
  let make_not_of_t () = Not
  let make_uadd_of_t () = UAdd
  let make_usub_of_t () = USub
end

module ComparisonOperator = struct
  type t = Eq | NotEq | Lt | Lte | Gt | Gte | Is | IsNot | In | NotIn
  [@@deriving sexp, compare, hash]

  let make_eq_of_t () = Eq
  let make_noteq_of_t () = NotEq
  let make_lt_of_t () = Lt
  let make_lte_of_t () = Lte
  let make_gt_of_t () = Gt
  let make_gte_of_t () = Gte
  let make_is_of_t () = Is
  let make_isnot_of_t () = IsNot
  let make_in_of_t () = In
  let make_notin_of_t () = NotIn
end

module ImportAlias = struct
  type t = { location : Location.t; name : Identifier.t; asname : Identifier.t option }
  [@@deriving sexp, compare, hash, make]
end

module rec Comprehension : sig
  type t = { target : Expression.t; iter : Expression.t; ifs : Expression.t list; is_async : bool }
  [@@deriving sexp, compare, hash, make]
end = struct
  type t = { target : Expression.t; iter : Expression.t; ifs : Expression.t list; is_async : bool }
  [@@deriving sexp, compare, hash, make]
end

and Keyword : sig
  type t = { location : Location.t; arg : Identifier.t option; value : Expression.t }
  [@@deriving sexp, compare, hash, make]
end = struct
  type t = { location : Location.t; arg : Identifier.t option; value : Expression.t }
  [@@deriving sexp, compare, hash, make]
end

and Argument : sig
  type t = {
    location : Location.t;
    identifier : Identifier.t;
    annotation : Expression.t option;
    type_comment : string option;
  }
  [@@deriving sexp, compare, hash, make]
end = struct
  type t = {
    location : Location.t;
    identifier : Identifier.t;
    annotation : Expression.t option;
    type_comment : string option;
  }
  [@@deriving sexp, compare, hash, make]
end

and Arguments : sig
  type t = {
    posonlyargs : Argument.t list;
    args : Argument.t list;
    vararg : Argument.t option;
    kwonlyargs : Argument.t list;
    kw_defaults : Expression.t option list;
    kwarg : Argument.t option;
    defaults : Expression.t list;
  }
  [@@deriving sexp, compare, hash, make]
end = struct
  type t = {
    posonlyargs : Argument.t list;
    args : Argument.t list;
    vararg : Argument.t option;
    kwonlyargs : Argument.t list;
    kw_defaults : Expression.t option list;
    kwarg : Argument.t option;
    defaults : Expression.t list;
  }
  [@@deriving sexp, compare, hash, make]
end

and Expression : sig
  type t =
    | BoolOp of { location : Location.t; op : BooleanOperator.t; values : t list }
    | NamedExpr of { location : Location.t; target : t; value : t }
    | BinOp of { location : Location.t; left : t; op : BinaryOperator.t; right : t }
    | UnaryOp of { location : Location.t; op : UnaryOperator.t; operand : t }
    | Lambda of { location : Location.t; args : Arguments.t; body : t }
    | IfExp of { location : Location.t; test : t; body : t; orelse : t }
    | Dict of { location : Location.t; keys : t option list; values : t list }
    | Set of { location : Location.t; elts : t list }
    | ListComp of { location : Location.t; elt : t; generators : Comprehension.t list }
    | SetComp of { location : Location.t; elt : t; generators : Comprehension.t list }
    | DictComp of { location : Location.t; key : t; value : t; generators : Comprehension.t list }
    | GeneratorExp of { location : Location.t; elt : t; generators : Comprehension.t list }
    | Await of { location : Location.t; value : t }
    | Yield of { location : Location.t; value : t option }
    | YieldFrom of { location : Location.t; value : t }
    | Compare of {
        location : Location.t;
        left : t;
        ops : ComparisonOperator.t list;
        comparators : t list;
      }
    | Call of { location : Location.t; func : t; args : t list; keywords : Keyword.t list }
    | FormattedValue of {
        location : Location.t;
        value : t;
        conversion : int;
        format_spec : t option;
      }
    | JoinedStr of { location : Location.t; values : t list }
    | Constant of { location : Location.t; value : Constant.t; kind : string option }
    | Attribute of {
        location : Location.t;
        value : t;
        attr : Identifier.t;
        ctx : ExpressionContext.t;
      }
    | Subscript of { location : Location.t; value : t; slice : t; ctx : ExpressionContext.t }
    | Starred of { location : Location.t; value : t; ctx : ExpressionContext.t }
    | Name of { location : Location.t; id : Identifier.t; ctx : ExpressionContext.t }
    | List of { location : Location.t; elts : t list; ctx : ExpressionContext.t }
    | Tuple of { location : Location.t; elts : t list; ctx : ExpressionContext.t }
    | Slice of { location : Location.t; lower : t option; upper : t option; step : t option }
  [@@deriving sexp, compare, hash, make]
end = struct
  type t =
    | BoolOp of { location : Location.t; op : BooleanOperator.t; values : t list }
    | NamedExpr of { location : Location.t; target : t; value : t }
    | BinOp of { location : Location.t; left : t; op : BinaryOperator.t; right : t }
    | UnaryOp of { location : Location.t; op : UnaryOperator.t; operand : t }
    | Lambda of { location : Location.t; args : Arguments.t; body : t }
    | IfExp of { location : Location.t; test : t; body : t; orelse : t }
    | Dict of { location : Location.t; keys : t option list; values : t list }
    | Set of { location : Location.t; elts : t list }
    | ListComp of { location : Location.t; elt : t; generators : Comprehension.t list }
    | SetComp of { location : Location.t; elt : t; generators : Comprehension.t list }
    | DictComp of { location : Location.t; key : t; value : t; generators : Comprehension.t list }
    | GeneratorExp of { location : Location.t; elt : t; generators : Comprehension.t list }
    | Await of { location : Location.t; value : t }
    | Yield of { location : Location.t; value : t option }
    | YieldFrom of { location : Location.t; value : t }
    | Compare of {
        location : Location.t;
        left : t;
        ops : ComparisonOperator.t list;
        comparators : t list;
      }
    | Call of { location : Location.t; func : t; args : t list; keywords : Keyword.t list }
    | FormattedValue of {
        location : Location.t;
        value : t;
        conversion : int;
        format_spec : t option;
      }
    | JoinedStr of { location : Location.t; values : t list }
    | Constant of { location : Location.t; value : Constant.t; kind : string option }
    | Attribute of {
        location : Location.t;
        value : t;
        attr : Identifier.t;
        ctx : ExpressionContext.t;
      }
    | Subscript of { location : Location.t; value : t; slice : t; ctx : ExpressionContext.t }
    | Starred of { location : Location.t; value : t; ctx : ExpressionContext.t }
    | Name of { location : Location.t; id : Identifier.t; ctx : ExpressionContext.t }
    | List of { location : Location.t; elts : t list; ctx : ExpressionContext.t }
    | Tuple of { location : Location.t; elts : t list; ctx : ExpressionContext.t }
    | Slice of { location : Location.t; lower : t option; upper : t option; step : t option }
  [@@deriving sexp, compare, hash, make]
end

module WithItem = struct
  type t = { context_expr : Expression.t; optional_vars : Expression.t option }
  [@@deriving sexp, compare, hash, make]
end

module TypeParam = struct
  type t =
    | TypeVar of { location : Location.t; name : Identifier.t; bound : Expression.t option }
    | ParamSpec of { location : Location.t; name : Identifier.t }
    | TypeVarTuple of { location : Location.t; name : Identifier.t }
  [@@deriving sexp, compare, hash, make]
end

module rec ExceptionHandler : sig
  type t = {
    location : Location.t;
    type_ : Expression.t option;
    name : Identifier.t option;
    body : Statement.t list;
  }
  [@@deriving sexp, compare, hash, make]
end = struct
  type t = {
    location : Location.t;
    type_ : Expression.t option;
    name : Identifier.t option;
    body : Statement.t list;
  }
  [@@deriving sexp, compare, hash, make]
end

and MatchCase : sig
  type t = { pattern : Pattern.t; guard : Expression.t option; body : Statement.t list }
  [@@deriving sexp, compare, hash, make]
end = struct
  type t = { pattern : Pattern.t; guard : Expression.t option; body : Statement.t list }
  [@@deriving sexp, compare, hash, make]
end

and Pattern : sig
  type t =
    | MatchValue of { location : Location.t; value : Expression.t }
    | MatchSingleton of { location : Location.t; value : Constant.t }
    | MatchSequence of { location : Location.t; patterns : t list }
    | MatchMapping of {
        location : Location.t;
        keys : Expression.t list;
        patterns : t list;
        rest : Identifier.t option;
      }
    | MatchClass of {
        location : Location.t;
        cls : Expression.t;
        patterns : t list;
        kwd_attrs : Identifier.t list;
        kwd_patterns : t list;
      }
    | MatchStar of { location : Location.t; name : Identifier.t option }
    | MatchAs of { location : Location.t; pattern : t option; name : Identifier.t option }
    | MatchOr of { location : Location.t; patterns : t list }
  [@@deriving sexp, compare, hash, make]
end = struct
  type t =
    | MatchValue of { location : Location.t; value : Expression.t }
    | MatchSingleton of { location : Location.t; value : Constant.t }
    | MatchSequence of { location : Location.t; patterns : t list }
    | MatchMapping of {
        location : Location.t;
        keys : Expression.t list;
        patterns : t list;
        rest : Identifier.t option;
      }
    | MatchClass of {
        location : Location.t;
        cls : Expression.t;
        patterns : t list;
        kwd_attrs : Identifier.t list;
        kwd_patterns : Pattern.t list;
      }
    | MatchStar of { location : Location.t; name : Identifier.t option }
    | MatchAs of { location : Location.t; pattern : Pattern.t option; name : Identifier.t option }
    | MatchOr of { location : Location.t; patterns : Pattern.t list }
  [@@deriving sexp, compare, hash, make]
end

and Statement : sig
  type t =
    | FunctionDef of {
        location : Location.t;
        name : Identifier.t;
        args : Arguments.t;
        body : t list;
        decorator_list : Expression.t list;
        returns : Expression.t option;
        type_comment : string option;
        type_params : TypeParam.t list;
      }
    | AsyncFunctionDef of {
        location : Location.t;
        name : Identifier.t;
        args : Arguments.t;
        body : t list;
        decorator_list : Expression.t list;
        returns : Expression.t option;
        type_comment : string option;
        type_params : TypeParam.t list;
      }
    | ClassDef of {
        location : Location.t;
        name : Identifier.t;
        bases : Expression.t list;
        keywords : Keyword.t list;
        body : t list;
        decorator_list : Expression.t list;
        type_params : TypeParam.t list;
      }
    | Return of { location : Location.t; value : Expression.t option }
    | Delete of { location : Location.t; targets : Expression.t list }
    | Assign of {
        location : Location.t;
        targets : Expression.t list;
        value : Expression.t;
        type_comment : string option;
      }
    | TypeAlias of {
        location : Location.t;
        name : Expression.t;
        type_params : TypeParam.t list;
        value : Expression.t;
      }
    | AugAssign of {
        location : Location.t;
        target : Expression.t;
        op : BinaryOperator.t;
        value : Expression.t;
      }
    | AnnAssign of {
        location : Location.t;
        target : Expression.t;
        annotation : Expression.t;
        value : Expression.t option;
        simple : bool;
      }
    | For of {
        location : Location.t;
        target : Expression.t;
        iter : Expression.t;
        body : t list;
        orelse : t list;
        type_comment : string option;
      }
    | AsyncFor of {
        location : Location.t;
        target : Expression.t;
        iter : Expression.t;
        body : t list;
        orelse : t list;
        type_comment : string option;
      }
    | While of { location : Location.t; test : Expression.t; body : t list; orelse : t list }
    | If of { location : Location.t; test : Expression.t; body : t list; orelse : t list }
    | With of {
        location : Location.t;
        items : WithItem.t list;
        body : t list;
        type_comment : string option;
      }
    | AsyncWith of {
        location : Location.t;
        items : WithItem.t list;
        body : t list;
        type_comment : string option;
      }
    | Match of { location : Location.t; subject : Expression.t; cases : MatchCase.t list }
    | Raise of { location : Location.t; exc : Expression.t option; cause : Expression.t option }
    | Try of {
        location : Location.t;
        body : t list;
        handlers : ExceptionHandler.t list;
        orelse : t list;
        finalbody : t list;
      }
    | TryStar of {
        location : Location.t;
        body : t list;
        handlers : ExceptionHandler.t list;
        orelse : t list;
        finalbody : t list;
      }
    | Assert of { location : Location.t; test : Expression.t; msg : Expression.t option }
    | Import of { location : Location.t; names : ImportAlias.t list }
    | ImportFrom of {
        location : Location.t;
        module_ : Identifier.t option;
        names : ImportAlias.t list;
        level : int;
      }
    | Global of { location : Location.t; names : Identifier.t list }
    | Nonlocal of { location : Location.t; names : Identifier.t list }
    | Expr of { location : Location.t; value : Expression.t }
    | Pass of { location : Location.t }
    | Break of { location : Location.t }
    | Continue of { location : Location.t }
  [@@deriving sexp, compare, hash, make]
end = struct
  type t =
    | FunctionDef of {
        location : Location.t;
        name : Identifier.t;
        args : Arguments.t;
        body : t list;
        decorator_list : Expression.t list;
        returns : Expression.t option;
        type_comment : string option;
        type_params : TypeParam.t list;
      }
    | AsyncFunctionDef of {
        location : Location.t;
        name : Identifier.t;
        args : Arguments.t;
        body : t list;
        decorator_list : Expression.t list;
        returns : Expression.t option;
        type_comment : string option;
        type_params : TypeParam.t list;
      }
    | ClassDef of {
        location : Location.t;
        name : Identifier.t;
        bases : Expression.t list;
        keywords : Keyword.t list;
        body : t list;
        decorator_list : Expression.t list;
        type_params : TypeParam.t list;
      }
    | Return of { location : Location.t; value : Expression.t option }
    | Delete of { location : Location.t; targets : Expression.t list }
    | Assign of {
        location : Location.t;
        targets : Expression.t list;
        value : Expression.t;
        type_comment : string option;
      }
    | TypeAlias of {
        location : Location.t;
        name : Expression.t;
        type_params : TypeParam.t list;
        value : Expression.t;
      }
    | AugAssign of {
        location : Location.t;
        target : Expression.t;
        op : BinaryOperator.t;
        value : Expression.t;
      }
    | AnnAssign of {
        location : Location.t;
        target : Expression.t;
        annotation : Expression.t;
        value : Expression.t option;
        simple : bool;
      }
    | For of {
        location : Location.t;
        target : Expression.t;
        iter : Expression.t;
        body : t list;
        orelse : t list;
        type_comment : string option;
      }
    | AsyncFor of {
        location : Location.t;
        target : Expression.t;
        iter : Expression.t;
        body : t list;
        orelse : t list;
        type_comment : string option;
      }
    | While of { location : Location.t; test : Expression.t; body : t list; orelse : t list }
    | If of { location : Location.t; test : Expression.t; body : t list; orelse : t list }
    | With of {
        location : Location.t;
        items : WithItem.t list;
        body : t list;
        type_comment : string option;
      }
    | AsyncWith of {
        location : Location.t;
        items : WithItem.t list;
        body : t list;
        type_comment : string option;
      }
    | Match of { location : Location.t; subject : Expression.t; cases : MatchCase.t list }
    | Raise of { location : Location.t; exc : Expression.t option; cause : Expression.t option }
    | Try of {
        location : Location.t;
        body : t list;
        handlers : ExceptionHandler.t list;
        orelse : t list;
        finalbody : t list;
      }
    | TryStar of {
        location : Location.t;
        body : t list;
        handlers : ExceptionHandler.t list;
        orelse : t list;
        finalbody : t list;
      }
    | Assert of { location : Location.t; test : Expression.t; msg : Expression.t option }
    | Import of { location : Location.t; names : ImportAlias.t list }
    | ImportFrom of {
        location : Location.t;
        module_ : Identifier.t option;
        names : ImportAlias.t list;
        level : int;
      }
    | Global of { location : Location.t; names : Identifier.t list }
    | Nonlocal of { location : Location.t; names : Identifier.t list }
    | Expr of { location : Location.t; value : Expression.t }
    | Pass of { location : Location.t }
    | Break of { location : Location.t }
    | Continue of { location : Location.t }
  [@@deriving sexp, compare, hash, make]
end

module TypeIgnore = struct
  type t = { lineno : int; tag : string option } [@@deriving sexp, compare, hash, make]
end

module Module = struct
  type t = { body : Statement.t list; type_ignores : TypeIgnore.t list }
  [@@deriving sexp, compare, hash, make]
end

module FunctionType = struct
  type t = { argtypes : Expression.t list; returns : Expression.t }
  [@@deriving sexp, compare, hash, make]
end

module MakeTaglessFinal = struct
  let argument ~location ~identifier ~annotation ~type_comment =
    Argument.make_t ~location ~identifier ?annotation ?type_comment ()

  let arguments ~posonlyargs ~args ~vararg ~kwonlyargs ~kw_defaults ~kwarg ~defaults =
    Arguments.make_t ~posonlyargs ~args ?vararg ~kwonlyargs ~kw_defaults ?kwarg ~defaults ()

  let binary_operator =
    let open BinaryOperator in
    TaglessFinal.BinaryOperator.make ~add:(make_add_of_t ()) ~sub:(make_sub_of_t ())
      ~mult:(make_mult_of_t ()) ~matmult:(make_matmult_of_t ()) ~div:(make_div_of_t ())
      ~mod_:(make_mod_of_t ()) ~pow:(make_pow_of_t ()) ~lshift:(make_lshift_of_t ())
      ~rshift:(make_rshift_of_t ()) ~bitor:(make_bitor_of_t ()) ~bitxor:(make_bitxor_of_t ())
      ~bitand:(make_bitand_of_t ()) ~floordiv:(make_floordiv_of_t ()) ()

  let boolean_operator =
    let open BooleanOperator in
    TaglessFinal.BooleanOperator.make ~and_:(make_and_of_t ()) ~or_:(make_or_of_t ()) ()

  let comparison_operator =
    let open ComparisonOperator in
    TaglessFinal.ComparisonOperator.make ~eq:(make_eq_of_t ()) ~noteq:(make_noteq_of_t ())
      ~lt:(make_lt_of_t ()) ~lte:(make_lte_of_t ()) ~gt:(make_gt_of_t ()) ~gte:(make_gte_of_t ())
      ~is:(make_is_of_t ()) ~isnot:(make_isnot_of_t ()) ~in_:(make_in_of_t ())
      ~notin:(make_notin_of_t ()) ()

  let comprehension ~target ~iter ~ifs ~is_async =
    Comprehension.make_t ~target ~iter ~ifs ~is_async ()

  let constant =
    let open Constant in
    TaglessFinal.Constant.make ~none:(make_none_of_t ()) ~false_:(make_false_of_t ())
      ~true_:(make_true_of_t ()) ~ellipsis:(make_ellipsis_of_t ()) ~integer:make_integer_of_t
      ~big_integer:make_big_integer_of_t ~float_:make_float_of_t ~complex:make_complex_of_t
      ~string_:make_string_of_t ~byte_string:make_byte_string_of_t ()

  let exception_handler ~location ~type_ ~name ~body =
    ExceptionHandler.make_t ~location ?type_ ?name ~body ()

  let expression =
    let open Expression in
    TaglessFinal.Expression.make
      ~bool_op:(fun ~location ~op ~values -> make_boolop_of_t ~location ~op ~values ())
      ~named_expr:(fun ~location ~target ~value -> make_namedexpr_of_t ~location ~target ~value ())
      ~bin_op:(fun ~location ~left ~op ~right -> make_binop_of_t ~location ~left ~op ~right ())
      ~unary_op:(fun ~location ~op ~operand -> make_unaryop_of_t ~location ~op ~operand ())
      ~lambda:(fun ~location ~args ~body -> make_lambda_of_t ~location ~args ~body ())
      ~if_exp:(fun ~location ~test ~body ~orelse ->
        make_ifexp_of_t ~location ~test ~body ~orelse ())
      ~dict:(fun ~location ~keys ~values -> make_dict_of_t ~location ~keys ~values ())
      ~set:(fun ~location ~elts -> make_set_of_t ~location ~elts ())
      ~list_comp:(fun ~location ~elt ~generators ->
        make_listcomp_of_t ~location ~elt ~generators ())
      ~set_comp:(fun ~location ~elt ~generators -> make_setcomp_of_t ~location ~elt ~generators ())
      ~dict_comp:(fun ~location ~key ~value ~generators ->
        make_dictcomp_of_t ~location ~key ~value ~generators ())
      ~generator_exp:(fun ~location ~elt ~generators ->
        make_generatorexp_of_t ~location ~elt ~generators ())
      ~await:(fun ~location ~value -> make_await_of_t ~location ~value ())
      ~yield:(fun ~location ~value -> make_yield_of_t ~location ?value ())
      ~yield_from:(fun ~location ~value -> make_yieldfrom_of_t ~location ~value ())
      ~compare:(fun ~location ~left ~ops ~comparators ->
        make_compare_of_t ~location ~left ~ops ~comparators ())
      ~call:(fun ~location ~func ~args ~keywords ->
        make_call_of_t ~location ~func ~args ~keywords ())
      ~formatted_value:(fun ~location ~value ~conversion ~format_spec ->
        make_formattedvalue_of_t ~location ~value ~conversion ?format_spec ())
      ~joined_str:(fun ~location ~values -> make_joinedstr_of_t ~location ~values ())
      ~constant:(fun ~location ~value ~kind -> make_constant_of_t ~location ~value ?kind ())
      ~attribute:(fun ~location ~value ~attr ~ctx ->
        make_attribute_of_t ~location ~value ~attr ~ctx ())
      ~subscript:(fun ~location ~value ~slice ~ctx ->
        make_subscript_of_t ~location ~value ~slice ~ctx ())
      ~starred:(fun ~location ~value ~ctx -> make_starred_of_t ~location ~value ~ctx ())
      ~name:(fun ~location ~id ~ctx -> make_name_of_t ~location ~id ~ctx ())
      ~list:(fun ~location ~elts ~ctx -> make_list_of_t ~location ~elts ~ctx ())
      ~tuple:(fun ~location ~elts ~ctx -> make_tuple_of_t ~location ~elts ~ctx ())
      ~slice:(fun ~location ~lower ~upper ~step -> make_slice_of_t ~location ?lower ?upper ?step ())
      ()

  let expression_context =
    let open ExpressionContext in
    TaglessFinal.ExpressionContext.make ~load:(make_load_of_t ()) ~store:(make_store_of_t ())
      ~del:(make_del_of_t ()) ()

  let identifier id = Identifier.make_t id ()
  let import_alias ~location ~name ~asname = ImportAlias.make_t ~location ~name ?asname ()
  let keyword ~location ~arg ~value = Keyword.make_t ~location ?arg ~value ()
  let location ~start ~stop = Location.make_t ~start ~stop ()
  let match_case ~pattern ~guard ~body = MatchCase.make_t ~pattern ?guard ~body ()
  let module_ ~body ~type_ignores = Module.make_t ~body ~type_ignores ()
  let function_type ~argtypes ~returns = FunctionType.make_t ~argtypes ~returns ()

  let type_param =
    let open TypeParam in
    TaglessFinal.TypeParam.make
      ~type_var:(fun ~location ~name ~bound -> make_typevar_of_t ~location ~name ?bound ())
      ~param_spec:(fun ~location ~name -> make_paramspec_of_t ~location ~name ())
      ~type_var_tuple:(fun ~location ~name -> make_typevartuple_of_t ~location ~name ())
      ()

  let pattern =
    let open Pattern in
    TaglessFinal.Pattern.make
      ~match_value:(fun ~location ~value -> make_matchvalue_of_t ~location ~value ())
      ~match_singleton:(fun ~location ~value -> make_matchsingleton_of_t ~location ~value ())
      ~match_sequence:(fun ~location ~patterns -> make_matchsequence_of_t ~location ~patterns ())
      ~match_mapping:(fun ~location ~keys ~patterns ~rest ->
        make_matchmapping_of_t ~location ~keys ~patterns ?rest ())
      ~match_class:(fun ~location ~cls ~patterns ~kwd_attrs ~kwd_patterns ->
        make_matchclass_of_t ~location ~cls ~patterns ~kwd_attrs ~kwd_patterns ())
      ~match_star:(fun ~location ~name -> make_matchstar_of_t ~location ?name ())
      ~match_as:(fun ~location ~pattern ~name -> make_matchas_of_t ~location ?pattern ?name ())
      ~match_or:(fun ~location ~patterns -> make_matchor_of_t ~location ~patterns ())
      ()

  let position ~line ~column = Position.make_t ~line ~column ()

  let statement =
    let open Statement in
    TaglessFinal.Statement.make
      ~function_def:(fun
          ~location ~name ~args ~body ~decorator_list ~returns ~type_comment ~type_params ->
        make_functiondef_of_t ~location ~name ~args ~body ~decorator_list ?returns ?type_comment
          ~type_params ())
      ~async_function_def:(fun
          ~location ~name ~args ~body ~decorator_list ~returns ~type_comment ~type_params ->
        make_asyncfunctiondef_of_t ~location ~name ~args ~body ~decorator_list ?returns
          ?type_comment ~type_params ())
      ~class_def:(fun ~location ~name ~bases ~keywords ~body ~decorator_list ~type_params ->
        make_classdef_of_t ~location ~name ~bases ~keywords ~body ~decorator_list ~type_params ())
      ~return:(fun ~location ~value -> make_return_of_t ~location ?value ())
      ~delete:(fun ~location ~targets -> make_delete_of_t ~location ~targets ())
      ~assign:(fun ~location ~targets ~value ~type_comment ->
        make_assign_of_t ~location ~targets ~value ?type_comment ())
      ~type_alias:(fun ~location ~name ~type_params ~value ->
        make_typealias_of_t ~location ~name ~type_params ~value ())
      ~aug_assign:(fun ~location ~target ~op ~value ->
        make_augassign_of_t ~location ~target ~op ~value ())
      ~ann_assign:(fun ~location ~target ~annotation ~value ~simple ->
        make_annassign_of_t ~location ~target ~annotation ?value ~simple ())
      ~for_:(fun ~location ~target ~iter ~body ~orelse ~type_comment ->
        make_for_of_t ~location ~target ~iter ~body ~orelse ?type_comment ())
      ~async_for:(fun ~location ~target ~iter ~body ~orelse ~type_comment ->
        make_asyncfor_of_t ~location ~target ~iter ~body ~orelse ?type_comment ())
      ~while_:(fun ~location ~test ~body ~orelse ->
        make_while_of_t ~location ~test ~body ~orelse ())
      ~if_:(fun ~location ~test ~body ~orelse -> make_if_of_t ~location ~test ~body ~orelse ())
      ~with_:(fun ~location ~items ~body ~type_comment ->
        make_with_of_t ~location ~items ~body ?type_comment ())
      ~async_with:(fun ~location ~items ~body ~type_comment ->
        make_asyncwith_of_t ~location ~items ~body ?type_comment ())
      ~match_:(fun ~location ~subject ~cases -> make_match_of_t ~location ~subject ~cases ())
      ~raise_:(fun ~location ~exc ~cause -> make_raise_of_t ~location ?exc ?cause ())
      ~try_:(fun ~location ~body ~handlers ~orelse ~finalbody ->
        make_try_of_t ~location ~body ~handlers ~orelse ~finalbody ())
      ~try_star:(fun ~location ~body ~handlers ~orelse ~finalbody ->
        make_trystar_of_t ~location ~body ~handlers ~orelse ~finalbody ())
      ~assert_:(fun ~location ~test ~msg -> make_assert_of_t ~location ~test ?msg ())
      ~import:(fun ~location ~names -> make_import_of_t ~location ~names ())
      ~import_from:(fun ~location ~module_ ~names ~level ->
        make_importfrom_of_t ~location ?module_ ~names ~level ())
      ~global:(fun ~location ~names -> make_global_of_t ~location ~names ())
      ~nonlocal:(fun ~location ~names -> make_nonlocal_of_t ~location ~names ())
      ~expr:(fun ~location ~value -> make_expr_of_t ~location ~value ())
      ~pass:(fun ~location -> make_pass_of_t ~location ())
      ~break:(fun ~location -> make_break_of_t ~location ())
      ~continue:(fun ~location -> make_continue_of_t ~location ())
      ()

  let type_ignore ~lineno ~tag = TypeIgnore.make_t ~lineno ~tag ()

  let unary_operator =
    let open UnaryOperator in
    TaglessFinal.UnaryOperator.make ~invert:(make_invert_of_t ()) ~not_:(make_not_of_t ())
      ~uadd:(make_uadd_of_t ()) ~usub:(make_usub_of_t ()) ()

  let with_item ~context_expr ~optional_vars = WithItem.make_t ~context_expr ?optional_vars ()
end

let make_tagless_final =
  let open MakeTaglessFinal in
  TaglessFinal.make ~argument ~arguments ~binary_operator ~boolean_operator ~comparison_operator
    ~comprehension ~constant ~exception_handler ~expression ~expression_context ~function_type
    ~identifier ~import_alias ~keyword ~location ~match_case ~module_ ~pattern ~position ~statement
    ~type_ignore ~type_param ~unary_operator ~with_item
