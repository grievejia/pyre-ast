(** This module provides tagless-final interfaces for the syntax of Python.

    {{:http://okmij.org/ftp/tagless-final/JFP.pdf} Tagless-final style}, also known as
    {{:http://www.cs.utexas.edu/~wcook/Drafts/2012/ecoop2012.pdf} object algebra} in OOP contexts,
    is a programming techinque of embedding domain-specific languages in a typed functional host
    language. Reading the associated literature to get a deeper understanding of the concept is
    definitely encouraged. But we will also include a short explanation here in order to facilitate
    basic understandings of the APIs.

    Let's consider a small DSL containing both boolean and integer values as our running example.
    Conventional wisdom suggests the following patterns to construct and process the syntax of a
    language:

    {[
      (* Syntax of DSL *)
      type value = Int of int | Bool of bool

      type expression =
        | Constant of value
        | Add of expression * expression
        | IsEqual of expression * expression

      (* An evaluator of the DSL, as an example of a downstream processing logic of the DSL's
         syntax. *)
      let rec eval = function
        | Constant value -> value
        | Add (lhs, rhs) -> (
            match (eval lhs, eval rhs) with
            | Int lhs, Int rhs -> Int (lhs + rhs)
            | _ -> failwith "Runtime type error")
        | IsEqual (lhs, rhs) -> (
            match (eval lhs, eval rhs) with
            | Int lhs, Int rhs -> Bool (lhs = rhs)
            | Bool lhs, Bool rhs -> Bool (lhs = rhs)
            | _ -> failwith "Runtime type error")

      (* The program constructs an explicit AST for the DSL, then process it. *)

      let construct_ast () =
        IsEqual
          ( Constant (Bool true),
            IsEqual (Add (Constant (Int 2), Constant (Int 3)), Constant (Int 4)) )

      let _ =
        let syntax_tree = construct_ast () in
        eval syntax_tree
    ]}

    Note how the program first construct an explicit structure [syntax_tree] first, and then send
    this structure to downstream processing logic so it can get pattern-matched on. If the same
    logic gets re-written in the tagless-final style, it would roughly look like this:

    {[
      (* This definition is still needed, as the result type of [eval]. *)
      type value = Int of int | Bool of bool

      (* Syntax of DSL. Note the lack of concrete variant/record definitions -- syntax are 100%
         defined as a collection of free-functions. *)
      type 'e expression = {
        constant : value -> 'e;
        add : 'e -> 'e -> 'e;
        is_equal : 'e -> 'e -> 'e;
      }

      (* An evaluator of the DSL. Note the lack of explicit pattern match on the syntax tree (which
         is why this style is named "tagless"). *)
      let eval : value expression =
        let constant v = v in
        let add lhs rhs =
          match (lhs, rhs) with
          | Int lhs, Int rhs -> Int (lhs + rhs)
          | _ -> failwith "Runtime type error"
        in
        let is_equal lhs rhs =
          match (lhs, rhs) with
          | Int lhs, Int rhs -> Bool (lhs = rhs)
          | Bool lhs, Bool rhs -> Bool (lhs = rhs)
          | _ -> failwith "Runtime type error"
        in
        { constant; add; is_equal }

      (* The program does not constructs an explicit AST for the DSL. Processing of the DSL is
         performed exactly when the DSL gets constructed. *)

      let construct_and_eval { constant; add; is_equal } =
        is_equal (constant (Bool true))
          (is_equal (add (constant (Int 2)) (constant (Int 3))) (constant (Int 4)))

      let _ = construct_and_eval eval
    ]}

    Note how the intermediate step of constructing an explicit [syntax_tree] structure is completely
    eliminated -- we specify the downstream processing logic on top of the syntatical structure of
    the DSL directly as free functions, and invoke them immediately when the corrsponding DSL
    structure is constructed. In other words, the processing logic somehow gets "dependency
    injected" into the construction logic.

    This seemlying unintuitive style of programming actually provides two main benefits over the
    conventional alternative:

    - It solves the {{:https://en.wikipedia.org/wiki/Expression_problem} expression problem}. That
      is, both extending the syntax of the DSL and adding new operations of the DSL can be done in a
      type-safe way that does not requires the developer to modify pre-existing logic. In contrast,
      in non-tagless-final approaches, if the syntax tree is represented as ADT then adding new
      operations would require modifying old logic, and if the syntax tree is represented as class
      hierarchy then adding new syntax variant would require modifying old logic.

    - It offers more flexibility than the tranditional ADT approach in the sense that one can easily
      add stronger type-level constraints to the tagless-final interfaces. For example, we can tweak
      the definition of [expression] in our DSL to syntactically rule out terms that cannot
      statically type check:

    {[
      (* The intention here is to use the first parameter as a phantom type, i.e. the actual
         definition of this type is going to be [type (_, 'a) typed = 'a] *)
      type ('phantom, 'a) typed

      type 'v value = { intv : int -> (int, 'v) typed; boolv : bool -> (bool, 'v) typed }

      type ('e, 'v) expression = {
        constant : 't. ('t, 'v) typed -> ('v, 'e) typed;
        (* Note how we only allow adding terms with [int] types. *)
        add : (int, 'e) typed -> (int, 'e) typed -> (int, 'e) typed;
        (* Note how we only allow comparing terms with the same type. *)
        is_equal : 't. ('t, 'e) typed -> ('t, 'e) typed -> (bool, 'e) typed;
      }
    ]}

    Doing the same thing with the non-tagless-final style requires advanced language features such
    as {{:https://ocaml.org/manual/gadts.html} GADT}, which is not always available in the host
    language. Besides, GADT often has its own set of nuisances even in a host language that supports
    it. Granted, such a flexibility is not actively exploited by this library. But the point that
    tagless-final style allows us to pursue this potential direction without too much pain still
    remains.

    The main type this module offers, {!type: TaglessFinal.t}, works just like the [expression] type
    appeared in the above example: it is nothing more than a collection of free functions on which
    downstream syntax processors can be defined. Once it is constructed with {!val:
    TaglessFinal.make}, one can then pass it onto various paring APIs in {!module:
    Parser.TaglessFinal}, which serves simliar purpose as the [construct_and_eval] function in our
    running example.

    Keeping the structure of {!type: TaglessFinal.t} fully authentic to Python's official
    {{:https://docs.python.org/3.10/library/ast.html} [ast]} module is an explicit design goal for
    this library. Even if Python's own [ast] representation may exhibit some obvious issues or
    inconsistencies, it is not the job of this module to fix them, for both philosophical and
    techinical reasons. If certain design of [ast] is undesirable, the recommended way to address
    those is to write downstream conversion logic. *)
module TaglessFinal : sig
  (** A position is a pair of integers representing line number and column numbers.

      Line numbers start from 1 and column numbers start from 0. In other words, first character in
      the input string has location of line 1, column 0. *)
  module Position : sig
    type 'a t = line:int -> column:int -> 'a
  end

  (** A location is a pair of {!type: Position.t} representing a range for a given token.

      Ranges are inclusive in their start position and exclusive in their end position. For example,
      identifier [foo] at the begining of an input string has a location from (line 1, column 0) to
      (line 1, column 3). *)
  module Location : sig
    type ('position, 'location) t = start:'position -> stop:'position -> 'location
  end

  (** This module provide a type that represents Python
      {{:https://docs.python.org/3/reference/lexical_analysis.html#identifiers} identifiers}.

      Due to technical limitation, currently this library does not support parsing non-ascii
      identifiers as specified in {{:https://www.python.org/dev/peps/pep-3131/} PEP 3131}. *)
  module Identifier : sig
    type 'a t = string -> 'a
  end

  (** This module provides a type that represents Python constant value. *)
  module Constant : sig
    type 'a t = private {
      none : 'a;  (** Represents the value [None]. *)
      false_ : 'a;  (** Represents the value [False]. *)
      true_ : 'a;  (** Represents the value [True]. *)
      ellipsis : 'a;  (** Represents the value [...]. *)
      integer : int -> 'a;
          (** Represents integer literals whose value can be represented by OCaml {!type: int} type
              (e.g. [42]). *)
      big_integer : string -> 'a;
          (** Represents integer literals whose value cannot be represented by OCaml {!type: int}
              type. The integer will be stored as a string instead. *)
      float_ : float -> 'a;  (** Represents Python float literals (e.g. [4.2]). *)
      complex : float -> 'a;
          (** Represents Python complex literals (e.g. [4.2j]). The float argument is the imaginary
              part of the complex number.

              The reason why we do not need to keep track of the real part is that in Python,
              complex numbers with non-zero real part are represented as compound expressions (e.g.
              [1+2j] will be parsed as a binary expression that adds up constant [1] and [2j]) as
              opposed to literals *)
      string_ : string -> 'a;
          (** Represents Python string literals (e.g. ["derp"]).

              At OCaml level, the value of the string literal is a UTF-8 decoded byte array of the
              original string literal in Python. If the original literal cannot be UTF-8 decoded,
              the {{:https://docs.python.org/3/library/codecs.html#error-handlers}
              [backslahreplace]} error handler is used, which replaces the un-decodable parts with
              backslashed escape sequence. *)
      byte_string : string -> 'a;
          (** Represents Python string literals (e.g. [b"derp"]).

              Unlike non-ascii identifiers, non-ascii bytestring literals can be parsed properly by
              this library. That also means it has no trouble handling strings with embedded null
              bytes. *)
    }

    val make :
      none:'a ->
      false_:'a ->
      true_:'a ->
      ellipsis:'a ->
      integer:(int -> 'a) ->
      big_integer:(string -> 'a) ->
      float_:(float -> 'a) ->
      complex:(float -> 'a) ->
      string_:(string -> 'a) ->
      byte_string:(string -> 'a) ->
      unit ->
      'a t
    (** Constructor of {!type: t}. *)
  end

  (** This module provides a type that represents Python expression contexts.

      Expression contexts are mainly used to syntactically differentiate among expressions that will
      be read at runtime, those that will be written at runtime, and those that will be [del]ed at
      runtime.

      As of Python 3.10, there are 6 kinds of expressions with an explicit context field:

      - {!field: Expression.name}
      - {!field: Expression.attribute}
      - {!field: Expression.subscript}
      - {!field: Expression.list}
      - {!field: Expression.tuple}
      - {!field: Expression.starred}

      These expressions are allowed to appear at the following "target" positions:

      - The [targets] field in {!field: Statement.assign}.
      - The [target] field in {!field: Statement.aug_assign} and {!field: Statement.ann_assign}
        (except for list, tuple, and starred expression).
      - The [targets] field in {!field: Statement.delete} (except for starred expression).
      - The [targets] field in {!field: Statement.for_} (except for starred expression).
      - The [targets] field in {!field: Statement.with_} (except for starred expression). *)
  module ExpressionContext : sig
    type 'a t = private { load : 'a; store : 'a; del : 'a }

    val make : load:'a -> store:'a -> del:'a -> unit -> 'a t
    (** Constructor of {!type: t}. *)
  end

  (** This module provides a type that represents Python boolean operators. *)
  module BooleanOperator : sig
    type 'a t = private {
      and_ : 'a;  (** Represents Python operator [and]. *)
      or_ : 'a;  (** Represents Python operator [or]. *)
    }

    val make : and_:'a -> or_:'a -> unit -> 'a t
    (** Constructor of {!type: t}. *)
  end

  (** This module provides a type that represents Python numerical binary operators. *)
  module BinaryOperator : sig
    type 'a t = private {
      add : 'a;  (** Represents Python operator [+]. *)
      sub : 'a;  (** Represents Python operator [-]. *)
      mult : 'a;  (** Represents Python operator [*]. *)
      matmult : 'a;
          (** Represents Python operator [@], as specified in
              {{:https://www.python.org/dev/peps/pep-0465/} PEP 465}.

              As of Python 3.10, no builtin Python types implement this operator. *)
      div : 'a;  (** Represents Python operator [/]. *)
      mod_ : 'a;  (** Represents Python operator [%]. *)
      pow : 'a;  (** Represents Python operator [**]. *)
      lshift : 'a;  (** Represents Python operator [<<]. *)
      rshift : 'a;  (** Represents Python operator [>>]. *)
      bitor : 'a;  (** Represents Python operator [|]. *)
      bitxor : 'a;  (** Represents Python operator [^]. *)
      bitand : 'a;  (** Represents Python operator [&]. *)
      floordiv : 'a;  (** Represents Python operator [//]. *)
    }

    val make :
      add:'a ->
      sub:'a ->
      mult:'a ->
      matmult:'a ->
      div:'a ->
      mod_:'a ->
      pow:'a ->
      lshift:'a ->
      rshift:'a ->
      bitor:'a ->
      bitxor:'a ->
      bitand:'a ->
      floordiv:'a ->
      unit ->
      'a t
    (** Constructor of {!type: t}. *)
  end

  (** This module provides a type that represents Python unary operators. *)
  module UnaryOperator : sig
    type 'a t = private {
      invert : 'a;  (** Represents Python operator [~]. *)
      not_ : 'a;  (** Represents Python operator [not]. *)
      uadd : 'a;  (** Represents Python operator [+] (unary). *)
      usub : 'a;  (** Represents Python operator [-] (unary). *)
    }

    val make : invert:'a -> not_:'a -> uadd:'a -> usub:'a -> unit -> 'a t
    (** Constructor of {!type: t}. *)
  end

  (** This module provides a type that represents Python comparison operators. *)
  module ComparisonOperator : sig
    type 'a t = {
      eq : 'a;  (** Represents Python operator [==]. *)
      noteq : 'a;  (** Represents Python operator [!=]. *)
      lt : 'a;  (** Represents Python operator [<]. *)
      lte : 'a;  (** Represents Python operator [<]. *)
      gt : 'a;  (** Represents Python operator [>]. *)
      gte : 'a;  (** Represents Python operator [<=]. *)
      is : 'a;  (** Represents Python operator [is]. *)
      isnot : 'a;  (** Represents Python operator [is not]. *)
      in_ : 'a;  (** Represents Python operator [in]. *)
      notin : 'a;  (** Represents Python operator [not in] *)
    }

    val make :
      eq:'a ->
      noteq:'a ->
      lt:'a ->
      lte:'a ->
      gt:'a ->
      gte:'a ->
      is:'a ->
      isnot:'a ->
      in_:'a ->
      notin:'a ->
      unit ->
      'a t
    (** Constructor of {!type: t}. *)
  end

  (** This module provides a type that represents Python comprehension structures.

      Example: [... async for x, y in z if x > 0 if y < 0]

      In this case, [target] will be [(x, y)], [iter] will be [z], [ifs] will be a list
      [\[x>0, y<0\]], and [is_async] will be [true]. *)
  module Comprehension : sig
    type ('expr, 'comprehension) t =
      target:'expr -> iter:'expr -> ifs:'expr list -> is_async:bool -> 'comprehension
  end

  (** This module provides a type that represents keyword arguments at each callsite.

      [arg] is the name of the keyword argument, and value is the value specified for that argument.

      Reason why the type of [arg] is an [option]: If a callsite contains [**some_kwarg] as
      argument, it will also be treated as a keyword argument, with [arg] being unset and [value]
      being [some_kwarg]. *)
  module Keyword : sig
    type ('expr, 'identifier, 'location, 'keyword) t =
      location:'location -> arg:'identifier option -> value:'expr -> 'keyword
  end

  (** This module provides a type that represents single parameter at each function definition site.

      [identifier] is the name of the parameter. [annotation] is the (optionally specified) type
      annotation for the parameter. [type_comment] can be optionally used in place of [annotation]
      for Python2-style type annotation.

      Example:

      {v def foo(x: int): ... v}

      Here for parameter [x], its annotation will be the [int] part and [type_comment] will be
      unset.

      Another example:

      {v
def foo(
  x  # type: int
): ...
      v}

      Here for parameter [x], its annotation will be unset and [type_comment] will be ["int"]. *)
  module Argument : sig
    type ('expr, 'identifier, 'location, 'arg) t =
      location:'location ->
      identifier:'identifier ->
      annotation:'expr option ->
      type_comment:string option ->
      'arg
  end

  (** This module provides a type that represents a parameter list at each function definition site.

      This structure is complicated because Python needs to distinguish among 5 kinds of parameters:

      - Regular parameters, which can be specified at callsites both positionally and as keyword
        arguments.
      - Positional-only parameters, which can only be specified at callsites positionally (see
        {{:https://www.python.org/dev/peps/pep-0570/} PEP 570}).
      - Keyword-only parameters, which can only be specified at callsites as keyword arguments (see
        {{:https://www.python.org/dev/peps/pep-3102/} PEP 3102}).
      - Special vararg parameter of the form [*arg], representing the "rest" of both the
        positional-only and regular parameters.
      - Special kwarg parameter of the form [**kwarg], representing the "rest" of both the regular
        and keyword-only parameters.

      Parameter lists in Python can be generally chopped into three chunks: the positional-only
      chunk, followed by the regular chunk, followed by the keyword-only chunk. Positional-only
      chunk and regular chunk are separated by [/], and regular chunk and keyword-only chunk are
      separated by [*].

      Example:

      {v def foo(a, b=1, /, c=2, *, e, f=3, **g): ... v}

      Here [a] and [b] are positional-only parameters. [c] is a regular parameter. [e] and [f] are
      keyword-only parameters. There is no special vararg parameter, and there is one special kwarg
      parameter [g]. *)
  module Arguments : sig
    type ('arg, 'expr, 'arguments) t =
      posonlyargs:'arg list ->
      args:'arg list ->
      vararg:'arg option ->
      kwonlyargs:'arg list ->
      kw_defaults:'expr option list ->
      kwarg:'arg option ->
      defaults:'expr list ->
      'arguments
    (** Structure of this type mirrors the structure of the parameter list mentioned in the
        documentation of the containing module:

        - [posonlyargs] represents the list of positional-only parameters.
        - [args] represents the list of regular parameters.
        - [kwonlyarg] represents the list of keyword-only parameters.
        - [vararg] represents the special vararg parameter, if there is one.
        - [kwarg] represents the special kwarg parameter, if there is one.
        - [defaults] represents the list of default values for both positional-only and regular
          parameters. Note that in Python, regular parameters always comes after positional-only
          parameters. Also, if a parameter has a default value, then every positional-only/regular
          parameter that comes after it must have a default value as well. Hence there is no
          ambiguity regarding which default value in the [defaults] list corresponds to which
          parameter -- with the aforementioned rules there is only one possible interpretation.
        - [kw_defaults] represents the list of default values for keyword-only parameters. This list
          should always have the same length with [kwonlyarg]. If a keyword-only parameter does not
          have a default value, the corresponding element in [kw_defaults] will be unset. *)
  end

  (** This module provides a type that represents a Python expression. *)
  module Expression : sig
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
         t =
      private {
      bool_op : location:'location -> op:'bool_op -> values:'expr list -> 'expr;
          (** Represents a boolean expression.

              - Example: [a and b]
              - Example: [a or b or c] *)
      named_expr : location:'location -> target:'expr -> value:'expr -> 'expr;
          (** Represents an assignment expression, a.k.a. the "walrus" operator. See
              {{:https://www.python.org/dev/peps/pep-0572/} PEP 572}.

              - Example: [(a := b)] *)
      bin_op : location:'location -> left:'expr -> op:'bin_op -> right:'expr -> 'expr;
          (** Represents a numerical binary expression.

              - Example: [a + b] *)
      unary_op : location:'location -> op:'unary_op -> operand:'expr -> 'expr;
          (** Represents an unary expression.

              - Example: [not a] *)
      lambda : location:'location -> args:'arguments -> body:'expr -> 'expr;
          (** Represents a lambda expression. See {{:https://www.python.org/dev/peps/pep-0312/} PEP
              312}.

              - Example: [lambda x, /, y, *, z: x + y + z] *)
      if_exp : location:'location -> test:'expr -> body:'expr -> orelse:'expr -> 'expr;
          (** Represents a conditional expression. See {{:https://www.python.org/dev/peps/pep-0308/}
              PEP 308}.

              - Example: [x if x > 0 else None] *)
      dict : location:'location -> keys:'expr option list -> values:'expr list -> 'expr;
          (** Represents a Python dictionary display.

              - Example: [{}]
              - Example: [{ "a": 1, "b": 2 }]
              - Example: [{ "a": 1, **b }]. The element [**b] here will be represented with a [None]
                key and a value of [b]. *)
      set : location:'location -> elts:'expr list -> 'expr;
          (** Represents a Python set display.

              - Example: [{ "a", "b" }] *)
      list_comp : location:'location -> elt:'expr -> generators:'comprehension list -> 'expr;
          (** Represents a list comprehension.

              - Example: [\[y for x in l if len(x) > 1 for y in x if y < 4\]] *)
      set_comp : location:'location -> elt:'expr -> generators:'comprehension list -> 'expr;
          (** Represents a set comprehension.

              - Example: [{x for x in l}] *)
      dict_comp :
        location:'location -> key:'expr -> value:'expr -> generators:'comprehension list -> 'expr;
          (** Represents a dict comprehension.

              - Example: [{x:y for x, y in l}] *)
      generator_exp : location:'location -> elt:'expr -> generators:'comprehension list -> 'expr;
          (** Represents a generator expression.

              - Example: [(x for x in l)] *)
      await : location:'location -> value:'expr -> 'expr;
          (** Represents an await expression.

              - Example: [await foo] *)
      yield : location:'location -> value:'expr option -> 'expr;
          (** Represents a yield expression.

              - Example: [(yield)]
              - Example: [(yield x)] *)
      yield_from : location:'location -> value:'expr -> 'expr;
          (** Represents a yield from expression.

              - Example: [(yield from x)] *)
      compare :
        location:'location -> left:'expr -> ops:'compare list -> comparators:'expr list -> 'expr;
          (** Represents a comparison expression.

              - Example: [x is y]
              - Example: [x > y > z] *)
      call : location:'location -> func:'expr -> args:'expr list -> keywords:'keyword list -> 'expr;
          (** Represents a call expression, where [args] is the list of positionally-specified
              arguments, and [keywords] is the list of keyword-specified arguments.

              - Example: [foo(x, y=1)]
              - Example: [foo(x, *y, **z)] *)
      formatted_value :
        location:'location -> value:'expr -> conversion:int -> format_spec:'expr option -> 'expr;
          (** Represents the expression component of an f-string. See
              {{:https://www.python.org/dev/peps/pep-0498/} PEP 498}. [conversion] is an integer
              representing the type conversion operator:

              - [-1] means no formatting.
              - [115] means [!s] string formatting.
              - [114] means [!r] repr formatting
              - [97] means [!a] ascii formatting

              [format_spec] represents the format specifier. This is always going to be a
              [joined_str] expression.

              - Example: [f"derp={a+b:{foo}}"]. The entire expression will be a [joined_str] but the
                [a+b] part will be represented as a [formatted_value] with the [foo] part as its
                format specifier. *)
      joined_str : location:'location -> values:'expr list -> 'expr;
          (** Represents an f-string. See {{:https://www.python.org/dev/peps/pep-0498/} PEP 498}.

              - Example: [f"a={b}"] *)
      constant : location:'location -> value:'constant -> kind:string option -> 'expr;
          (** Represents a constant expression.

              - Example: [42] *)
      attribute :
        location:'location -> value:'expr -> attr:'identifier -> ctx:'expr_context -> 'expr;
          (** Represents an attribute access expression.

              - Example: [a.b] *)
      subscript : location:'location -> value:'expr -> slice:'expr -> ctx:'expr_context -> 'expr;
          (** Represents a subscript access expression.

              - Example: [a\[b\]] *)
      starred : location:'location -> value:'expr -> ctx:'expr_context -> 'expr;
          (** Represents an starred expression, which is used to represent iterable unpacking
              (inside [call], [list], or comprehension expressions, for example). See
              {{:https://www.python.org/dev/peps/pep-0448/} PEP 448}. *)
      name : location:'location -> id:'identifier -> ctx:'expr_context -> 'expr;
          (** Represents a simple identifier.

              - Example: [a] *)
      list : location:'location -> elts:'expr list -> ctx:'expr_context -> 'expr;
          (** Represents a Python list display.

              - Example: [\[\]]
              - Example: [\[a, b\]]
              - Example: [\[a, *b\]] *)
      tuple : location:'location -> elts:'expr list -> ctx:'expr_context -> 'expr;
          (** Represents a Python tuple display.

              - Example: [()]
              - Example: [(a, b)]
              - Example: [(a, *b)] *)
      slice :
        location:'location -> lower:'expr option -> upper:'expr option -> step:'expr option -> 'expr;
          (** Represents a slice expression. This kind of expression can only appear within the
              [slice] field of the [subscript] expression, either directly or as an element of a
              [tuple] expression.

              - Example: [a\[:\]]
              - Example: [a\[1:2, 3:4\]] *)
    }

    val make :
      bool_op:(location:'a -> op:'b -> values:'c list -> 'c) ->
      named_expr:(location:'a -> target:'c -> value:'c -> 'c) ->
      bin_op:(location:'a -> left:'c -> op:'d -> right:'c -> 'c) ->
      unary_op:(location:'a -> op:'e -> operand:'c -> 'c) ->
      lambda:(location:'a -> args:'f -> body:'c -> 'c) ->
      if_exp:(location:'a -> test:'c -> body:'c -> orelse:'c -> 'c) ->
      dict:(location:'a -> keys:'c option list -> values:'c list -> 'c) ->
      set:(location:'a -> elts:'c list -> 'c) ->
      list_comp:(location:'a -> elt:'c -> generators:'g list -> 'c) ->
      set_comp:(location:'a -> elt:'c -> generators:'g list -> 'c) ->
      dict_comp:(location:'a -> key:'c -> value:'c -> generators:'g list -> 'c) ->
      generator_exp:(location:'a -> elt:'c -> generators:'g list -> 'c) ->
      await:(location:'a -> value:'c -> 'c) ->
      yield:(location:'a -> value:'c option -> 'c) ->
      yield_from:(location:'a -> value:'c -> 'c) ->
      compare:(location:'a -> left:'c -> ops:'h list -> comparators:'c list -> 'c) ->
      call:(location:'a -> func:'c -> args:'c list -> keywords:'i list -> 'c) ->
      formatted_value:(location:'a -> value:'c -> conversion:int -> format_spec:'c option -> 'c) ->
      joined_str:(location:'a -> values:'c list -> 'c) ->
      constant:(location:'a -> value:'j -> kind:string option -> 'c) ->
      attribute:(location:'a -> value:'c -> attr:'k -> ctx:'l -> 'c) ->
      subscript:(location:'a -> value:'c -> slice:'c -> ctx:'l -> 'c) ->
      starred:(location:'a -> value:'c -> ctx:'l -> 'c) ->
      name:(location:'a -> id:'k -> ctx:'l -> 'c) ->
      list:(location:'a -> elts:'c list -> ctx:'l -> 'c) ->
      tuple:(location:'a -> elts:'c list -> ctx:'l -> 'c) ->
      slice:(location:'a -> lower:'c option -> upper:'c option -> step:'c option -> 'c) ->
      unit ->
      ('f, 'd, 'b, 'h, 'g, 'j, 'l, 'k, 'i, 'a, 'e, 'c) t
    (** Constructor of {!type: t}. *)
  end

  (** This module provides a type that represents a Python [with] item.

      Example: The statement [with (f() as a, g()): ...] contains 2 [with] items. The first one has
      [f()] as its [context_expr] and [a] as its [optional_vars]. The second one has [g()] as its
      [context_expr] and no [optional_vars].

      Note that [optional_vars] is "target"-like: It can only be one of a selected few kinds of
      expressions, and the corresponding expression context will always be set to {!field:
      ExpressionContext.store}. See documentation of {!module: ExpressionContext} for more details
      of which expressions are allowed. *)
  module WithItem : sig
    type ('expr, 'with_item) t = context_expr:'expr -> optional_vars:'expr option -> 'with_item
  end

  (** This module provides a type that represents a Python [import] item.

      [name] is the target of the import, and [asname] is the (optional) alias of the target. *)
  module ImportAlias : sig
    type ('identifier, 'location, 'alias) t =
      location:'location -> name:'identifier -> asname:'identifier option -> 'alias
  end

  (** This module provides a type that represents a Python exception handler.

      Example: [try ... except Foo as e: pass]. Here [type_] will be set to [Foo], [name] will be
      set to [e], and the [pass] part will be the [body]. *)
  module ExceptionHandler : sig
    type ('expr, 'identifier, 'location, 'stmt, 'except_handler) t =
      location:'location ->
      type_:'expr option ->
      name:'identifier option ->
      body:'stmt list ->
      'except_handler
  end

  (** This module provides a type that represents a branch of the Python [match] statement. See
      {{:https://www.python.org/dev/peps/pep-0622/} PEP 622}.

      [pattern] is the pattern of the branch. [guard] is the guard expression, if there is one.
      [body] is the body of the branch. *)
  module MatchCase : sig
    type ('expr, 'pattern, 'stmt, 'match_case) t =
      pattern:'pattern -> guard:'expr option -> body:'stmt list -> 'match_case
  end

  (** This module provides a type that represents a pattern for a given [match] branch. See
      {{:https://www.python.org/dev/peps/pep-0622/} PEP 622}. *)
  module Pattern : sig
    type ('constant, 'expr, 'identifier, 'location, 'pattern) t = private {
      match_value : location:'location -> value:'expr -> 'pattern;
          (** Represent a pattern that matches constants, except [None], [True], and [False]
              literal. *)
      match_singleton : location:'location -> value:'constant -> 'pattern;
          (** Represent a pattern that matches [None], [True], and [False] (i.e. literals that are
              compared by identity instead of equality). *)
      match_sequence : location:'location -> patterns:'pattern list -> 'pattern;
          (** Represent a pattern that matches a sequence of other patterns. *)
      match_mapping :
        location:'location ->
        keys:'expr list ->
        patterns:'pattern list ->
        rest:'identifier option ->
        'pattern;
          (** Represent a pattern that matches a map of patterns. *)
      match_class :
        location:'location ->
        cls:'expr ->
        patterns:'pattern list ->
        kwd_attrs:'identifier list ->
        kwd_patterns:'pattern list ->
        'pattern;
          (** Represent a pattern that destructuring arbitrary objects. *)
      match_star : location:'location -> name:'identifier option -> 'pattern;
          (** This pattern can only be nested in another [match_sequence] pattern, representing the
              "rest" of the sequence *)
      match_as :
        location:'location -> pattern:'pattern option -> name:'identifier option -> 'pattern;
          (** Represent a capture or wildcard pattern (e.g. [case x as y], [case x], or [case _]). *)
      match_or : location:'location -> patterns:'pattern list -> 'pattern;
          (** Represent a pattern that matches a combination of other patterns. *)
    }

    val make :
      match_value:(location:'a -> value:'b -> 'c) ->
      match_singleton:(location:'a -> value:'d -> 'c) ->
      match_sequence:(location:'a -> patterns:'c list -> 'c) ->
      match_mapping:(location:'a -> keys:'b list -> patterns:'c list -> rest:'e option -> 'c) ->
      match_class:
        (location:'a ->
        cls:'b ->
        patterns:'c list ->
        kwd_attrs:'e list ->
        kwd_patterns:'c list ->
        'c) ->
      match_star:(location:'a -> name:'e option -> 'c) ->
      match_as:(location:'a -> pattern:'c option -> name:'e option -> 'c) ->
      match_or:(location:'a -> patterns:'c list -> 'c) ->
      unit ->
      ('d, 'b, 'e, 'a, 'c) t
    (** Constructor of {!type: t}. *)
  end

  (** This module provides a type that represents a Python statement. *)
  module Statement : sig
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
         t =
      private {
      function_def :
        location:'location ->
        name:'identifier ->
        args:'arguments ->
        body:'stmt list ->
        decorator_list:'expr list ->
        returns:'expr option ->
        type_comment:string option ->
        'stmt;
          (** Represent a function definition (i.e. [def]) statement.

              - [name] is the name of the function.
              - [args] is the argument list of the function (see {!module: Arguments}).
              - [body] is the body of the function.
              - [decorator_list] is the list of decorators applied to the function.
              - [returns] is the (optionally specified) return annotation.
              - [type_comment] is the Py2-style type comment for the function (see {!val:
                Parser.TaglessFinal.parse_function_type}). *)
      async_function_def :
        location:'location ->
        name:'identifier ->
        args:'arguments ->
        body:'stmt list ->
        decorator_list:'expr list ->
        returns:'expr option ->
        type_comment:string option ->
        'stmt;
          (** Represent an async function definition (i.e. [async def]) statement. It has exactly
              the same set of fields as [def] statement. *)
      class_def :
        location:'location ->
        name:'identifier ->
        bases:'expr list ->
        keywords:'keyword list ->
        body:'stmt list ->
        decorator_list:'expr list ->
        'stmt;
          (** Represent a class definition (i.e. [class]) statement.

              - [name] is the name of the class.
              - [bases] is the positional arguments of the class' argument list (e.g. the [Bar] part
                in [class Foo(Bar): ...]).
              - [keywords] is the keyword arguments of the class' argument list (e.g. the
                [metaclass=FooMeta] part in [class Foo(metaclass=FooMeta): ...]). This is mainly
                used for metaclasses (see {{:https://www.python.org/dev/peps/pep-3115/} PEP 3115})
                but can also be used for other customizations of class creation (see
                {{:https://www.python.org/dev/peps/pep-0487/} PEP 487}).
              - [body] is the body of the class.
              - [decorator_list] is the list of decorators applied to the function. *)
      return : location:'location -> value:'expr option -> 'stmt;
          (** Represent a [return] statement.

              - [value] is the (optionally specified) return value. *)
      delete : location:'location -> targets:'expr list -> 'stmt;
          (** Represent a [delete] statement.

              - [targets] is the (optionally specified) values to delete. Deletion can only be one
                of a selected few kinds of expressions, and the corresponding expression context
                will always be set to {!field: ExpressionContext.del}. See documentation of
                {!module: ExpressionContext} for more details of which expressions are allowed. *)
      assign :
        location:'location ->
        targets:'expr list ->
        value:'expr ->
        type_comment:string option ->
        'stmt;
          (** Represent an unannotated assignment statement.

              - [targets] is the variables/attributes/subscripts to assign to. Assignment target can
                only be one of a selected few kinds of expressions, and the corresponding expression
                context will always be set to {!field: ExpressionContext.store}. See documentation
                of {!module: ExpressionContext} for more details of which expressions are allowed.
              - [value] is the value to be assigned to [targets], i.e. the "right-hand-side" of the
                assignment.
              - [type_comment] is the Py2-style type comment annotation for [targets]. Note that if
                [targets] contains more than one element, then type comment is the only possible way
                to specify the annotation for [targets] as the {!field: ann_assign} statement only
                supports inline annotation for a single target. *)
      aug_assign : location:'location -> target:'expr -> op:'bin_op -> value:'expr -> 'stmt;
          (** Represent an augmented assignment statement (see
              {{:https://www.python.org/dev/peps/pep-0203/} PEP 203}).

              - [target] is the variable/attribute/subscript to assign to. Assignment target can
                only be one of a selected few kinds of expressions, and the corresponding expression
                context will always be set to {!field: ExpressionContext.store}. See documentation
                of {!module: ExpressionContext} for more details of which expressions are allowed.
              - [value] is the value that will be used to compute what needs to be assigned to
                [target]. For an augmented assignment [x op= y], then "right-hand-side" of the
                assignment is computed as [x op y]. *)
      ann_assign :
        location:'location ->
        target:'expr ->
        annotation:'expr ->
        value:'expr option ->
        simple:bool ->
        'stmt;
          (** Represent an annotated assignment statement (see
              {{:https://www.python.org/dev/peps/pep-0526/} PEP 526}).

              - [target] is the variable/attribute/subscript to assign to. Assignment target can
                only be one of a selected few kinds of expressions, and the corresponding expression
                context will always be set to {!field: ExpressionContext.store}. See documentation
                of {!module: ExpressionContext} for more details of which expressions are allowed.
              - [value] is the value to be assigned to [target], i.e. the "right-hand-side" of the
                assignment. It can be unset (e.g. [x: int]), in which case the statement will only
                serve annotation purpose.
              - [simple] is set to [true] when [target] is a [name] expression that do not appear in
                between parenthesis, and [false] otherwise. *)
      for_ :
        location:'location ->
        target:'expr ->
        iter:'expr ->
        body:'stmt list ->
        orelse:'stmt list ->
        type_comment:string option ->
        'stmt;
          (** Represent a [for] statement.

              - [targets] is the "index variable" of the statement. It can only be one of a selected
                few kinds of expressions, and the corresponding expression context will always be
                set to {!field: ExpressionContext.store}. See documentation of {!module:
                ExpressionContext} for more details of which expressions are allowed.
              - [iter] is the expression that is being iterated on.
              - [body] is the body of the loop.
              - [orelse] is the list of statements that will be executed when the loop finishes.
              - [type_comment] is the Py2-style type comment annotation for [target]. Note that type
                comment is the only possible way to directly specify the annotation for [targets].
                Achieving the same result with inline type annotation requires one to precede the
                loop with an annotation-only {!field: ann_assign} statement. *)
      async_for :
        location:'location ->
        target:'expr ->
        iter:'expr ->
        body:'stmt list ->
        orelse:'stmt list ->
        type_comment:string option ->
        'stmt;
          (** Represent an [async for] statement. It has exactly the same set of fields as [for]
              statement. *)
      while_ : location:'location -> test:'expr -> body:'stmt list -> orelse:'stmt list -> 'stmt;
          (** Represent a [while] statement.

              - [test] is the loop condition.
              - [body] is the body of the loop.
              - [orelse] is the list of statements that will be executed when the loop finishes. *)
      if_ : location:'location -> test:'expr -> body:'stmt list -> orelse:'stmt list -> 'stmt;
          (** Represent a [if] statement.

              - [test] is the branch condition.
              - [body] is the true-branch of the loop.
              - [orelse] is the false-branch of the loop. *)
      with_ :
        location:'location ->
        items:'with_item list ->
        body:'stmt list ->
        type_comment:string option ->
        'stmt;
          (** Represent a [with] statement. See {{:https://www.python.org/dev/peps/pep-0343/} PEP
              343}.

              - [items] is the "context expression" which will be evaluated to obtain a context
                manager.
              - [body] is the body of the [with] block.
              - [type_comment] is the Py2-style type comment annotation for all targets in the with
                items. Note that type comment is the only possible way to directly specify the
                annotation for [targets]. Achieving the same result with inline type annotation
                requires one to precede the with block with annotation-only {!field: ann_assign}
                statements. *)
      async_with :
        location:'location ->
        items:'with_item list ->
        body:'stmt list ->
        type_comment:string option ->
        'stmt;
          (** Represent an [async with] statement. It has exactly the same set of fields as [with]
              statement. *)
      match_ : location:'location -> subject:'expr -> cases:'match_case list -> 'stmt;
          (** Represent a [match] statement. See {{:https://www.python.org/dev/peps/pep-0622/} PEP
              622}.

              - [subject] is the expression that will be matched on.
              - [cases] is the list of branches for this match. See {!module: MatchCase} for more
                details. *)
      raise_ : location:'location -> exc:'expr option -> cause:'expr option -> 'stmt;
          (** Represent a [raise] statement.

              - [exc] is the exception to raise. It can be left unset, in which case Python runtime
                will re-raise the "last" exception that was just caught.
              - [cause] is set if one specify the "from" clause. For example, [raise x from y] will
                set [cause] to [y], making the Python runtime attach [y] to the [__cause__] field of
                [x]. *)
      try_ :
        location:'location ->
        body:'stmt list ->
        handlers:'except_handler list ->
        orelse:'stmt list ->
        finalbody:'stmt list ->
        'stmt;
          (** Represent a [try] statement.

              - [body] is the body of the try block, i.e. the logic to run in the normal case.
              - [handlers] is a list of exception handlers, specifying the logic to run in the
                exceptional cases.
              - [orelse] is the list of statements that will be executed when the body finishes
                without having any exception raised and without early return/continue/break.
              - [finalbody] is the list of statements that is guaranteed to be executed when
                execution leaves the try block, regardless of whether an exception occured or not. *)
      assert_ : location:'location -> test:'expr -> msg:'expr option -> 'stmt;
          (** Represent an [assert] statement.

              - [test] is condition to assert on.
              - [msg] is an (optionally specified) argument that will be passed to the
                [AssertionError] being raised by the Python runtime, if the assertion fails. *)
      import : location:'location -> names:'alias list -> 'stmt;
          (** Represent an [import] statement.

              - [name] is the list of module names to import. *)
      import_from :
        location:'location -> module_:'identifier option -> names:'alias list -> level:int -> 'stmt;
          (** Represent a [from ... import ...] statement.

              - [module_] is the name of the module from which we want to import. It can be left
                unset, when no module name is specified (e.g. [from . import X]).
              - [names] is the list of names to import to the current module.
              - [level] is an integer holding the level of the relative import (i.e. the number of
                dots in the import target). *)
      global : location:'location -> names:'identifier list -> 'stmt;
          (** Represent a [global] statement.

              - [names] is a list of names in the current block to declare as global. *)
      nonlocal : location:'location -> names:'identifier list -> 'stmt;
          (** Represent a [nonlocal] statement. See {{:https://www.python.org/dev/peps/pep-3104/}
              PEP 3104}.

              - [names] is a list of names in the current block to declare as nonlocal. *)
      expr : location:'location -> value:'expr -> 'stmt;
          (** Represent an expression statement.

              - [value] holds the expression (usually with side-effects) that will be evaluated. *)
      pass : location:'location -> 'stmt;  (** Represent a [pass] statement. *)
      break : location:'location -> 'stmt;  (** Represent a [break] statement. *)
      continue : location:'location -> 'stmt;  (** Represent a [continue] statement. *)
    }

    val make :
      function_def:
        (location:'a ->
        name:'b ->
        args:'c ->
        body:'d list ->
        decorator_list:'e list ->
        returns:'e option ->
        type_comment:string option ->
        'd) ->
      async_function_def:
        (location:'a ->
        name:'b ->
        args:'c ->
        body:'d list ->
        decorator_list:'e list ->
        returns:'e option ->
        type_comment:string option ->
        'd) ->
      class_def:
        (location:'a ->
        name:'b ->
        bases:'e list ->
        keywords:'f list ->
        body:'d list ->
        decorator_list:'e list ->
        'd) ->
      return:(location:'a -> value:'e option -> 'd) ->
      delete:(location:'a -> targets:'e list -> 'd) ->
      assign:(location:'a -> targets:'e list -> value:'e -> type_comment:string option -> 'd) ->
      aug_assign:(location:'a -> target:'e -> op:'g -> value:'e -> 'd) ->
      ann_assign:(location:'a -> target:'e -> annotation:'e -> value:'e option -> simple:bool -> 'd) ->
      for_:
        (location:'a ->
        target:'e ->
        iter:'e ->
        body:'d list ->
        orelse:'d list ->
        type_comment:string option ->
        'd) ->
      async_for:
        (location:'a ->
        target:'e ->
        iter:'e ->
        body:'d list ->
        orelse:'d list ->
        type_comment:string option ->
        'd) ->
      while_:(location:'a -> test:'e -> body:'d list -> orelse:'d list -> 'd) ->
      if_:(location:'a -> test:'e -> body:'d list -> orelse:'d list -> 'd) ->
      with_:(location:'a -> items:'h list -> body:'d list -> type_comment:string option -> 'd) ->
      async_with:(location:'a -> items:'h list -> body:'d list -> type_comment:string option -> 'd) ->
      match_:(location:'a -> subject:'e -> cases:'i list -> 'd) ->
      raise_:(location:'a -> exc:'e option -> cause:'e option -> 'd) ->
      try_:
        (location:'a ->
        body:'d list ->
        handlers:'j list ->
        orelse:'d list ->
        finalbody:'d list ->
        'd) ->
      assert_:(location:'a -> test:'e -> msg:'e option -> 'd) ->
      import:(location:'a -> names:'k list -> 'd) ->
      import_from:(location:'a -> module_:'b option -> names:'k list -> level:int -> 'd) ->
      global:(location:'a -> names:'b list -> 'd) ->
      nonlocal:(location:'a -> names:'b list -> 'd) ->
      expr:(location:'a -> value:'e -> 'd) ->
      pass:(location:'a -> 'd) ->
      break:(location:'a -> 'd) ->
      continue:(location:'a -> 'd) ->
      unit ->
      ('k, 'c, 'g, 'j, 'e, 'b, 'f, 'a, 'i, 'h, 'd) t
    (** Constructor of {!type: t}. *)
  end

  (** This module provides a type that represents a Python type ignore item.

      A type ignore item can only appear as comment in the source code, in the form of
      [# type: ignore].

      [lineno] keeps track of their line numbers. And [tag] keep track of anything else that comes
      after the ignore comment. *)
  module TypeIgnore : sig
    type 'a t = lineno:int -> tag:string -> 'a
  end

  (** This module provides a type that represents a Python module.

      A Python module consists of a collection of statements, paired with a collection of type
      ignore items. *)
  module Module : sig
    type ('stmt, 'type_ignore, 'module_) t =
      body:'stmt list -> type_ignores:'type_ignore list -> 'module_
  end

  (** This module provides a type that represents a Python function type signature. See {!val:
      Parser.TaglessFinal.parse_function_type}. *)
  module FunctionType : sig
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
       t =
    private {
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

  val make :
    argument:('a, 'b, 'c, 'd) Argument.t ->
    arguments:('d, 'a, 'e) Arguments.t ->
    binary_operator:'f BinaryOperator.t ->
    boolean_operator:'g BooleanOperator.t ->
    comparison_operator:'h ComparisonOperator.t ->
    comprehension:('a, 'i) Comprehension.t ->
    constant:'j Constant.t ->
    exception_handler:('a, 'b, 'c, 'k, 'l) ExceptionHandler.t ->
    expression:('e, 'f, 'g, 'h, 'i, 'j, 'm, 'b, 'n, 'c, 'o, 'a) Expression.t ->
    expression_context:'m ExpressionContext.t ->
    function_type:('a, 'p) FunctionType.t ->
    identifier:'b Identifier.t ->
    import_alias:('b, 'c, 'q) ImportAlias.t ->
    keyword:('a, 'b, 'c, 'n) Keyword.t ->
    location:('r, 'c) Location.t ->
    match_case:('a, 's, 'k, 't) MatchCase.t ->
    module_:('k, 'u, 'v) Module.t ->
    pattern:('j, 'a, 'b, 'c, 's) Pattern.t ->
    position:'r Position.t ->
    statement:('q, 'e, 'f, 'l, 'a, 'b, 'n, 'c, 't, 'w, 'k) Statement.t ->
    type_ignore:'u TypeIgnore.t ->
    unary_operator:'o UnaryOperator.t ->
    with_item:('a, 'w) WithItem.t ->
    unit ->
    ('d, 'e, 'f, 'g, 'h, 'i, 'j, 'l, 'a, 'm, 'p, 'b, 'q, 'n, 'c, 't, 'v, 's, 'r, 'k, 'u, 'o, 'w) t
  (** Constructor of {!type: t}. *)
end

(** This module provides types for a concrete abstract syntax tree of Python, for downstream clients
    who perfer conventional ADT over the tagless-final approach.

    The structure of the syntax tree is kept in sync with the structure of the tagless-final APIs.
    Consult documentation of {!module: TaglessFinal} for meanings of the various syntax constructs.

    Record/variant definitions are intentionally made [private] within this module. To construct
    those records/variants, use the corresponding [make_t] constructor functions. *)
module Concrete : sig
  (** See {!module: TaglessFinal.Position}. *)
  module Position : sig
    type t = private { line : int; column : int } [@@deriving sexp, compare, hash, make]
  end

  (** See {!module: TaglessFinal.Location}. *)
  module Location : sig
    type t = private { start : Position.t; stop : Position.t }
    [@@deriving sexp, compare, hash, make]
  end

  (** See {!module: TaglessFinal.Identifier}. *)
  module Identifier : sig
    type t [@@deriving sexp, compare, hash]

    val make_t : string -> unit -> t

    val to_string : t -> string
  end

  (** See {!module: TaglessFinal.Constant}. *)
  module Constant : sig
    type t = private
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

    val make_none_of_t : unit -> t

    val make_false_of_t : unit -> t

    val make_true_of_t : unit -> t

    val make_ellipsis_of_t : unit -> t

    val make_integer_of_t : int -> t

    val make_big_integer_of_t : string -> t

    val make_float_of_t : float -> t

    val make_complex_of_t : float -> t

    val make_string_of_t : string -> t

    val make_byte_string_of_t : string -> t
  end

  (** See {!module: TaglessFinal.ExpressionContext}. *)
  module ExpressionContext : sig
    type t = private Load | Store | Del [@@deriving sexp, compare, hash]

    val make_load_of_t : unit -> t

    val make_store_of_t : unit -> t

    val make_del_of_t : unit -> t
  end

  (** See {!module: TaglessFinal.BooleanOperator}. *)
  module BooleanOperator : sig
    type t = private And | Or [@@deriving sexp, compare, hash]

    val make_and_of_t : unit -> t

    val make_or_of_t : unit -> t
  end

  (** See {!module: TaglessFinal.BinaryOperator}. *)
  module BinaryOperator : sig
    type t = private
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

    val make_add_of_t : unit -> t

    val make_sub_of_t : unit -> t

    val make_mult_of_t : unit -> t

    val make_matmult_of_t : unit -> t

    val make_div_of_t : unit -> t

    val make_mod_of_t : unit -> t

    val make_pow_of_t : unit -> t

    val make_lshift_of_t : unit -> t

    val make_rshift_of_t : unit -> t

    val make_bitor_of_t : unit -> t

    val make_bitxor_of_t : unit -> t

    val make_bitand_of_t : unit -> t

    val make_floordiv_of_t : unit -> t
  end

  (** See {!module: TaglessFinal.UnaryOperator}. *)
  module UnaryOperator : sig
    type t = private Invert | Not | UAdd | USub [@@deriving sexp, compare, hash]

    val make_invert_of_t : unit -> t

    val make_not_of_t : unit -> t

    val make_uadd_of_t : unit -> t

    val make_usub_of_t : unit -> t
  end

  (** See {!module: TaglessFinal.ComparisonOperator}. *)
  module ComparisonOperator : sig
    type t = private Eq | NotEq | Lt | Lte | Gt | Gte | Is | IsNot | In | NotIn
    [@@deriving sexp, compare, hash]

    val make_eq_of_t : unit -> t

    val make_noteq_of_t : unit -> t

    val make_lt_of_t : unit -> t

    val make_lte_of_t : unit -> t

    val make_gt_of_t : unit -> t

    val make_gte_of_t : unit -> t

    val make_is_of_t : unit -> t

    val make_isnot_of_t : unit -> t

    val make_in_of_t : unit -> t

    val make_notin_of_t : unit -> t
  end

  (** See {!module: TaglessFinal.ImportAlias}. *)
  module ImportAlias : sig
    type t = private { location : Location.t; name : Identifier.t; asname : Identifier.t option }
    [@@deriving sexp, compare, hash, make]
  end

  (** See {!module: TaglessFinal.Comprehension}. *)
  module rec Comprehension : sig
    type t = private {
      target : Expression.t;
      iter : Expression.t;
      ifs : Expression.t list;
      is_async : bool;
    }
    [@@deriving sexp, compare, hash, make]
  end

  (** See {!module: TaglessFinal.Keyword}. *)
  and Keyword : sig
    type t = private { location : Location.t; arg : Identifier.t option; value : Expression.t }
    [@@deriving sexp, compare, hash, make]
  end

  (** See {!module: TaglessFinal.Argument}. *)
  and Argument : sig
    type t = private {
      location : Location.t;
      identifier : Identifier.t;
      annotation : Expression.t option;
      type_comment : string option;
    }
    [@@deriving sexp, compare, hash, make]
  end

  (** See {!module: TaglessFinal.Arguments}. *)
  and Arguments : sig
    type t = private {
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

  (** See {!module: TaglessFinal.Expression}. *)
  and Expression : sig
    type t = private
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

  (** See {!module: TaglessFinal.WithItem}. *)
  module WithItem : sig
    type t = private { context_expr : Expression.t; optional_vars : Expression.t option }
    [@@deriving sexp, compare, hash, make]
  end

  (** See {!module: TaglessFinal.ExceptionHandler}. *)
  module rec ExceptionHandler : sig
    type t = private {
      location : Location.t;
      type_ : Expression.t option;
      name : Identifier.t option;
      body : Statement.t list;
    }
    [@@deriving sexp, compare, hash, make]
  end

  (** See {!module: TaglessFinal.MatchCase}. *)
  and MatchCase : sig
    type t = private { pattern : Pattern.t; guard : Expression.t option; body : Statement.t list }
    [@@deriving sexp, compare, hash, make]
  end

  (** See {!module: TaglessFinal.Pattern}. *)
  and Pattern : sig
    type t = private
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

  (** See {!module: TaglessFinal.Statement}. *)
  and Statement : sig
    type t = private
      | FunctionDef of {
          location : Location.t;
          name : Identifier.t;
          args : Arguments.t;
          body : t list;
          decorator_list : Expression.t list;
          returns : Expression.t option;
          type_comment : string option;
        }
      | AsyncFunctionDef of {
          location : Location.t;
          name : Identifier.t;
          args : Arguments.t;
          body : t list;
          decorator_list : Expression.t list;
          returns : Expression.t option;
          type_comment : string option;
        }
      | ClassDef of {
          location : Location.t;
          name : Identifier.t;
          bases : Expression.t list;
          keywords : Keyword.t list;
          body : t list;
          decorator_list : Expression.t list;
        }
      | Return of { location : Location.t; value : Expression.t option }
      | Delete of { location : Location.t; targets : Expression.t list }
      | Assign of {
          location : Location.t;
          targets : Expression.t list;
          value : Expression.t;
          type_comment : string option;
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

  (** See {!module: TaglessFinal.TypeIgnore}. *)
  module TypeIgnore : sig
    type t = private { lineno : int; tag : string option } [@@deriving sexp, compare, hash, make]
  end

  (** See {!module: TaglessFinal.Module}. *)
  module Module : sig
    type t = private { body : Statement.t list; type_ignores : TypeIgnore.t list }
    [@@deriving sexp, compare, hash, make]
  end

  (** See {!module: TaglessFinal.FunctionType}. *)
  module FunctionType : sig
    type t = { argtypes : Expression.t list; returns : Expression.t }
    [@@deriving sexp, compare, hash, make]
  end

  val make_tagless_final :
    unit ->
    ( Argument.t,
      Arguments.t,
      BinaryOperator.t,
      BooleanOperator.t,
      ComparisonOperator.t,
      Comprehension.t,
      Constant.t,
      ExceptionHandler.t,
      Expression.t,
      ExpressionContext.t,
      FunctionType.t,
      Identifier.t,
      ImportAlias.t,
      Keyword.t,
      Location.t,
      MatchCase.t,
      Module.t,
      Pattern.t,
      Position.t,
      Statement.t,
      TypeIgnore.t,
      UnaryOperator.t,
      WithItem.t )
    TaglessFinal.t
  (** Return a value of {!type: TaglessFinal.t} whose specification is to construct a concrete AST
      as defined in this module. *)
end

(** This module contains all parsing APIs, i.e. functions that transfrom plain strings into
    syntactical constructs. These APIs will always hand back a {!type: Result.t} where if the
    parsing fails, a {!type: Parser.Error.t} gets returned.

    Under the hood, this library actually compiles and calls into the actual CPython parser code
    (with {{:https://docs.python.org/3.10/library/ast.html#ast.PyCF_TYPE_COMMENTS}
    PyCF_TYPE_COMMENTS} flag enabled), and then it walk through the CPython AST and translate them
    into OCaml structures via C bindings. This is how 100% fidelity with the official CPython
    implementation is achieved -- we are actually relying on exactly the same parser implementation
    that CPython uses. This approach has some notable implications:

    - The parsing APIs are stateful as one need to intialize/finalize CPython runtime before
      invoking its parser. The low-level details are abstracted away with the {!module:
      Parser.Context} module, but the fact that no parsing can be done prior to obtaining a {!type:
      Parser.Context.t} still holds.
    - As of the latest release, I have yet to find an easy way for the parser to handle Unicode
      identifier names. Unicode characters in string literals are fine, but Unicode identifier name
      is something that a barely-initialized CPython runtime cannot handle even with utf-8 mode
      enabled. *)
module Parser : sig
  (** This module contains a type that abstracts away the details of global states required to set
      up the parser. *)
  module Context : sig
    type t
    (** An opaque type representing the global state of the parser. Obtaining a value of this type
        is a pre-requisite for invoking all other parsing-related APIs.

        A value of this type can be obtained via {!val: Parser.with_context}. *)
  end

  (** This module contains a type that represents parsing errors. *)
  module Error : sig
    type t = { message : string; line : int; column : int }
    (** Line numbers start from 1 and column numbers start from 0. *)
  end

  val with_context : ?on_init_failure:(unit -> 'a) -> (Context.t -> 'a) -> 'a
  (** [with_context ?on_init_failure f] first creates a value [c] of type {!type: Context.t} and
      then invoke [f] on [c]. It is guaranteed that the created context [c] will be destroyed in the
      end regardless of whether [f] raises an exception or not.

      If the creation of [c] fails, [on_init_failure ()] will be invoked, and [f] will not be
      called. By default, if not explicitly overriden then [on_init_failure] would simply raise a
      {!exception: Failure}. *)

  (** This module provides parsing APIs for downstream clients that are written in tagless-final
      style. See {!module: PyreAst.TaglessFinal} for more explanation about this style.

      An implementation note: if the client provides a tagless-final specification that contains
      side-effects, then the order in which various syntax constructs gets recursed into by these
      APIs starts to matter. This library generally adopts the following order:

      - Traversals are post-order: child constructs will be visited before the parent constructs.

      - If a parent construct contains more than one child constructs, these child constructs will
        be visited in their order of appearance (e.g. for {!field:
        PyreAst.TaglessFinal.Expression.name}, the [location] field will be visited first, then
        [name], and finally [ctx]).

      - For list-like constructs, however, the visiting order will be the {i reverse} of the
        appearance order (e.g. when parsing a module [x = 1; y = 2], the second statement [y = 2]
        will be visited before the first statement [x = 1]). *)
  module TaglessFinal : sig
    val parse_module :
      context:Context.t ->
      spec:
        (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, 'module_, _, _, _, _, _, _) TaglessFinal.t ->
      ?filename:string ->
      string ->
      ('module_, Error.t) Result.t
    (** [parse_module ~context ~spec input] takes the string [input] and parse it as Python module
        using tagless-final specification [spec]. See documentation of {!type: Context.t} for the
        meaning of the [context] argument.

        Optionally a [filename] argument can be specified. If there is a parse error, [filename]
        will appear in the {!field: Error.message} field. *)

    val parse_expression :
      context:Context.t ->
      spec:
        ( _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          'expression,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _ )
        TaglessFinal.t ->
      string ->
      ('expression, Error.t) Result.t
    (** [parse_expression ~context ~spec input] takes the string [input] and parse it as Python
        expression using tagless-final specification [spec]. See documentation of {!type: Context.t}
        for the meaning of the [context] argument. *)

    val parse_function_type :
      context:Context.t ->
      spec:
        ( _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          'function_type,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _ )
        TaglessFinal.t ->
      string ->
      ('function_type, Error.t) Result.t
    (** [parse_expression ~context ~spec input] takes the string [input] and parse it as Python
        function type signature using tagless-final specification [spec]. See documentation of
        {!type: Context.t} for the meaning of the [context] argument.

        Function type signature is not a reified construct in the core Python langugage. They only
        appears in
        {{:https://www.python.org/dev/peps/pep-0484/#suggested-syntax-for-python-2-7-and-straddling-code}
        Python2-style typing comments} for functions and are superceded by the inline annotation
        syntax added in Python3. This API is provided here for completeness, in case downstream
        clients want to support the old comment-style annotation. *)
  end

  (** This module provides parsing APIs for downstream clients that are written in the traditional
      "initial" style which expects a concrete ADT representation for abstract syntax trees.

      Logic in this module is implemented entirely on top of the tagless-final APIs provided in the
      {!module:Parser.TaglessFinal} module. *)
  module Concrete : sig
    val parse_module :
      context:Context.t -> ?filename:string -> string -> (Concrete.Module.t, Error.t) result
    (** [parse_module ~context input] takes the string [input] and parse into a Python module. See
        documentation of {!type: Context.t} for the meaning of the [context] argument.

        Optionally a [filename] argument can be specified. If there is a parse error, [filename]
        will appear in the {!field: Error.message} field.

        Calling this function is equivalent to calling {!val: TaglessFinal.parse_module} with [spec]
        set to the return value of {!val: PyreAst.Concrete.make_tagless_final}. *)

    val parse_expression : context:Context.t -> string -> (Concrete.Expression.t, Error.t) result
    (** [parse_expression ~context ~spec input] takes the string [input] and parse it into a Python
        expression . See documentation of {!type: Context.t} for the meaning of the [context]
        argument.

        Calling this function is equivalent to calling {!val: TaglessFinal.parse_expression} with
        [spec] set to the return value of {!val: PyreAst.Concrete.make_tagless_final}. *)

    val parse_function_type :
      context:Context.t -> string -> (Concrete.FunctionType.t, Error.t) result
    (** [parse_expression ~context ~spec input] takes the string [input] and parse it into a Python
        function type signature. See documentation of {!type: Context.t} for the meaning of the
        [context] argument, and see documentation of {!val: TaglessFinal.parse_function_type} for
        the meaning of "function type signature".

        Calling this function is equivalent to calling {!val: TaglessFinal.parse_function_type} with
        [spec] set to the return value of {!val: PyreAst.Concrete.make_tagless_final}. *)
  end
end
