// OCaml-related includes
#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#define Val_none Val_int(0)

// CPython-related includes
#define PY_SSIZE_T_CLEAN
#define Py_BUILD_CORE // Required to include internal headers
#include "Python.h"
#include "internal/pycore_ast.h"
#include "internal/pycore_parser.h"

#define DEFAULT_SYNTAX_ERROR_LINE -1
#define DEFAULT_SYNTAX_ERROR_COLUMN -1

static value Val_some(value v) {
  CAMLparam1(v);
  CAMLlocal1(some);
  some = caml_alloc(1, 0);
  Store_field(some, 0, v);
  CAMLreturn(some);
}

static value PyUnicode_to_ocaml_string(PyObject *object) {
  CAMLparam0();
  CAMLlocal1(result);

  // Explicitly get the size to avoid issues with embedded null bytes.
  Py_ssize_t size;
  const char *data = PyUnicode_AsUTF8AndSize(object, &size);
  if (data == NULL) {
    caml_failwith("UTF8 casting failed");
  }
  result = caml_alloc_initialized_string(size, data);

  CAMLreturn(result);
}

static value PyBytes_to_ocaml_string(PyObject *object) {
  CAMLparam0();
  CAMLlocal1(result);

  // Explicitly get the size to avoid issues with embedded null bytes.
  Py_ssize_t size;
  char *buffer;
  int success = PyBytes_AsStringAndSize(object, &buffer, &size);
  if (success == -1) {
    caml_failwith("Unable to read byte string literal");
  }
  result = caml_alloc_initialized_string(size, buffer);

  CAMLreturn(result);
}

// Mostly similar to PyUnicode_to_ocaml_string, except the string gets encoded
// instead of casted to UTF8. This function can handle more inputs than
// PyUnicdoe_to_ocaml_string, but it cost one more allocation. Avoid calling
// this if the given object is known to be UTF8-encoded to begin with.
static value PyUnicode_to_encoded_ocaml_string(PyObject *object) {
  CAMLparam0();
  CAMLlocal1(result);

  PyObject *bytes_object =
      PyUnicode_AsEncodedString(object, "utf-8", "backslashreplace");
  if (bytes_object == NULL) {
    caml_failwith("UTF8 encoding failed");
  }
  // TODO: If this call raises, we would leak `bytes_object`
  result = PyBytes_to_ocaml_string(bytes_object);
  Py_DECREF(bytes_object);

  CAMLreturn(result);
}

static const char *Dummy_filename = "<string>";

static PyStatus cpython_initialize(void) {
  PyPreConfig preconfig;
  PyPreConfig_InitIsolatedConfig(&preconfig);
  preconfig.utf8_mode = 1;

  PyStatus status;
  status = Py_PreInitialize(&preconfig);
  if (!PyStatus_Exception(status)) {
    PyConfig config;
    PyConfig_InitIsolatedConfig(&config);
    config.parse_argv = 0;
    config._install_importlib = 0;
    config._init_main = 0;
    status = Py_InitializeFromConfig(&config);
    PyConfig_Clear(&config);
  }
  return status;
}

static void cpython_finalize(void) { Py_Finalize(); }

static void raise_parsing_error(const char *message, long line, long column,
                                long end_line, long end_column) {
  CAMLparam0();
  static const value *exn = NULL;
  if (exn == NULL) {
    exn = caml_named_value("parsing_error");
  }
  value args[5] = {caml_copy_string(message), Val_long(line), Val_long(column),
                   Val_long(end_line), Val_long(end_column)};
  caml_raise_with_args(*exn, 5, args);
}

static void raise_parsing_error_from_last_python_exception() {
  CAMLparam0();
  PyObject *type, *value, *traceback;
  PyErr_Fetch(&type, &value, &traceback);
  PyErr_NormalizeException(&type, &value, &traceback);
  assert(type != NULL);

  const char *msg = NULL;
  PyObject *line_object = NULL;
  PyObject *column_object = NULL;
  PyObject *end_line_object = NULL;
  PyObject *end_column_object = NULL;
  PyObject *msg_object = NULL;

  long line = DEFAULT_SYNTAX_ERROR_LINE;
  long column = DEFAULT_SYNTAX_ERROR_COLUMN;
  long end_line = DEFAULT_SYNTAX_ERROR_LINE;
  long end_column = DEFAULT_SYNTAX_ERROR_COLUMN;

  PyObject *name_object = PyObject_GetAttrString(type, "__name__");
  const char *exception_name = PyUnicode_AsUTF8(name_object);
  if (exception_name == NULL ||
      (strcmp(exception_name, "SyntaxError") != 0 &&
       strcmp(exception_name, "IndentationError") != 0)) {
    // TODO: Distinguish more exceptions / provide more info on the exception.
    msg = "CPython runtime raised a non-syntax exception";
    goto cleanup;
  }

  if (value == NULL) {
    msg = "Parsing failed but error location cannot be extracted";
    goto cleanup;
  }
  // Both `type` and `value` should be non-NULL from here on.

  line_object = PyObject_GetAttrString(value, "lineno");
  if (line_object == NULL) {
    msg = "Parsing failed but line number cannot be extracted";
    goto cleanup;
  }
  column_object = PyObject_GetAttrString(value, "offset");
  if (column_object == NULL) {
    msg = "Parsing failed but column number cannot be extracted";
    goto cleanup;
  }
  end_line_object = PyObject_GetAttrString(value, "end_lineno");
  if (end_line_object == NULL) {
    msg = "Parsing failed but end line number cannot be extracted";
    goto cleanup;
  }
  end_column_object = PyObject_GetAttrString(value, "end_offset");
  if (end_column_object == NULL) {
    msg = "Parsing failed but end column number cannot be extracted";
    goto cleanup;
  }

  msg_object = PyObject_GetAttrString(value, "msg");
  if (msg_object == NULL) {
    msg = "Parsing failed but error message cannot be extracted";
  } else {
    msg = PyUnicode_AsUTF8(msg_object);
  }

  line = PyLong_AsLong(line_object);
  column = PyLong_AsLong(column_object);
  end_line = PyLong_AsLong(end_line_object);
  end_column = PyLong_AsLong(end_column_object);

cleanup:
  if (line_object != NULL) {
    Py_DECREF(line_object);
  }
  if (column_object != NULL) {
    Py_DECREF(column_object);
  }
  if (end_line_object != NULL) {
    Py_DECREF(end_line_object);
  }
  if (end_column_object != NULL) {
    Py_DECREF(end_column_object);
  }
  if (msg_object != NULL) {
    Py_DECREF(msg_object);
  }
  if (value != NULL) {
    Py_DECREF(value);
  }
  if (traceback != NULL) {
    Py_DECREF(traceback);
  }
  Py_DECREF(name_object);
  Py_DECREF(type);
  PyErr_Clear();

  raise_parsing_error(msg, line, column, end_line, end_column);
}

CAMLprim value initialize_python_runtime(void) {
  CAMLparam0();
  PyStatus status = cpython_initialize();
  if (PyStatus_Exception(status)) {
    CAMLreturn(Val_false);
  } else {
    CAMLreturn(Val_true);
  }
}

CAMLprim value finalize_python_runtime(void) {
  CAMLparam0();
  cpython_finalize();
  CAMLreturn(Val_unit);
}

#define ARG(v) Field(v, 0)
#define ARGUMENTS(v) Field(v, 1)
#define BINARY_OPERATOR(v) Field(v, 2)
#define BOOLEAN_OPERATOR(v) Field(v, 3)
#define COMPARISON_OPERATOR(v) Field(v, 4)
#define COMPREHENSION(v) Field(v, 5)
#define CONSTANT(v) Field(v, 6)
#define EXCEPT_HANDLER(v) Field(v, 7)
#define EXPRESSION(v) Field(v, 8)
#define EXPRESSION_CONTEXT(v) Field(v, 9)
#define FUNCTION_TYPE(v) Field(v, 10)
#define IDENTIFIER(v) Field(v, 11)
#define IMPORT_ALIAS(v) Field(v, 12)
#define KEYWORD(v) Field(v, 13)
#define LOCATION(v) Field(v, 14)
#define MATCH_CASE(v) Field(v, 15)
#define MODULE(v) Field(v, 16)
#define PATTERN(v) Field(v, 17)
#define POSITION(v) Field(v, 18)
#define STATEMENT(v) Field(v, 19)
#define TYPE_IGNORE(v) Field(v, 20)
#define TYPE_PARAM(v) Field(v, 21)
#define UNARY_OPERATOR(v) Field(v, 22)
#define WITH_ITEM(v) Field(v, 23)

#define CONSTANT_NONE(v) Field(CONSTANT(v), 0)
#define CONSTANT_FALSE(v) Field(CONSTANT(v), 1)
#define CONSTANT_TRUE(v) Field(CONSTANT(v), 2)
#define CONSTANT_ELLIPSIS(v) Field(CONSTANT(v), 3)
#define CONSTANT_INTEGER(v) Field(CONSTANT(v), 4)
#define CONSTANT_BIG_INTEGER(v) Field(CONSTANT(v), 5)
#define CONSTANT_FLOAT(v) Field(CONSTANT(v), 6)
#define CONSTANT_COMPLEX(v) Field(CONSTANT(v), 7)
#define CONSTANT_STRING(v) Field(CONSTANT(v), 8)
#define CONSTANT_BYTE_STRING(v) Field(CONSTANT(v), 9)

#define EXPR_CONTEXT_LOAD(v) Field(EXPRESSION_CONTEXT(v), 0)
#define EXPR_CONTEXT_STORE(v) Field(EXPRESSION_CONTEXT(v), 1)
#define EXPR_CONTEXT_DEL(v) Field(EXPRESSION_CONTEXT(v), 2)

#define BOOLOP_AND(v) Field(BOOLEAN_OPERATOR(v), 0)
#define BOOLOP_OR(v) Field(BOOLEAN_OPERATOR(v), 1)

#define BINOP_ADD(v) Field(BINARY_OPERATOR(v), 0)
#define BINOP_SUB(v) Field(BINARY_OPERATOR(v), 1)
#define BINOP_MULT(v) Field(BINARY_OPERATOR(v), 2)
#define BINOP_MATMULT(v) Field(BINARY_OPERATOR(v), 3)
#define BINOP_DIV(v) Field(BINARY_OPERATOR(v), 4)
#define BINOP_MOD(v) Field(BINARY_OPERATOR(v), 5)
#define BINOP_POW(v) Field(BINARY_OPERATOR(v), 6)
#define BINOP_LSHIFT(v) Field(BINARY_OPERATOR(v), 7)
#define BINOP_RSHIFT(v) Field(BINARY_OPERATOR(v), 8)
#define BINOP_BITOR(v) Field(BINARY_OPERATOR(v), 9)
#define BINOP_BITXOR(v) Field(BINARY_OPERATOR(v), 10)
#define BINOP_BITAND(v) Field(BINARY_OPERATOR(v), 11)
#define BINOP_FLOORDIV(v) Field(BINARY_OPERATOR(v), 12)

#define UNARYOP_INVERT(v) Field(UNARY_OPERATOR(v), 0)
#define UNARYOP_NOT(v) Field(UNARY_OPERATOR(v), 1)
#define UNARYOP_UADD(v) Field(UNARY_OPERATOR(v), 2)
#define UNARYOP_USUB(v) Field(UNARY_OPERATOR(v), 3)

#define CMPOP_EQ(v) Field(COMPARISON_OPERATOR(v), 0)
#define CMPOP_NOTEQ(v) Field(COMPARISON_OPERATOR(v), 1)
#define CMPOP_LT(v) Field(COMPARISON_OPERATOR(v), 2)
#define CMPOP_LTE(v) Field(COMPARISON_OPERATOR(v), 3)
#define CMPOP_GT(v) Field(COMPARISON_OPERATOR(v), 4)
#define CMPOP_GTE(v) Field(COMPARISON_OPERATOR(v), 5)
#define CMPOP_IS(v) Field(COMPARISON_OPERATOR(v), 6)
#define CMPOP_ISNOT(v) Field(COMPARISON_OPERATOR(v), 7)
#define CMPOP_IN(v) Field(COMPARISON_OPERATOR(v), 8)
#define CMPOP_NOTIN(v) Field(COMPARISON_OPERATOR(v), 9)

#define EXPR_BOOL_OP(v) Field(EXPRESSION(v), 0)
#define EXPR_NAMED_EXPR(v) Field(EXPRESSION(v), 1)
#define EXPR_BIN_OP(v) Field(EXPRESSION(v), 2)
#define EXPR_UNARY_OP(v) Field(EXPRESSION(v), 3)
#define EXPR_LAMBDA(v) Field(EXPRESSION(v), 4)
#define EXPR_IF_EXP(v) Field(EXPRESSION(v), 5)
#define EXPR_DICT(v) Field(EXPRESSION(v), 6)
#define EXPR_SET(v) Field(EXPRESSION(v), 7)
#define EXPR_LIST_COMP(v) Field(EXPRESSION(v), 8)
#define EXPR_SET_COMP(v) Field(EXPRESSION(v), 9)
#define EXPR_DICT_COMP(v) Field(EXPRESSION(v), 10)
#define EXPR_GENERATOR_EXP(v) Field(EXPRESSION(v), 11)
#define EXPR_AWAIT(v) Field(EXPRESSION(v), 12)
#define EXPR_YIELD(v) Field(EXPRESSION(v), 13)
#define EXPR_YIELD_FROM(v) Field(EXPRESSION(v), 14)
#define EXPR_COMPARE(v) Field(EXPRESSION(v), 15)
#define EXPR_CALL(v) Field(EXPRESSION(v), 16)
#define EXPR_FORMATTED_VALUE(v) Field(EXPRESSION(v), 17)
#define EXPR_JOINED_STR(v) Field(EXPRESSION(v), 18)
#define EXPR_CONSTANT(v) Field(EXPRESSION(v), 19)
#define EXPR_ATTRIBUTE(v) Field(EXPRESSION(v), 20)
#define EXPR_SUBSCRIPT(v) Field(EXPRESSION(v), 21)
#define EXPR_STARRED(v) Field(EXPRESSION(v), 22)
#define EXPR_NAME(v) Field(EXPRESSION(v), 23)
#define EXPR_LIST(v) Field(EXPRESSION(v), 24)
#define EXPR_TUPLE(v) Field(EXPRESSION(v), 25)
#define EXPR_SLICE(v) Field(EXPRESSION(v), 26)

#define TYPE_PARAM_TYPEVAR(v) Field(TYPE_PARAM(v), 0)
#define TYPE_PARAM_PARAMSPEC(v) Field(TYPE_PARAM(v), 1)
#define TYPE_PARAM_TYPEVARTUPLE(v) Field(TYPE_PARAM(v), 2)

#define PATTERN_MATCH_VALUE(v) Field(PATTERN(v), 0)
#define PATTERN_MATCH_SINGLETON(v) Field(PATTERN(v), 1)
#define PATTERN_MATCH_SEQUENCE(v) Field(PATTERN(v), 2)
#define PATTERN_MATCH_MAPPING(v) Field(PATTERN(v), 3)
#define PATTERN_MATCH_CLASS(v) Field(PATTERN(v), 4)
#define PATTERN_MATCH_STAR(v) Field(PATTERN(v), 5)
#define PATTERN_MATCH_AS(v) Field(PATTERN(v), 6)
#define PATTERN_MATCH_OR(v) Field(PATTERN(v), 7)

#define STMT_FUNCTION_DEF(v) Field(STATEMENT(v), 0)
#define STMT_ASYNC_FUNCTION_DEF(v) Field(STATEMENT(v), 1)
#define STMT_CLASS_DEF(v) Field(STATEMENT(v), 2)
#define STMT_RETURN(v) Field(STATEMENT(v), 3)
#define STMT_DELETE(v) Field(STATEMENT(v), 4)
#define STMT_ASSIGN(v) Field(STATEMENT(v), 5)
#define STMT_TYPE_ALIAS(v) Field(STATEMENT(v), 6)
#define STMT_AUG_ASSIGN(v) Field(STATEMENT(v), 7)
#define STMT_ANN_ASSIGN(v) Field(STATEMENT(v), 8)
#define STMT_FOR(v) Field(STATEMENT(v), 9)
#define STMT_ASYNC_FOR(v) Field(STATEMENT(v), 10)
#define STMT_WHILE(v) Field(STATEMENT(v), 11)
#define STMT_IF(v) Field(STATEMENT(v), 12)
#define STMT_WITH(v) Field(STATEMENT(v), 13)
#define STMT_ASYNC_WITH(v) Field(STATEMENT(v), 14)
#define STMT_MATCH(v) Field(STATEMENT(v), 15)
#define STMT_RAISE(v) Field(STATEMENT(v), 16)
#define STMT_TRY(v) Field(STATEMENT(v), 17)
#define STMT_TRYSTAR(v) Field(STATEMENT(v), 18)
#define STMT_ASSERT(v) Field(STATEMENT(v), 19)
#define STMT_IMPORT(v) Field(STATEMENT(v), 20)
#define STMT_IMPORT_FROM(v) Field(STATEMENT(v), 21)
#define STMT_GLOBAL(v) Field(STATEMENT(v), 22)
#define STMT_NONLOCAL(v) Field(STATEMENT(v), 23)
#define STMT_EXPR(v) Field(STATEMENT(v), 24)
#define STMT_PASS(v) Field(STATEMENT(v), 25)
#define STMT_BREAK(v) Field(STATEMENT(v), 26)
#define STMT_CONTINUE(v) Field(STATEMENT(v), 27)

// Forward delcarations
CAMLprim value visit_expr(value visitor_value, expr_ty expr);
CAMLprim value visit_exprs(value visitor_value, asdl_expr_seq *exprs);
CAMLprim value visit_optional_exprs(value visitor_value, asdl_expr_seq *exprs);
CAMLprim value visit_stmt(value visitor_value, stmt_ty stmt);
CAMLprim value visit_stmts(value visitor_value, asdl_stmt_seq *stmts);
CAMLprim value visit_pattern(value visitor_value, pattern_ty pattern);
CAMLprim value visit_patterns(value visitor_value, asdl_pattern_seq *patterns);
CAMLprim value visit_type_param(value visitor_value, type_param_ty type_param);
CAMLprim value visit_type_params(value visitor_value,
                                 asdl_type_param_seq *type_params);

CAMLprim value visit_position(value visitor_value, int lineno, int col_offset) {
  CAMLparam1(visitor_value);
  CAMLlocal1(result);
  result = caml_callback2(POSITION(visitor_value), Val_int(lineno),
                          Val_int(col_offset));
  CAMLreturn(result);
}

CAMLprim value visit_location(value visitor_value, int lineno, int col_offset,
                              int end_lineno, int end_col_offset) {
  CAMLparam1(visitor_value);
  CAMLlocal3(start_value, stop_value, result);

  start_value = visit_position(visitor_value, lineno, col_offset);
  stop_value = visit_position(visitor_value, end_lineno, end_col_offset);
  result = caml_callback2(LOCATION(visitor_value), start_value, stop_value);

  CAMLreturn(result);
}

CAMLprim value visit_identifier(value visitor_value, identifier identifier) {
  CAMLparam1(visitor_value);
  CAMLlocal1(result);
  result = caml_callback(IDENTIFIER(visitor_value),
                         PyUnicode_to_ocaml_string(identifier));
  CAMLreturn(result);
}

CAMLprim value visit_identifiers(value visitor_value,
                                 asdl_identifier_seq *identifiers) {
  CAMLparam1(visitor_value);
  CAMLlocal3(identifier_value, cons, result);

  Py_ssize_t i, len = asdl_seq_LEN(identifiers);
  result = Val_emptylist;
  for (i = len - 1; i >= 0; i--) {
    identifier identifier = asdl_seq_GET(identifiers, i);
    identifier_value = visit_identifier(visitor_value, identifier);
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, identifier_value);
    Store_field(cons, 1, result);
    result = cons;
  }
  CAMLreturn(result);
}

CAMLprim value visit_constant(value visitor_value, PyObject *object) {
  CAMLparam1(visitor_value);
  CAMLlocal1(result);

  if (object == Py_None) {
    result = CONSTANT_NONE(visitor_value);
  } else if (object == Py_False) {
    result = CONSTANT_FALSE(visitor_value);
  } else if (object == Py_True) {
    result = CONSTANT_TRUE(visitor_value);
  } else if (object == Py_Ellipsis) {
    result = CONSTANT_ELLIPSIS(visitor_value);
  } else if (PyLong_Check(object)) {
    int overflow = 0;
    long number = PyLong_AsLongAndOverflow(object, &overflow);
    if (overflow == 0 && number <= Max_long && number >= Min_long) {
      result = caml_callback(CONSTANT_INTEGER(visitor_value), Val_long(number));
    } else {
      PyObject *string_object = PyObject_Str(object);
      // Need to catch the exception so we get a chance to clean up
      // `string_object`.
      result = caml_callback_exn(CONSTANT_BIG_INTEGER(visitor_value),
                                 PyUnicode_to_ocaml_string(string_object));
      Py_DECREF(string_object);
      if (Is_exception_result(result)) {
        caml_raise(Extract_exception(result));
      }
    }
  } else if (PyUnicode_Check(object)) {
    result = caml_callback(CONSTANT_STRING(visitor_value),
                           PyUnicode_to_encoded_ocaml_string(object));
  } else if (PyFloat_Check(object)) {
    result = caml_callback(CONSTANT_FLOAT(visitor_value),
                           caml_copy_double(PyFloat_AsDouble(object)));
  } else if (PyComplex_Check(object)) {
    // Only the image part matters -- Python has no syntax to directly
    // construct complex literals with real part.
    result = caml_callback(CONSTANT_COMPLEX(visitor_value),
                           caml_copy_double(PyComplex_ImagAsDouble(object)));
  } else if (PyBytes_Check(object)) {
    result = caml_callback(CONSTANT_BYTE_STRING(visitor_value),
                           PyBytes_to_ocaml_string(object));
  } else {
    caml_failwith("Unrecognized constant");
  }

  CAMLreturn(result);
}

CAMLprim value visit_arg(value visitor_value, arg_ty argument) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, identifier_value, annotation_value,
             type_comment_value, result);

  location_value =
      visit_location(visitor_value, argument->lineno, argument->col_offset,
                     argument->end_lineno, argument->end_col_offset);
  identifier_value = visit_identifier(visitor_value, argument->arg);
  if (argument->annotation == NULL) {
    annotation_value = Val_none;
  } else {
    annotation_value =
        Val_some(visit_expr(visitor_value, argument->annotation));
  }
  if (argument->type_comment == NULL) {
    type_comment_value = Val_none;
  } else {
    type_comment_value =
        Val_some(PyUnicode_to_ocaml_string(argument->type_comment));
  }

  value args[4] = {location_value, identifier_value, annotation_value,
                   type_comment_value};
  result = caml_callbackN(ARG(visitor_value), 4, args);
  CAMLreturn(result);
}

CAMLprim value visit_args(value visitor_value, asdl_arg_seq *args) {
  CAMLparam1(visitor_value);
  CAMLlocal3(arg_value, cons, result);

  Py_ssize_t i, len = asdl_seq_LEN(args);
  result = Val_emptylist;
  for (i = len - 1; i >= 0; i--) {
    arg_ty arg = asdl_seq_GET(args, i);
    arg_value = visit_arg(visitor_value, arg);
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, arg_value);
    Store_field(cons, 1, result);
    result = cons;
  }
  CAMLreturn(result);
}

CAMLprim value visit_arguments(value visitor_value, arguments_ty arguments) {
  CAMLparam1(visitor_value);
  CAMLlocal5(posonlyargs_value, args_value, vararg_value, kwonlyargs_value,
             kw_defaults_value);
  CAMLlocal3(kwarg_value, defaults_value, result);

  posonlyargs_value = visit_args(visitor_value, arguments->posonlyargs);
  args_value = visit_args(visitor_value, arguments->args);
  if (arguments->vararg == NULL) {
    vararg_value = Val_none;
  } else {
    vararg_value = Val_some(visit_arg(visitor_value, arguments->vararg));
  }
  kwonlyargs_value = visit_args(visitor_value, arguments->kwonlyargs);
  kw_defaults_value =
      visit_optional_exprs(visitor_value, arguments->kw_defaults);
  if (arguments->kwarg == NULL) {
    kwarg_value = Val_none;
  } else {
    kwarg_value = Val_some(visit_arg(visitor_value, arguments->kwarg));
  }
  defaults_value = visit_exprs(visitor_value, arguments->defaults);

  value args[7] = {posonlyargs_value, args_value,        vararg_value,
                   kwonlyargs_value,  kw_defaults_value, kwarg_value,
                   defaults_value};
  result = caml_callbackN(ARGUMENTS(visitor_value), 7, args);

  CAMLreturn(result);
}

CAMLprim value visit_keyword(value visitor_value, keyword_ty keyword) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, arg_value, value_value, result);

  location_value =
      visit_location(visitor_value, keyword->lineno, keyword->col_offset,
                     keyword->end_lineno, keyword->end_col_offset);
  if (keyword->arg == NULL) {
    arg_value = Val_none;
  } else {
    arg_value = Val_some(visit_identifier(visitor_value, keyword->arg));
  }
  value_value = visit_expr(visitor_value, keyword->value);
  result = caml_callback3(KEYWORD(visitor_value), location_value, arg_value,
                          value_value);

  CAMLreturn(result);
}

CAMLprim value visit_keywords(value visitor_value, asdl_keyword_seq *keywords) {
  CAMLparam1(visitor_value);
  CAMLlocal3(keyword_value, cons, result);

  Py_ssize_t i, len = asdl_seq_LEN(keywords);
  result = Val_emptylist;
  for (i = len - 1; i >= 0; i--) {
    keyword_ty keyword = asdl_seq_GET(keywords, i);
    keyword_value = visit_keyword(visitor_value, keyword);
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, keyword_value);
    Store_field(cons, 1, result);
    result = cons;
  }
  CAMLreturn(result);
}

CAMLprim value visit_comprehension(value visitor_value,
                                   comprehension_ty comprehension) {
  CAMLparam1(visitor_value);
  CAMLlocal4(target_value, iter_value, ifs_value, result);

  target_value = visit_expr(visitor_value, comprehension->target);
  iter_value = visit_expr(visitor_value, comprehension->iter);
  ifs_value = visit_exprs(visitor_value, comprehension->ifs);

  value args[4] = {target_value, iter_value, ifs_value,
                   Val_bool(comprehension->is_async)};
  result = caml_callbackN(COMPREHENSION(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_comprehensions(value visitor_value,
                                    asdl_comprehension_seq *comprehensions) {
  CAMLparam1(visitor_value);
  CAMLlocal3(comprehension_value, cons, result);

  Py_ssize_t i, len = asdl_seq_LEN(comprehensions);
  result = Val_emptylist;
  for (i = len - 1; i >= 0; i--) {
    comprehension_ty comprehension = asdl_seq_GET(comprehensions, i);
    comprehension_value = visit_comprehension(visitor_value, comprehension);
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, comprehension_value);
    Store_field(cons, 1, result);
    result = cons;
  }
  CAMLreturn(result);
}

CAMLprim value visit_expression_context(value visitor_value,
                                        expr_context_ty expr_context) {
  CAMLparam1(visitor_value);
  CAMLlocal1(result);
  if (expr_context == Load) {
    result = EXPR_CONTEXT_LOAD(visitor_value);
  } else if (expr_context == Store) {
    result = EXPR_CONTEXT_STORE(visitor_value);
  } else {
    assert(expr_context == Del && "Invalid value of expr_context");
    result = EXPR_CONTEXT_DEL(visitor_value);
  }
  CAMLreturn(result);
}

CAMLprim value visit_boolean_operator(value visitor_value, boolop_ty bool_op) {
  CAMLparam1(visitor_value);
  CAMLlocal1(result);
  if (bool_op == And) {
    result = BOOLOP_AND(visitor_value);
  } else {
    assert(bool_op == Or && "Invalid value of bool_op");
    result = BOOLOP_OR(visitor_value);
  }
  CAMLreturn(result);
}

CAMLprim value visit_binary_operator(value visitor_value, operator_ty bin_op) {
  CAMLparam1(visitor_value);
  CAMLlocal1(result);
  if (bin_op == Add) {
    result = BINOP_ADD(visitor_value);
  } else if (bin_op == Sub) {
    result = BINOP_SUB(visitor_value);
  } else if (bin_op == Mult) {
    result = BINOP_MULT(visitor_value);
  } else if (bin_op == MatMult) {
    result = BINOP_MATMULT(visitor_value);
  } else if (bin_op == Div) {
    result = BINOP_DIV(visitor_value);
  } else if (bin_op == Mod) {
    result = BINOP_MOD(visitor_value);
  } else if (bin_op == Pow) {
    result = BINOP_POW(visitor_value);
  } else if (bin_op == LShift) {
    result = BINOP_LSHIFT(visitor_value);
  } else if (bin_op == RShift) {
    result = BINOP_RSHIFT(visitor_value);
  } else if (bin_op == BitOr) {
    result = BINOP_BITOR(visitor_value);
  } else if (bin_op == BitXor) {
    result = BINOP_BITXOR(visitor_value);
  } else if (bin_op == BitAnd) {
    result = BINOP_BITAND(visitor_value);
  } else {
    assert(bin_op == FloorDiv && "Invalid value of bin_op");
    result = BINOP_FLOORDIV(visitor_value);
  }
  CAMLreturn(result);
}

CAMLprim value visit_unary_operator(value visitor_value, unaryop_ty unary_op) {
  CAMLparam1(visitor_value);
  CAMLlocal1(result);
  if (unary_op == Invert) {
    result = UNARYOP_INVERT(visitor_value);
  } else if (unary_op == Not) {
    result = UNARYOP_NOT(visitor_value);
  } else if (unary_op == UAdd) {
    result = UNARYOP_UADD(visitor_value);
  } else {
    assert(unary_op == USub && "Invalid value of uanry_op");
    result = UNARYOP_USUB(visitor_value);
  }
  CAMLreturn(result);
}

CAMLprim value visit_comparison_operator(value visitor_value, cmpop_ty cmp_op) {
  CAMLparam1(visitor_value);
  CAMLlocal1(result);
  if (cmp_op == Eq) {
    result = CMPOP_EQ(visitor_value);
  } else if (cmp_op == NotEq) {
    result = CMPOP_NOTEQ(visitor_value);
  } else if (cmp_op == Lt) {
    result = CMPOP_LT(visitor_value);
  } else if (cmp_op == LtE) {
    result = CMPOP_LTE(visitor_value);
  } else if (cmp_op == Gt) {
    result = CMPOP_GT(visitor_value);
  } else if (cmp_op == GtE) {
    result = CMPOP_GTE(visitor_value);
  } else if (cmp_op == Is) {
    result = CMPOP_IS(visitor_value);
  } else if (cmp_op == IsNot) {
    result = CMPOP_ISNOT(visitor_value);
  } else if (cmp_op == In) {
    result = CMPOP_IN(visitor_value);
  } else {
    assert(cmp_op == NotIn && "Invalid value of cmp_op");
    result = CMPOP_NOTIN(visitor_value);
  }
  CAMLreturn(result);
}

CAMLprim value visit_comparison_operators(value visitor_value,
                                          asdl_int_seq *comparators) {
  CAMLparam1(visitor_value);
  CAMLlocal3(comparator_value, cons, result);

  Py_ssize_t i, len = asdl_seq_LEN(comparators);
  result = Val_emptylist;
  for (i = len - 1; i >= 0; i--) {
    cmpop_ty comparator = asdl_seq_GET(comparators, i);
    comparator_value = visit_comparison_operator(visitor_value, comparator);
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, comparator_value);
    Store_field(cons, 1, result);
    result = cons;
  }
  CAMLreturn(result);
}

CAMLprim value visit_bool_op_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, op_value, values_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  op_value = visit_boolean_operator(visitor_value, expr->v.BoolOp.op);
  values_value = visit_exprs(visitor_value, expr->v.BoolOp.values);
  result = caml_callback3(EXPR_BOOL_OP(visitor_value), location_value, op_value,
                          values_value);

  CAMLreturn(result);
}

CAMLprim value visit_named_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, target_value, value_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  target_value = visit_expr(visitor_value, expr->v.NamedExpr.target);
  value_value = visit_expr(visitor_value, expr->v.NamedExpr.value);
  result = caml_callback3(EXPR_NAMED_EXPR(visitor_value), location_value,
                          target_value, value_value);

  CAMLreturn(result);
}

CAMLprim value visit_bin_op_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, op_value, left_value, right_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  left_value = visit_expr(visitor_value, expr->v.BinOp.left);
  op_value = visit_binary_operator(visitor_value, expr->v.BinOp.op);
  right_value = visit_expr(visitor_value, expr->v.BinOp.right);

  value args[4] = {location_value, left_value, op_value, right_value};
  result = caml_callbackN(EXPR_BIN_OP(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_unary_op_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, op_value, operand_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  op_value = visit_unary_operator(visitor_value, expr->v.UnaryOp.op);
  operand_value = visit_expr(visitor_value, expr->v.UnaryOp.operand);
  result = caml_callback3(EXPR_UNARY_OP(visitor_value), location_value,
                          op_value, operand_value);

  CAMLreturn(result);
}

CAMLprim value visit_lambda_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, args_value, body_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  args_value = visit_arguments(visitor_value, expr->v.Lambda.args);
  body_value = visit_expr(visitor_value, expr->v.Lambda.body);
  result = caml_callback3(EXPR_LAMBDA(visitor_value), location_value,
                          args_value, body_value);

  CAMLreturn(result);
}

CAMLprim value visit_if_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, test_value, body_value, orelse_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  test_value = visit_expr(visitor_value, expr->v.IfExp.test);
  body_value = visit_expr(visitor_value, expr->v.IfExp.body);
  orelse_value = visit_expr(visitor_value, expr->v.IfExp.orelse);

  value args[4] = {location_value, test_value, body_value, orelse_value};
  result = caml_callbackN(EXPR_IF_EXP(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_dict_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, keys_value, values_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  keys_value = visit_optional_exprs(visitor_value, expr->v.Dict.keys);
  values_value = visit_exprs(visitor_value, expr->v.Dict.values);
  result = caml_callback3(EXPR_DICT(visitor_value), location_value, keys_value,
                          values_value);

  CAMLreturn(result);
}

CAMLprim value visit_set_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal3(location_value, elts_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  elts_value = visit_exprs(visitor_value, expr->v.Set.elts);
  result = caml_callback2(EXPR_SET(visitor_value), location_value, elts_value);

  CAMLreturn(result);
}

CAMLprim value visit_list_comp_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, elt_value, generators_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  elt_value = visit_expr(visitor_value, expr->v.ListComp.elt);
  generators_value =
      visit_comprehensions(visitor_value, expr->v.ListComp.generators);
  result = caml_callback3(EXPR_LIST_COMP(visitor_value), location_value,
                          elt_value, generators_value);

  CAMLreturn(result);
}

CAMLprim value visit_set_comp_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, elt_value, generators_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  elt_value = visit_expr(visitor_value, expr->v.SetComp.elt);
  generators_value =
      visit_comprehensions(visitor_value, expr->v.SetComp.generators);
  result = caml_callback3(EXPR_SET_COMP(visitor_value), location_value,
                          elt_value, generators_value);

  CAMLreturn(result);
}

CAMLprim value visit_dict_comp_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, key_value, value_value, generators_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  key_value = visit_expr(visitor_value, expr->v.DictComp.key);
  value_value = visit_expr(visitor_value, expr->v.DictComp.value);
  generators_value =
      visit_comprehensions(visitor_value, expr->v.DictComp.generators);

  value args[4] = {location_value, key_value, value_value, generators_value};
  result = caml_callbackN(EXPR_DICT_COMP(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_generator_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, elt_value, generators_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  elt_value = visit_expr(visitor_value, expr->v.GeneratorExp.elt);
  generators_value =
      visit_comprehensions(visitor_value, expr->v.GeneratorExp.generators);
  result = caml_callback3(EXPR_GENERATOR_EXP(visitor_value), location_value,
                          elt_value, generators_value);

  CAMLreturn(result);
}

CAMLprim value visit_await_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal3(location_value, value_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  value_value = visit_expr(visitor_value, expr->v.Await.value);
  result =
      caml_callback2(EXPR_AWAIT(visitor_value), location_value, value_value);

  CAMLreturn(result);
}

CAMLprim value visit_yield_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal3(location_value, value_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  if (expr->v.Yield.value == NULL) {
    value_value = Val_none;
  } else {
    value_value = Val_some(visit_expr(visitor_value, expr->v.Yield.value));
  }
  result =
      caml_callback2(EXPR_YIELD(visitor_value), location_value, value_value);

  CAMLreturn(result);
}

CAMLprim value visit_yield_from_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal3(location_value, value_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  value_value = visit_expr(visitor_value, expr->v.YieldFrom.value);
  result = caml_callback2(EXPR_YIELD_FROM(visitor_value), location_value,
                          value_value);

  CAMLreturn(result);
}

CAMLprim value visit_compare_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, left_value, ops_value, comparators_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  left_value = visit_expr(visitor_value, expr->v.Compare.left);
  ops_value = visit_comparison_operators(visitor_value, expr->v.Compare.ops);
  comparators_value = visit_exprs(visitor_value, expr->v.Compare.comparators);

  value args[4] = {location_value, left_value, ops_value, comparators_value};
  result = caml_callbackN(EXPR_COMPARE(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_call_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, func_value, args_value, keywords_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  func_value = visit_expr(visitor_value, expr->v.Call.func);
  args_value = visit_exprs(visitor_value, expr->v.Call.args);
  keywords_value = visit_keywords(visitor_value, expr->v.Call.keywords);

  value args[4] = {location_value, func_value, args_value, keywords_value};
  result = caml_callbackN(EXPR_CALL(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_formatted_value_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, value_value, format_spec_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  value_value = visit_expr(visitor_value, expr->v.FormattedValue.value);
  if (expr->v.FormattedValue.format_spec == NULL) {
    format_spec_value = Val_none;
  } else {
    format_spec_value =
        Val_some(visit_expr(visitor_value, expr->v.FormattedValue.format_spec));
  }

  value args[4] = {location_value, value_value,
                   Val_int(expr->v.FormattedValue.conversion),
                   format_spec_value};
  result = caml_callbackN(EXPR_FORMATTED_VALUE(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_joined_str_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal3(location_value, values_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  values_value = visit_exprs(visitor_value, expr->v.JoinedStr.values);
  result = caml_callback2(EXPR_JOINED_STR(visitor_value), location_value,
                          values_value);

  CAMLreturn(result);
}

CAMLprim value visit_constant_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, value_value, kind_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  value_value = visit_constant(visitor_value, expr->v.Constant.value);
  if (expr->v.Constant.kind == NULL) {
    kind_value = Val_none;
  } else {
    kind_value = Val_some(PyUnicode_to_ocaml_string(expr->v.Constant.kind));
  }
  result = caml_callback3(EXPR_CONSTANT(visitor_value), location_value,
                          value_value, kind_value);

  CAMLreturn(result);
}

CAMLprim value visit_attribute_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, value_value, attr_value, context_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  value_value = visit_expr(visitor_value, expr->v.Attribute.value);
  attr_value = visit_identifier(visitor_value, expr->v.Attribute.attr);
  context_value =
      visit_expression_context(visitor_value, expr->v.Attribute.ctx);

  value args[4] = {location_value, value_value, attr_value, context_value};
  result = caml_callbackN(EXPR_ATTRIBUTE(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_subscript_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, value_value, slice_value, context_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  value_value = visit_expr(visitor_value, expr->v.Subscript.value);
  slice_value = visit_expr(visitor_value, expr->v.Subscript.slice);
  context_value =
      visit_expression_context(visitor_value, expr->v.Subscript.ctx);

  value args[4] = {location_value, value_value, slice_value, context_value};
  result = caml_callbackN(EXPR_SUBSCRIPT(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_starred_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, value_value, context_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  value_value = visit_expr(visitor_value, expr->v.Starred.value);
  context_value = visit_expression_context(visitor_value, expr->v.Starred.ctx);
  result = caml_callback3(EXPR_STARRED(visitor_value), location_value,
                          value_value, context_value);

  CAMLreturn(result);
}

CAMLprim value visit_name_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, identifier_value, context_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  identifier_value = visit_identifier(visitor_value, expr->v.Name.id);
  context_value = visit_expression_context(visitor_value, expr->v.Name.ctx);
  result = caml_callback3(EXPR_NAME(visitor_value), location_value,
                          identifier_value, context_value);

  CAMLreturn(result);
}

CAMLprim value visit_list_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, elts_value, context_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  elts_value = visit_exprs(visitor_value, expr->v.List.elts);
  context_value = visit_expression_context(visitor_value, expr->v.List.ctx);
  result = caml_callback3(EXPR_LIST(visitor_value), location_value, elts_value,
                          context_value);

  CAMLreturn(result);
}

CAMLprim value visit_tuple_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, elts_value, context_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  elts_value = visit_exprs(visitor_value, expr->v.Tuple.elts);
  context_value = visit_expression_context(visitor_value, expr->v.Tuple.ctx);
  result = caml_callback3(EXPR_TUPLE(visitor_value), location_value, elts_value,
                          context_value);

  CAMLreturn(result);
}

CAMLprim value visit_slice_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, lower_value, upper_value, step_value, result);

  location_value = visit_location(visitor_value, expr->lineno, expr->col_offset,
                                  expr->end_lineno, expr->end_col_offset);
  if (expr->v.Slice.lower == NULL) {
    lower_value = Val_none;
  } else {
    lower_value = Val_some(visit_expr(visitor_value, expr->v.Slice.lower));
  }
  if (expr->v.Slice.upper == NULL) {
    upper_value = Val_none;
  } else {
    upper_value = Val_some(visit_expr(visitor_value, expr->v.Slice.upper));
  }
  if (expr->v.Slice.step == NULL) {
    step_value = Val_none;
  } else {
    step_value = Val_some(visit_expr(visitor_value, expr->v.Slice.step));
  }

  value args[4] = {location_value, lower_value, upper_value, step_value};
  result = caml_callbackN(EXPR_SLICE(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_expr(value visitor_value, expr_ty expr) {
  CAMLparam1(visitor_value);
  CAMLlocal1(result);

  switch (expr->kind) {
  case BoolOp_kind:
    result = visit_bool_op_expr(visitor_value, expr);
    break;
  case NamedExpr_kind:
    result = visit_named_expr(visitor_value, expr);
    break;
  case BinOp_kind:
    result = visit_bin_op_expr(visitor_value, expr);
    break;
  case UnaryOp_kind:
    result = visit_unary_op_expr(visitor_value, expr);
    break;
  case Lambda_kind:
    result = visit_lambda_expr(visitor_value, expr);
    break;
  case IfExp_kind:
    result = visit_if_expr(visitor_value, expr);
    break;
  case Dict_kind:
    result = visit_dict_expr(visitor_value, expr);
    break;
  case Set_kind:
    result = visit_set_expr(visitor_value, expr);
    break;
  case ListComp_kind:
    result = visit_list_comp_expr(visitor_value, expr);
    break;
  case SetComp_kind:
    result = visit_set_comp_expr(visitor_value, expr);
    break;
  case DictComp_kind:
    result = visit_dict_comp_expr(visitor_value, expr);
    break;
  case GeneratorExp_kind:
    result = visit_generator_expr(visitor_value, expr);
    break;
  case Await_kind:
    result = visit_await_expr(visitor_value, expr);
    break;
  case Yield_kind:
    result = visit_yield_expr(visitor_value, expr);
    break;
  case YieldFrom_kind:
    result = visit_yield_from_expr(visitor_value, expr);
    break;
  case Compare_kind:
    result = visit_compare_expr(visitor_value, expr);
    break;
  case Call_kind:
    result = visit_call_expr(visitor_value, expr);
    break;
  case FormattedValue_kind:
    result = visit_formatted_value_expr(visitor_value, expr);
    break;
  case JoinedStr_kind:
    result = visit_joined_str_expr(visitor_value, expr);
    break;
  case Constant_kind:
    result = visit_constant_expr(visitor_value, expr);
    break;
  case Attribute_kind:
    result = visit_attribute_expr(visitor_value, expr);
    break;
  case Subscript_kind:
    result = visit_subscript_expr(visitor_value, expr);
    break;
  case Starred_kind:
    result = visit_starred_expr(visitor_value, expr);
    break;
  case Name_kind:
    result = visit_name_expr(visitor_value, expr);
    break;
  case List_kind:
    result = visit_list_expr(visitor_value, expr);
    break;
  case Tuple_kind:
    result = visit_tuple_expr(visitor_value, expr);
    break;
  case Slice_kind:
    result = visit_slice_expr(visitor_value, expr);
    break;
  }

  CAMLreturn(result);
}

CAMLprim value visit_exprs(value visitor_value, asdl_expr_seq *exprs) {
  CAMLparam1(visitor_value);
  CAMLlocal3(expr_value, cons, result);

  Py_ssize_t i, len = asdl_seq_LEN(exprs);
  result = Val_emptylist;
  for (i = len - 1; i >= 0; i--) {
    expr_ty expr = asdl_seq_GET(exprs, i);
    expr_value = visit_expr(visitor_value, expr);
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, expr_value);
    Store_field(cons, 1, result);
    result = cons;
  }
  CAMLreturn(result);
}

CAMLprim value visit_optional_exprs(value visitor_value, asdl_expr_seq *exprs) {
  CAMLparam1(visitor_value);
  CAMLlocal3(expr_value, cons, result);

  Py_ssize_t i, len = asdl_seq_LEN(exprs);
  result = Val_emptylist;
  for (i = len - 1; i >= 0; i--) {
    expr_ty expr = asdl_seq_GET(exprs, i);
    if (expr == NULL) {
      expr_value = Val_none;
    } else {
      expr_value = Val_some(visit_expr(visitor_value, expr));
    }
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, expr_value);
    Store_field(cons, 1, result);
    result = cons;
  }
  CAMLreturn(result);
}

CAMLprim value visit_with_item(value visitor_value, withitem_ty with_item) {
  CAMLparam1(visitor_value);
  CAMLlocal3(context_expr_value, optional_vars_value, result);

  context_expr_value = visit_expr(visitor_value, with_item->context_expr);
  if (with_item->optional_vars == NULL) {
    optional_vars_value = Val_none;
  } else {
    optional_vars_value =
        Val_some(visit_expr(visitor_value, with_item->optional_vars));
  }
  result = caml_callback2(WITH_ITEM(visitor_value), context_expr_value,
                          optional_vars_value);
  CAMLreturn(result);
}

CAMLprim value visit_with_items(value visitor_value,
                                asdl_withitem_seq *with_items) {
  CAMLparam1(visitor_value);
  CAMLlocal3(with_item_value, cons, result);

  Py_ssize_t i, len = asdl_seq_LEN(with_items);
  result = Val_emptylist;
  for (i = len - 1; i >= 0; i--) {
    withitem_ty with_item = asdl_seq_GET(with_items, i);
    with_item_value = visit_with_item(visitor_value, with_item);
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, with_item_value);
    Store_field(cons, 1, result);
    result = cons;
  }
  CAMLreturn(result);
}

CAMLprim value visit_type_param_typevar(value visitor_value,
                                        value location_value,
                                        type_param_ty type_param) {
  CAMLparam1(visitor_value);
  CAMLlocal3(name_value, bound_value, result);

  name_value = visit_identifier(visitor_value, type_param->v.TypeVar.name);
  if (type_param->v.TypeVar.bound == NULL) {
    bound_value = Val_none;
  } else {
    bound_value =
        Val_some(visit_expr(visitor_value, type_param->v.TypeVar.bound));
  }
  result = caml_callback3(TYPE_PARAM_TYPEVAR(visitor_value), location_value,
                          name_value, bound_value);
  CAMLreturn(result);
}

CAMLprim value visit_type_param_paramspec(value visitor_value,
                                          value location_value,
                                          type_param_ty type_param) {
  CAMLparam1(visitor_value);
  CAMLlocal2(name_value, result);

  name_value = visit_identifier(visitor_value, type_param->v.TypeVar.name);
  result = caml_callback2(TYPE_PARAM_PARAMSPEC(visitor_value), location_value,
                          name_value);
  CAMLreturn(result);
}

CAMLprim value visit_type_param_typevartuple(value visitor_value,
                                             value location_value,
                                             type_param_ty type_param) {
  CAMLparam1(visitor_value);
  CAMLlocal2(name_value, result);

  name_value = visit_identifier(visitor_value, type_param->v.TypeVar.name);
  result = caml_callback2(TYPE_PARAM_TYPEVARTUPLE(visitor_value),
                          location_value, name_value);
  CAMLreturn(result);
}

CAMLprim value visit_type_param(value visitor_value, type_param_ty type_param) {
  CAMLparam1(visitor_value);
  CAMLlocal2(location_value, result);

  location_value =
      visit_location(visitor_value, type_param->lineno, type_param->col_offset,
                     type_param->end_lineno, type_param->end_col_offset);
  switch (type_param->kind) {
  case TypeVar_kind:
    result =
        visit_type_param_typevar(visitor_value, location_value, type_param);
    break;
  case ParamSpec_kind:
    result =
        visit_type_param_paramspec(visitor_value, location_value, type_param);
    break;
  case TypeVarTuple_kind:
    result = visit_type_param_typevartuple(visitor_value, location_value,
                                           type_param);
    break;
  }

  CAMLreturn(result);
}

CAMLprim value visit_type_params(value visitor_value,
                                 asdl_type_param_seq *type_params) {
  CAMLparam1(visitor_value);
  CAMLlocal3(type_param_value, cons, result);

  Py_ssize_t i, len = asdl_seq_LEN(type_params);
  result = Val_emptylist;
  for (i = len - 1; i >= 0; i--) {
    type_param_ty type_param = asdl_seq_GET(type_params, i);
    type_param_value = visit_type_param(visitor_value, type_param);
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, type_param_value);
    Store_field(cons, 1, result);
    result = cons;
  }
  CAMLreturn(result);
}

CAMLprim value visit_except_handler(value visitor_value,
                                    excepthandler_ty except_handler) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, type_value, name_value, body_value, result);

  location_value = visit_location(
      visitor_value, except_handler->lineno, except_handler->col_offset,
      except_handler->end_lineno, except_handler->end_col_offset);
  if (except_handler->v.ExceptHandler.type == NULL) {
    type_value = Val_none;
  } else {
    type_value = Val_some(
        visit_expr(visitor_value, except_handler->v.ExceptHandler.type));
  }
  if (except_handler->v.ExceptHandler.name == NULL) {
    name_value = Val_none;
  } else {
    name_value = Val_some(
        visit_identifier(visitor_value, except_handler->v.ExceptHandler.name));
  }
  body_value = visit_stmts(visitor_value, except_handler->v.ExceptHandler.body);

  value args[4] = {location_value, type_value, name_value, body_value};
  result = caml_callbackN(EXCEPT_HANDLER(visitor_value), 4, args);
  CAMLreturn(result);
}

CAMLprim value visit_except_handlers(value visitor_value,
                                     asdl_excepthandler_seq *except_handlers) {
  CAMLparam1(visitor_value);
  CAMLlocal3(except_handler_value, cons, result);

  Py_ssize_t i, len = asdl_seq_LEN(except_handlers);
  result = Val_emptylist;
  for (i = len - 1; i >= 0; i--) {
    excepthandler_ty except_handler = asdl_seq_GET(except_handlers, i);
    except_handler_value = visit_except_handler(visitor_value, except_handler);
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, except_handler_value);
    Store_field(cons, 1, result);
    result = cons;
  }
  CAMLreturn(result);
}

CAMLprim value visit_match_value_pattern(value visitor_value,
                                         pattern_ty pattern) {
  CAMLparam1(visitor_value);
  CAMLlocal3(location_value, value_value, result);

  location_value =
      visit_location(visitor_value, pattern->lineno, pattern->col_offset,
                     pattern->end_lineno, pattern->end_col_offset);
  value_value = visit_expr(visitor_value, pattern->v.MatchValue.value);
  result = caml_callback2(PATTERN_MATCH_VALUE(visitor_value), location_value,
                          value_value);
  CAMLreturn(result);
}

CAMLprim value visit_match_singleton_pattern(value visitor_value,

                                             pattern_ty pattern) {
  CAMLparam1(visitor_value);
  CAMLlocal3(location_value, value_value, result);

  location_value =
      visit_location(visitor_value, pattern->lineno, pattern->col_offset,
                     pattern->end_lineno, pattern->end_col_offset);
  value_value = visit_constant(visitor_value, pattern->v.MatchSingleton.value);
  result = caml_callback2(PATTERN_MATCH_SINGLETON(visitor_value),
                          location_value, value_value);
  CAMLreturn(result);
}

CAMLprim value visit_match_sequence_pattern(value visitor_value,
                                            pattern_ty pattern) {
  CAMLparam1(visitor_value);
  CAMLlocal3(location_value, patterns_value, result);

  location_value =
      visit_location(visitor_value, pattern->lineno, pattern->col_offset,
                     pattern->end_lineno, pattern->end_col_offset);
  patterns_value =
      visit_patterns(visitor_value, pattern->v.MatchSequence.patterns);
  result = caml_callback2(PATTERN_MATCH_SEQUENCE(visitor_value), location_value,
                          patterns_value);
  CAMLreturn(result);
}

CAMLprim value visit_match_mapping_pattern(value visitor_value,
                                           pattern_ty pattern) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, keys_value, patterns_value, rest_value, result);

  location_value =
      visit_location(visitor_value, pattern->lineno, pattern->col_offset,
                     pattern->end_lineno, pattern->end_col_offset);
  keys_value = visit_exprs(visitor_value, pattern->v.MatchMapping.keys);
  patterns_value =
      visit_patterns(visitor_value, pattern->v.MatchMapping.patterns);
  if (pattern->v.MatchMapping.rest == NULL) {
    rest_value = Val_none;
  } else {
    rest_value =
        Val_some(visit_identifier(visitor_value, pattern->v.MatchMapping.rest));
  }

  value args[4] = {location_value, keys_value, patterns_value, rest_value};
  result = caml_callbackN(PATTERN_MATCH_MAPPING(visitor_value), 4, args);
  CAMLreturn(result);
}

CAMLprim value visit_match_class_pattern(value visitor_value,
                                         pattern_ty pattern) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, cls_value, patterns_value, kwd_attrs_value,
             kwd_patterns_value);
  CAMLlocal1(result);

  location_value =
      visit_location(visitor_value, pattern->lineno, pattern->col_offset,
                     pattern->end_lineno, pattern->end_col_offset);
  cls_value = visit_expr(visitor_value, pattern->v.MatchClass.cls);
  patterns_value =
      visit_patterns(visitor_value, pattern->v.MatchClass.patterns);
  kwd_attrs_value =
      visit_identifiers(visitor_value, pattern->v.MatchClass.kwd_attrs);
  kwd_patterns_value =
      visit_patterns(visitor_value, pattern->v.MatchClass.kwd_patterns);

  value args[5] = {location_value, cls_value, patterns_value, kwd_attrs_value,
                   kwd_patterns_value};
  result = caml_callbackN(PATTERN_MATCH_CLASS(visitor_value), 5, args);
  CAMLreturn(result);
}

CAMLprim value visit_match_star_pattern(value visitor_value,
                                        pattern_ty pattern) {
  CAMLparam1(visitor_value);
  CAMLlocal3(location_value, name_value, result);

  location_value =
      visit_location(visitor_value, pattern->lineno, pattern->col_offset,
                     pattern->end_lineno, pattern->end_col_offset);
  if (pattern->v.MatchStar.name == NULL) {
    name_value = Val_none;
  } else {
    name_value =
        Val_some(visit_identifier(visitor_value, pattern->v.MatchStar.name));
  }
  result = caml_callback2(PATTERN_MATCH_STAR(visitor_value), location_value,
                          name_value);
  CAMLreturn(result);
}

CAMLprim value visit_match_as_pattern(value visitor_value, pattern_ty pattern) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, pattern_value, name_value, result);

  location_value =
      visit_location(visitor_value, pattern->lineno, pattern->col_offset,
                     pattern->end_lineno, pattern->end_col_offset);
  if (pattern->v.MatchAs.pattern == NULL) {
    pattern_value = Val_none;
  } else {
    pattern_value =
        Val_some(visit_pattern(visitor_value, pattern->v.MatchAs.pattern));
  }
  if (pattern->v.MatchAs.name == NULL) {
    name_value = Val_none;
  } else {
    name_value =
        Val_some(visit_identifier(visitor_value, pattern->v.MatchAs.name));
  }
  result = caml_callback3(PATTERN_MATCH_AS(visitor_value), location_value,
                          pattern_value, name_value);
  CAMLreturn(result);
}

CAMLprim value visit_match_or_pattern(value visitor_value, pattern_ty pattern) {
  CAMLparam1(visitor_value);
  CAMLlocal3(location_value, patterns_value, result);

  location_value =
      visit_location(visitor_value, pattern->lineno, pattern->col_offset,
                     pattern->end_lineno, pattern->end_col_offset);
  patterns_value = visit_patterns(visitor_value, pattern->v.MatchOr.patterns);
  result = caml_callback2(PATTERN_MATCH_OR(visitor_value), location_value,
                          patterns_value);
  CAMLreturn(result);
}

CAMLprim value visit_pattern(value visitor_value, pattern_ty pattern) {
  CAMLparam1(visitor_value);
  CAMLlocal1(result);

  switch (pattern->kind) {
  case MatchValue_kind:
    result = visit_match_value_pattern(visitor_value, pattern);
    break;
  case MatchSingleton_kind:
    result = visit_match_singleton_pattern(visitor_value, pattern);
    break;
  case MatchSequence_kind:
    result = visit_match_sequence_pattern(visitor_value, pattern);
    break;
  case MatchMapping_kind:
    result = visit_match_mapping_pattern(visitor_value, pattern);
    break;
  case MatchClass_kind:
    result = visit_match_class_pattern(visitor_value, pattern);
    break;
  case MatchStar_kind:
    result = visit_match_star_pattern(visitor_value, pattern);
    break;
  case MatchAs_kind:
    result = visit_match_as_pattern(visitor_value, pattern);
    break;
  case MatchOr_kind:
    result = visit_match_or_pattern(visitor_value, pattern);
    break;
  }

  CAMLreturn(result);
}

CAMLprim value visit_patterns(value visitor_value, asdl_pattern_seq *patterns) {
  CAMLparam1(visitor_value);
  CAMLlocal3(pattern_value, cons, result);

  Py_ssize_t i, len = asdl_seq_LEN(patterns);
  result = Val_emptylist;
  for (i = len - 1; i >= 0; i--) {
    pattern_ty pattern = asdl_seq_GET(patterns, i);
    pattern_value = visit_pattern(visitor_value, pattern);
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, pattern_value);
    Store_field(cons, 1, result);
    result = cons;
  }
  CAMLreturn(result);
}

CAMLprim value visit_match_case(value visitor_value, match_case_ty match_case) {
  CAMLparam1(visitor_value);
  CAMLlocal4(pattern_value, guard_value, body_value, result);

  pattern_value = visit_pattern(visitor_value, match_case->pattern);
  if (match_case->guard == NULL) {
    guard_value = Val_none;
  } else {
    guard_value = Val_some(visit_expr(visitor_value, match_case->guard));
  }
  body_value = visit_stmts(visitor_value, match_case->body);
  result = caml_callback3(MATCH_CASE(visitor_value), pattern_value, guard_value,
                          body_value);

  CAMLreturn(result);
}

CAMLprim value visit_match_cases(value visitor_value,
                                 asdl_match_case_seq *match_cases) {
  CAMLparam1(visitor_value);
  CAMLlocal3(match_case_value, cons, result);

  Py_ssize_t i, len = asdl_seq_LEN(match_cases);
  result = Val_emptylist;
  for (i = len - 1; i >= 0; i--) {
    match_case_ty match_case = asdl_seq_GET(match_cases, i);
    match_case_value = visit_match_case(visitor_value, match_case);
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, match_case_value);
    Store_field(cons, 1, result);
    result = cons;
  }
  CAMLreturn(result);
}

CAMLprim value visit_function_def_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, name_value, args_value, body_value,
             decorator_list_value);
  CAMLlocal4(returns_value, type_comment_value, type_params_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  name_value = visit_identifier(visitor_value, stmt->v.FunctionDef.name);
  args_value = visit_arguments(visitor_value, stmt->v.FunctionDef.args);
  body_value = visit_stmts(visitor_value, stmt->v.FunctionDef.body);
  decorator_list_value =
      visit_exprs(visitor_value, stmt->v.FunctionDef.decorator_list);
  if (stmt->v.FunctionDef.returns == NULL) {
    returns_value = Val_none;
  } else {
    returns_value =
        Val_some(visit_expr(visitor_value, stmt->v.FunctionDef.returns));
  }
  if (stmt->v.FunctionDef.type_comment == NULL) {
    type_comment_value = Val_none;
  } else {
    type_comment_value =
        Val_some(PyUnicode_to_ocaml_string(stmt->v.FunctionDef.type_comment));
  }
  type_params_value =
      visit_type_params(visitor_value, stmt->v.FunctionDef.type_params);

  value args[8] = {location_value,     name_value,           args_value,
                   body_value,         decorator_list_value, returns_value,
                   type_comment_value, type_params_value};
  result = caml_callbackN(STMT_FUNCTION_DEF(visitor_value), 8, args);

  CAMLreturn(result);
}

CAMLprim value visit_async_function_def_stmt(value visitor_value,
                                             stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, name_value, args_value, body_value,
             decorator_list_value);
  CAMLlocal4(returns_value, type_comment_value, type_params_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  name_value = visit_identifier(visitor_value, stmt->v.AsyncFunctionDef.name);
  args_value = visit_arguments(visitor_value, stmt->v.AsyncFunctionDef.args);
  body_value = visit_stmts(visitor_value, stmt->v.AsyncFunctionDef.body);
  decorator_list_value =
      visit_exprs(visitor_value, stmt->v.AsyncFunctionDef.decorator_list);
  if (stmt->v.AsyncFunctionDef.returns == NULL) {
    returns_value = Val_none;
  } else {
    returns_value =
        Val_some(visit_expr(visitor_value, stmt->v.AsyncFunctionDef.returns));
  }
  if (stmt->v.AsyncFunctionDef.type_comment == NULL) {
    type_comment_value = Val_none;
  } else {
    type_comment_value = Val_some(caml_copy_string(
        PyUnicode_AsUTF8(stmt->v.AsyncFunctionDef.type_comment)));
  }
  type_params_value =
      visit_type_params(visitor_value, stmt->v.FunctionDef.type_params);

  value args[8] = {location_value,     name_value,           args_value,
                   body_value,         decorator_list_value, returns_value,
                   type_comment_value, type_params_value};
  result = caml_callbackN(STMT_FUNCTION_DEF(visitor_value), 8, args);

  CAMLreturn(result);
}

CAMLprim value visit_class_def_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, name_value, bases_value, keywords_value,
             body_value);
  CAMLlocal3(decorator_list_value, type_params_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  name_value = visit_identifier(visitor_value, stmt->v.ClassDef.name);
  bases_value = visit_exprs(visitor_value, stmt->v.ClassDef.bases);
  keywords_value = visit_keywords(visitor_value, stmt->v.ClassDef.keywords);
  body_value = visit_stmts(visitor_value, stmt->v.ClassDef.body);
  decorator_list_value =
      visit_exprs(visitor_value, stmt->v.ClassDef.decorator_list);
  type_params_value =
      visit_type_params(visitor_value, stmt->v.ClassDef.type_params);

  value args[7] = {location_value,   name_value, bases_value,
                   keywords_value,   body_value, decorator_list_value,
                   type_params_value};
  result = caml_callbackN(STMT_CLASS_DEF(visitor_value), 7, args);

  CAMLreturn(result);
}

CAMLprim value visit_return_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal3(location_value, value_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  if (stmt->v.Return.value == NULL) {
    value_value = Val_none;
  } else {
    value_value = Val_some(visit_expr(visitor_value, stmt->v.Return.value));
  }
  result =
      caml_callback2(STMT_RETURN(visitor_value), location_value, value_value);

  CAMLreturn(result);
}

CAMLprim value visit_delete_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal3(location_value, targets_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  targets_value = visit_exprs(visitor_value, stmt->v.Delete.targets);
  result =
      caml_callback2(STMT_DELETE(visitor_value), location_value, targets_value);

  CAMLreturn(result);
}

CAMLprim value visit_assign_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, targets_value, value_value, type_comment_value,
             result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  targets_value = visit_exprs(visitor_value, stmt->v.Assign.targets);
  value_value = visit_expr(visitor_value, stmt->v.Assign.value);
  if (stmt->v.Assign.type_comment == NULL) {
    type_comment_value = Val_none;
  } else {
    type_comment_value =
        Val_some(PyUnicode_to_ocaml_string(stmt->v.Assign.type_comment));
  }

  value args[4] = {location_value, targets_value, value_value,
                   type_comment_value};
  result = caml_callbackN(STMT_ASSIGN(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_type_alias_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, name_value, type_params_value, value_value,
             result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  name_value = visit_expr(visitor_value, stmt->v.TypeAlias.name);
  type_params_value =
      visit_type_params(visitor_value, stmt->v.TypeAlias.type_params);
  value_value = visit_expr(visitor_value, stmt->v.TypeAlias.value);

  value args[4] = {location_value, name_value, type_params_value, value_value};
  result = caml_callbackN(STMT_TYPE_ALIAS(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_aug_assign_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, target_value, op_value, value_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  target_value = visit_expr(visitor_value, stmt->v.AugAssign.target);
  op_value = visit_binary_operator(visitor_value, stmt->v.AugAssign.op);
  value_value = visit_expr(visitor_value, stmt->v.AugAssign.value);

  value args[4] = {location_value, target_value, op_value, value_value};
  result = caml_callbackN(STMT_AUG_ASSIGN(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_ann_assign_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, target_value, annotation_value, value_value,
             simple_value);
  CAMLlocal1(result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  target_value = visit_expr(visitor_value, stmt->v.AnnAssign.target);
  annotation_value = visit_expr(visitor_value, stmt->v.AnnAssign.annotation);
  if (stmt->v.AnnAssign.value == NULL) {
    value_value = Val_none;
  } else {
    value_value = Val_some(visit_expr(visitor_value, stmt->v.AnnAssign.value));
  }
  simple_value = Val_bool(stmt->v.AnnAssign.simple);

  value args[5] = {location_value, target_value, annotation_value, value_value,
                   simple_value};
  result = caml_callbackN(STMT_ANN_ASSIGN(visitor_value), 5, args);

  CAMLreturn(result);
}

CAMLprim value visit_for_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, target_value, iter_value, body_value,
             orelse_value);
  CAMLlocal2(type_comment_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  target_value = visit_expr(visitor_value, stmt->v.For.target);
  iter_value = visit_expr(visitor_value, stmt->v.For.iter);
  body_value = visit_stmts(visitor_value, stmt->v.For.body);
  orelse_value = visit_stmts(visitor_value, stmt->v.For.orelse);
  if (stmt->v.For.type_comment == NULL) {
    type_comment_value = Val_none;
  } else {
    type_comment_value =
        Val_some(PyUnicode_to_ocaml_string(stmt->v.For.type_comment));
  }

  value args[6] = {location_value, target_value, iter_value,
                   body_value,     orelse_value, type_comment_value};
  result = caml_callbackN(STMT_FOR(visitor_value), 6, args);

  CAMLreturn(result);
}

CAMLprim value visit_async_for_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, target_value, iter_value, body_value,
             orelse_value);
  CAMLlocal2(type_comment_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  target_value = visit_expr(visitor_value, stmt->v.AsyncFor.target);
  iter_value = visit_expr(visitor_value, stmt->v.AsyncFor.iter);
  body_value = visit_stmts(visitor_value, stmt->v.AsyncFor.body);
  orelse_value = visit_stmts(visitor_value, stmt->v.AsyncFor.orelse);
  if (stmt->v.AsyncFor.type_comment == NULL) {
    type_comment_value = Val_none;
  } else {
    type_comment_value =
        Val_some(PyUnicode_to_ocaml_string(stmt->v.AsyncFor.type_comment));
  }

  value args[6] = {location_value, target_value, iter_value,
                   body_value,     orelse_value, type_comment_value};
  result = caml_callbackN(STMT_ASYNC_FOR(visitor_value), 6, args);

  CAMLreturn(result);
}

CAMLprim value visit_while_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, test_value, body_value, orelse_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  test_value = visit_expr(visitor_value, stmt->v.While.test);
  body_value = visit_stmts(visitor_value, stmt->v.While.body);
  orelse_value = visit_stmts(visitor_value, stmt->v.While.orelse);

  value args[4] = {location_value, test_value, body_value, orelse_value};
  result = caml_callbackN(STMT_WHILE(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_if_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, test_value, body_value, orelse_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  test_value = visit_expr(visitor_value, stmt->v.If.test);
  body_value = visit_stmts(visitor_value, stmt->v.If.body);
  orelse_value = visit_stmts(visitor_value, stmt->v.If.orelse);

  value args[4] = {location_value, test_value, body_value, orelse_value};
  result = caml_callbackN(STMT_IF(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_with_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, items_value, body_value, type_comment_value,
             result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  items_value = visit_with_items(visitor_value, stmt->v.With.items);
  body_value = visit_stmts(visitor_value, stmt->v.With.body);
  if (stmt->v.With.type_comment == NULL) {
    type_comment_value = Val_none;
  } else {
    type_comment_value =
        Val_some(PyUnicode_to_ocaml_string(stmt->v.With.type_comment));
  }

  value args[4] = {location_value, items_value, body_value, type_comment_value};
  result = caml_callbackN(STMT_WITH(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_async_with_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, items_value, body_value, type_comment_value,
             result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  items_value = visit_with_items(visitor_value, stmt->v.AsyncWith.items);
  body_value = visit_stmts(visitor_value, stmt->v.AsyncWith.body);
  if (stmt->v.AsyncWith.type_comment == NULL) {
    type_comment_value = Val_none;
  } else {
    type_comment_value =
        Val_some(PyUnicode_to_ocaml_string(stmt->v.AsyncWith.type_comment));
  }

  value args[4] = {location_value, items_value, body_value, type_comment_value};
  result = caml_callbackN(STMT_ASYNC_WITH(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_match_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, subject_value, cases_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  subject_value = visit_expr(visitor_value, stmt->v.Match.subject);
  cases_value = visit_match_cases(visitor_value, stmt->v.Match.cases);
  result = caml_callback3(STMT_MATCH(visitor_value), location_value,
                          subject_value, cases_value);

  CAMLreturn(result);
}

CAMLprim value visit_raise_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, exc_value, cause_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  if (stmt->v.Raise.exc == NULL) {
    exc_value = Val_none;
  } else {
    exc_value = Val_some(visit_expr(visitor_value, stmt->v.Raise.exc));
  }
  if (stmt->v.Raise.cause == NULL) {
    cause_value = Val_none;
  } else {
    cause_value = Val_some(visit_expr(visitor_value, stmt->v.Raise.cause));
  }
  result = caml_callback3(STMT_RAISE(visitor_value), location_value, exc_value,
                          cause_value);

  CAMLreturn(result);
}

CAMLprim value visit_try_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, body_value, handlers_value, orelse_value,
             finalbody_value);
  CAMLlocal1(result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  body_value = visit_stmts(visitor_value, stmt->v.Try.body);
  handlers_value = visit_except_handlers(visitor_value, stmt->v.Try.handlers);
  orelse_value = visit_stmts(visitor_value, stmt->v.Try.orelse);
  finalbody_value = visit_stmts(visitor_value, stmt->v.Try.finalbody);

  value args[5] = {location_value, body_value, handlers_value, orelse_value,
                   finalbody_value};
  result = caml_callbackN(STMT_TRY(visitor_value), 5, args);

  CAMLreturn(result);
}

CAMLprim value visit_try_star_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal5(location_value, body_value, handlers_value, orelse_value,
             finalbody_value);
  CAMLlocal1(result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  body_value = visit_stmts(visitor_value, stmt->v.Try.body);
  handlers_value = visit_except_handlers(visitor_value, stmt->v.Try.handlers);
  orelse_value = visit_stmts(visitor_value, stmt->v.Try.orelse);
  finalbody_value = visit_stmts(visitor_value, stmt->v.Try.finalbody);

  value args[5] = {location_value, body_value, handlers_value, orelse_value,
                   finalbody_value};
  result = caml_callbackN(STMT_TRYSTAR(visitor_value), 5, args);

  CAMLreturn(result);
}

CAMLprim value visit_assert_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, test_value, msg_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  test_value = visit_expr(visitor_value, stmt->v.Assert.test);
  if (stmt->v.Assert.msg == NULL) {
    msg_value = Val_none;
  } else {
    msg_value = Val_some(visit_expr(visitor_value, stmt->v.Assert.msg));
  }
  result = caml_callback3(STMT_ASSERT(visitor_value), location_value,
                          test_value, msg_value);

  CAMLreturn(result);
}

CAMLprim value visit_import_alias(value visitor_value, alias_ty alias) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, name_value, asname_value, result);

  location_value =
      visit_location(visitor_value, alias->lineno, alias->col_offset,
                     alias->end_lineno, alias->end_col_offset);
  name_value = visit_identifier(visitor_value, alias->name);
  if (alias->asname == NULL) {
    asname_value = Val_none;
  } else {
    asname_value = Val_some(visit_identifier(visitor_value, alias->asname));
  }
  result = caml_callback3(IMPORT_ALIAS(visitor_value), location_value,
                          name_value, asname_value);

  CAMLreturn(result);
}

CAMLprim value visit_import_aliases(value visitor_value,
                                    asdl_alias_seq *aliases) {
  CAMLparam1(visitor_value);
  CAMLlocal3(alias_value, cons, result);

  Py_ssize_t i, len = asdl_seq_LEN(aliases);
  result = Val_emptylist;
  for (i = len - 1; i >= 0; i--) {
    alias_ty alias = asdl_seq_GET(aliases, i);
    alias_value = visit_import_alias(visitor_value, alias);
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, alias_value);
    Store_field(cons, 1, result);
    result = cons;
  }
  CAMLreturn(result);
}

CAMLprim value visit_import_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal3(location_value, names_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  names_value = visit_import_aliases(visitor_value, stmt->v.Import.names);
  result =
      caml_callback2(STMT_IMPORT(visitor_value), location_value, names_value);

  CAMLreturn(result);
}

CAMLprim value visit_import_from_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal4(location_value, module_value, names_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  if (stmt->v.ImportFrom.module == NULL) {
    module_value = Val_none;
  } else {
    module_value =
        Val_some(visit_identifier(visitor_value, stmt->v.ImportFrom.module));
  }
  names_value = visit_import_aliases(visitor_value, stmt->v.ImportFrom.names);

  value args[4] = {location_value, module_value, names_value,
                   Val_int(stmt->v.ImportFrom.level)};
  result = caml_callbackN(STMT_IMPORT_FROM(visitor_value), 4, args);

  CAMLreturn(result);
}

CAMLprim value visit_global_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal3(location_value, names_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  names_value = visit_identifiers(visitor_value, stmt->v.Global.names);
  result =
      caml_callback2(STMT_GLOBAL(visitor_value), location_value, names_value);

  CAMLreturn(result);
}

CAMLprim value visit_nonlocal_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal3(location_value, names_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  names_value = visit_identifiers(visitor_value, stmt->v.Nonlocal.names);
  result =
      caml_callback2(STMT_NONLOCAL(visitor_value), location_value, names_value);

  CAMLreturn(result);
}

CAMLprim value visit_expr_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal3(location_value, expr_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  expr_value = visit_expr(visitor_value, stmt->v.Expr.value);
  result = caml_callback2(STMT_EXPR(visitor_value), location_value, expr_value);

  CAMLreturn(result);
}

CAMLprim value visit_pass_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal2(location_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  result = caml_callback(STMT_PASS(visitor_value), location_value);

  CAMLreturn(result);
}

CAMLprim value visit_break_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal2(location_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  result = caml_callback(STMT_BREAK(visitor_value), location_value);

  CAMLreturn(result);
}

CAMLprim value visit_continue_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal2(location_value, result);

  location_value = visit_location(visitor_value, stmt->lineno, stmt->col_offset,
                                  stmt->end_lineno, stmt->end_col_offset);
  result = caml_callback(STMT_CONTINUE(visitor_value), location_value);

  CAMLreturn(result);
}

CAMLprim value visit_stmt(value visitor_value, stmt_ty stmt) {
  CAMLparam1(visitor_value);
  CAMLlocal1(result);

  switch (stmt->kind) {
  case FunctionDef_kind:
    result = visit_function_def_stmt(visitor_value, stmt);
    break;
  case AsyncFunctionDef_kind:
    result = visit_async_function_def_stmt(visitor_value, stmt);
    break;
  case ClassDef_kind:
    result = visit_class_def_stmt(visitor_value, stmt);
    break;
  case Return_kind:
    result = visit_return_stmt(visitor_value, stmt);
    break;
  case Delete_kind:
    result = visit_delete_stmt(visitor_value, stmt);
    break;
  case Assign_kind:
    result = visit_assign_stmt(visitor_value, stmt);
    break;
  case TypeAlias_kind:
    result = visit_type_alias_stmt(visitor_value, stmt);
    break;
  case AugAssign_kind:
    result = visit_aug_assign_stmt(visitor_value, stmt);
    break;
  case AnnAssign_kind:
    result = visit_ann_assign_stmt(visitor_value, stmt);
    break;
  case For_kind:
    result = visit_for_stmt(visitor_value, stmt);
    break;
  case AsyncFor_kind:
    result = visit_async_for_stmt(visitor_value, stmt);
    break;
  case While_kind:
    result = visit_while_stmt(visitor_value, stmt);
    break;
  case If_kind:
    result = visit_if_stmt(visitor_value, stmt);
    break;
  case With_kind:
    result = visit_with_stmt(visitor_value, stmt);
    break;
  case AsyncWith_kind:
    result = visit_async_with_stmt(visitor_value, stmt);
    break;
  case Match_kind:
    result = visit_match_stmt(visitor_value, stmt);
    break;
  case Raise_kind:
    result = visit_raise_stmt(visitor_value, stmt);
    break;
  case Try_kind:
    result = visit_try_stmt(visitor_value, stmt);
    break;
  case TryStar_kind:
    result = visit_try_star_stmt(visitor_value, stmt);
    break;
  case Assert_kind:
    result = visit_assert_stmt(visitor_value, stmt);
    break;
  case Import_kind:
    result = visit_import_stmt(visitor_value, stmt);
    break;
  case ImportFrom_kind:
    result = visit_import_from_stmt(visitor_value, stmt);
    break;
  case Global_kind:
    result = visit_global_stmt(visitor_value, stmt);
    break;
  case Nonlocal_kind:
    result = visit_nonlocal_stmt(visitor_value, stmt);
    break;
  case Expr_kind:
    result = visit_expr_stmt(visitor_value, stmt);
    break;
  case Pass_kind:
    result = visit_pass_stmt(visitor_value, stmt);
    break;
  case Break_kind:
    result = visit_break_stmt(visitor_value, stmt);
    break;
  case Continue_kind:
    result = visit_continue_stmt(visitor_value, stmt);
    break;
  }

  CAMLreturn(result);
}

CAMLprim value visit_stmts(value visitor_value, asdl_stmt_seq *stmts) {
  CAMLparam1(visitor_value);
  CAMLlocal3(stmt_value, cons, result);

  Py_ssize_t i, len = asdl_seq_LEN(stmts);
  result = Val_emptylist;
  for (i = len - 1; i >= 0; i--) {
    stmt_ty stmt = asdl_seq_GET(stmts, i);
    stmt_value = visit_stmt(visitor_value, stmt);
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, stmt_value);
    Store_field(cons, 1, result);
    result = cons;
  }
  CAMLreturn(result);
}

CAMLprim value visit_type_ignore(value visitor_value,
                                 type_ignore_ty type_ignore) {
  CAMLparam1(visitor_value);
  CAMLlocal3(lineno_value, tag_value, result);

  lineno_value = Val_int(type_ignore->v.TypeIgnore.lineno);
  tag_value = PyUnicode_to_ocaml_string(type_ignore->v.TypeIgnore.tag);
  result = caml_callback2(TYPE_IGNORE(visitor_value), lineno_value, tag_value);

  CAMLreturn(result);
}

CAMLprim value visit_type_ignores(value visitor_value,
                                  asdl_type_ignore_seq *type_ignores) {
  CAMLparam1(visitor_value);
  CAMLlocal3(type_ignore_value, cons, result);

  Py_ssize_t i, len = asdl_seq_LEN(type_ignores);
  result = Val_emptylist;
  for (i = len - 1; i >= 0; i--) {
    type_ignore_ty type_ignore = asdl_seq_GET(type_ignores, i);
    type_ignore_value = visit_type_ignore(visitor_value, type_ignore);
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, type_ignore_value);
    Store_field(cons, 1, result);
    result = cons;
  }
  CAMLreturn(result);
}

CAMLprim value visit_toplevel_module(value visitor_value, mod_ty module) {
  CAMLparam1(visitor_value);
  assert(module->kind == Module_kind &&
         "visit_toplevel_module() called on a non-module");

  CAMLlocal3(stmts_value, type_ignores_value, result);
  stmts_value = visit_stmts(visitor_value, module->v.Module.body);
  type_ignores_value =
      visit_type_ignores(visitor_value, module->v.Module.type_ignores);
  result =
      caml_callback2(MODULE(visitor_value), stmts_value, type_ignores_value);
  CAMLreturn(result);
}

CAMLprim value visit_toplevel_expression(value visitor_value, mod_ty module) {
  CAMLparam1(visitor_value);
  assert(module->kind == Expression_kind &&
         "visit_toplevel_expression() called on a non-expression");

  CAMLlocal1(result);
  result = visit_expr(visitor_value, module->v.Expression.body);
  CAMLreturn(result);
}

CAMLprim value visit_toplevel_function_type(value visitor_value,
                                            mod_ty module) {
  CAMLparam1(visitor_value);
  assert(module->kind == FunctionType_kind &&
         "visit_toplevel_function_type() called on a non-function-type");

  CAMLlocal3(argtypes_value, returns_value, result);
  argtypes_value = visit_exprs(visitor_value, module->v.FunctionType.argtypes);
  returns_value = visit_expr(visitor_value, module->v.FunctionType.returns);
  result = caml_callback2(FUNCTION_TYPE(visitor_value), argtypes_value,
                          returns_value);
  CAMLreturn(result);
}

struct RawModule {
  PyArena *arena;
  mod_ty ast;
};

static value Val_RawModule(PyArena *arena, mod_ty ast) {
  CAMLparam0();
  CAMLlocal3(arena_value, ast_value, result);

  arena_value = caml_alloc(1, Abstract_tag);
  *((PyArena **)Data_abstract_val(arena_value)) = arena;

  ast_value = caml_alloc(1, Abstract_tag);
  *((mod_ty *)Data_abstract_val(ast_value)) = ast;

  result = caml_alloc(2, 0);
  Store_field(result, 0, arena_value);
  Store_field(result, 1, ast_value);

  CAMLreturn(result);
}

static struct RawModule RawModule_val(value v) {
  struct RawModule result;
  result.arena = *((PyArena **)Data_abstract_val(Field(v, 0)));
  result.ast = *((mod_ty *)Data_abstract_val(Field(v, 1)));
  return result;
}

CAMLprim value cpython_parse_module(value filename_value,
                                    value type_comment_value,
                                    value input_value) {
  CAMLparam3(filename_value, type_comment_value, input_value);
  CAMLlocal1(result);

  PyObject *filename = PyUnicode_FromString(String_val(filename_value));
  if (filename == NULL) {
    raise_parsing_error(
        "CPython Internal error: filename PyObject conversion failed",
        DEFAULT_SYNTAX_ERROR_LINE, DEFAULT_SYNTAX_ERROR_COLUMN,
        DEFAULT_SYNTAX_ERROR_LINE, DEFAULT_SYNTAX_ERROR_COLUMN);
  }

  PyArena *arena = _PyArena_New();
  if (arena == NULL) {
    Py_DECREF(filename);
    raise_parsing_error("CPython Internal error: Arena allocation failed",
                        DEFAULT_SYNTAX_ERROR_LINE, DEFAULT_SYNTAX_ERROR_COLUMN,
                        DEFAULT_SYNTAX_ERROR_LINE, DEFAULT_SYNTAX_ERROR_COLUMN);
  }

  PyCompilerFlags flags = _PyCompilerFlags_INIT;
  if (Bool_val(type_comment_value)) {
    flags.cf_flags |= PyCF_TYPE_COMMENTS;
  }
  mod_ty ast = _PyParser_ASTFromString(String_val(input_value), filename,
                                       Py_file_input, &flags, arena);

  Py_DECREF(filename);
  if (ast == NULL) {
    _PyArena_Free(arena);
    raise_parsing_error_from_last_python_exception();
  }

  result = Val_RawModule(arena, ast);
  CAMLreturn(result);
}

CAMLprim value cpython_convert_module(value visitor_value,
                                      value raw_ast_value) {
  CAMLparam2(visitor_value, raw_ast_value);
  CAMLlocal1(result);

  struct RawModule raw_ast = RawModule_val(raw_ast_value);
  result = visit_toplevel_module(visitor_value, raw_ast.ast);

  CAMLreturn(result);
}

CAMLprim value cpython_parse_expression(value input_value) {
  CAMLparam1(input_value);
  CAMLlocal1(result);

  PyArena *arena = _PyArena_New();
  if (arena == NULL) {
    raise_parsing_error("CPython Internal error: Arena allocation failed",
                        DEFAULT_SYNTAX_ERROR_LINE, DEFAULT_SYNTAX_ERROR_COLUMN,
                        DEFAULT_SYNTAX_ERROR_LINE, DEFAULT_SYNTAX_ERROR_COLUMN);
  }

  PyObject *filename = PyUnicode_FromString(Dummy_filename);
  mod_ty ast = _PyParser_ASTFromString(String_val(input_value), filename,
                                       Py_eval_input, NULL, arena);
  Py_DECREF(filename);

  if (ast == NULL) {
    _PyArena_Free(arena);
    raise_parsing_error_from_last_python_exception();
  }

  result = Val_RawModule(arena, ast);
  CAMLreturn(result);
}

CAMLprim value cpython_convert_expression(value visitor_value,
                                          value raw_ast_value) {
  CAMLparam2(visitor_value, raw_ast_value);
  CAMLlocal1(result);

  struct RawModule raw_ast = RawModule_val(raw_ast_value);
  result = visit_toplevel_expression(visitor_value, raw_ast.ast);

  CAMLreturn(result);
}

CAMLprim value cpython_parse_function_type(value input_value) {
  CAMLparam1(input_value);
  CAMLlocal1(result);

  PyArena *arena = _PyArena_New();
  if (arena == NULL) {
    raise_parsing_error("CPython Internal error: Arena allocation failed",
                        DEFAULT_SYNTAX_ERROR_LINE, DEFAULT_SYNTAX_ERROR_COLUMN,
                        DEFAULT_SYNTAX_ERROR_LINE, DEFAULT_SYNTAX_ERROR_COLUMN);
  }

  PyObject *filename = PyUnicode_FromString(Dummy_filename);
  mod_ty ast = _PyParser_ASTFromString(String_val(input_value), filename,
                                       Py_func_type_input, NULL, arena);
  Py_DECREF(filename);

  if (ast == NULL) {
    _PyArena_Free(arena);
    raise_parsing_error_from_last_python_exception();
  }

  result = Val_RawModule(arena, ast);
  CAMLreturn(result);
}

CAMLprim value cpython_convert_function_type(value visitor_value,
                                             value raw_ast_value) {
  CAMLparam2(visitor_value, raw_ast_value);
  CAMLlocal1(result);

  struct RawModule raw_ast = RawModule_val(raw_ast_value);
  result = visit_toplevel_function_type(visitor_value, raw_ast.ast);

  CAMLreturn(result);
}

CAMLprim value cpython_release_module(value python_ast_value) {
  CAMLparam1(python_ast_value);

  struct RawModule python_ast = RawModule_val(python_ast_value);
  _PyArena_Free(python_ast.arena);

  CAMLreturn(Val_unit);
}
