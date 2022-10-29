Test function def stmt
  $ echo "def foo(): pass" | parse module -
  ((body
    ((FunctionDef
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 15)))))
      (name foo)
      (args
       ((posonlyargs ()) (args ()) (vararg ()) (kwonlyargs ()) (kw_defaults ())
        (kwarg ()) (defaults ())))
      (body
       ((Pass
         (location
          ((start ((line 1) (column 11))) (stop ((line 1) (column 15))))))))
      (decorator_list ()) (returns ()) (type_comment ()))))
   (type_ignores ()))
  $ echo "def foo(x, *args, **kwargs): pass" | parse module -
  ((body
    ((FunctionDef
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 33)))))
      (name foo)
      (args
       ((posonlyargs ())
        (args
         (((location
            ((start ((line 1) (column 8))) (stop ((line 1) (column 9)))))
           (identifier x) (annotation ()) (type_comment ()))))
        (vararg
         (((location
            ((start ((line 1) (column 12))) (stop ((line 1) (column 16)))))
           (identifier args) (annotation ()) (type_comment ()))))
        (kwonlyargs ()) (kw_defaults ())
        (kwarg
         (((location
            ((start ((line 1) (column 20))) (stop ((line 1) (column 26)))))
           (identifier kwargs) (annotation ()) (type_comment ()))))
        (defaults ())))
      (body
       ((Pass
         (location
          ((start ((line 1) (column 29))) (stop ((line 1) (column 33))))))))
      (decorator_list ()) (returns ()) (type_comment ()))))
   (type_ignores ()))
  $ echo "def foo(x, /, y, *, z, **kwargs): pass" | parse module -
  ((body
    ((FunctionDef
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 38)))))
      (name foo)
      (args
       ((posonlyargs
         (((location
            ((start ((line 1) (column 8))) (stop ((line 1) (column 9)))))
           (identifier x) (annotation ()) (type_comment ()))))
        (args
         (((location
            ((start ((line 1) (column 14))) (stop ((line 1) (column 15)))))
           (identifier y) (annotation ()) (type_comment ()))))
        (vararg ())
        (kwonlyargs
         (((location
            ((start ((line 1) (column 20))) (stop ((line 1) (column 21)))))
           (identifier z) (annotation ()) (type_comment ()))))
        (kw_defaults (()))
        (kwarg
         (((location
            ((start ((line 1) (column 25))) (stop ((line 1) (column 31)))))
           (identifier kwargs) (annotation ()) (type_comment ()))))
        (defaults ())))
      (body
       ((Pass
         (location
          ((start ((line 1) (column 34))) (stop ((line 1) (column 38))))))))
      (decorator_list ()) (returns ()) (type_comment ()))))
   (type_ignores ()))
  $ echo "def foo(x = 1, /, y = 2, *, z = 3): pass" | parse module -
  ((body
    ((FunctionDef
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 40)))))
      (name foo)
      (args
       ((posonlyargs
         (((location
            ((start ((line 1) (column 8))) (stop ((line 1) (column 9)))))
           (identifier x) (annotation ()) (type_comment ()))))
        (args
         (((location
            ((start ((line 1) (column 18))) (stop ((line 1) (column 19)))))
           (identifier y) (annotation ()) (type_comment ()))))
        (vararg ())
        (kwonlyargs
         (((location
            ((start ((line 1) (column 28))) (stop ((line 1) (column 29)))))
           (identifier z) (annotation ()) (type_comment ()))))
        (kw_defaults
         (((Constant
            (location
             ((start ((line 1) (column 32))) (stop ((line 1) (column 33)))))
            (value (Integer 3)) (kind ())))))
        (kwarg ())
        (defaults
         ((Constant
           (location
            ((start ((line 1) (column 12))) (stop ((line 1) (column 13)))))
           (value (Integer 1)) (kind ()))
          (Constant
           (location
            ((start ((line 1) (column 22))) (stop ((line 1) (column 23)))))
           (value (Integer 2)) (kind ()))))))
      (body
       ((Pass
         (location
          ((start ((line 1) (column 36))) (stop ((line 1) (column 40))))))))
      (decorator_list ()) (returns ()) (type_comment ()))))
   (type_ignores ()))
  $ echo "def foo(x, /, y, z = 2, *, u = 3, v): pass" | parse module -
  ((body
    ((FunctionDef
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 42)))))
      (name foo)
      (args
       ((posonlyargs
         (((location
            ((start ((line 1) (column 8))) (stop ((line 1) (column 9)))))
           (identifier x) (annotation ()) (type_comment ()))))
        (args
         (((location
            ((start ((line 1) (column 14))) (stop ((line 1) (column 15)))))
           (identifier y) (annotation ()) (type_comment ()))
          ((location
            ((start ((line 1) (column 17))) (stop ((line 1) (column 18)))))
           (identifier z) (annotation ()) (type_comment ()))))
        (vararg ())
        (kwonlyargs
         (((location
            ((start ((line 1) (column 27))) (stop ((line 1) (column 28)))))
           (identifier u) (annotation ()) (type_comment ()))
          ((location
            ((start ((line 1) (column 34))) (stop ((line 1) (column 35)))))
           (identifier v) (annotation ()) (type_comment ()))))
        (kw_defaults
         (((Constant
            (location
             ((start ((line 1) (column 31))) (stop ((line 1) (column 32)))))
            (value (Integer 3)) (kind ())))
          ()))
        (kwarg ())
        (defaults
         ((Constant
           (location
            ((start ((line 1) (column 21))) (stop ((line 1) (column 22)))))
           (value (Integer 2)) (kind ()))))))
      (body
       ((Pass
         (location
          ((start ((line 1) (column 38))) (stop ((line 1) (column 42))))))))
      (decorator_list ()) (returns ()) (type_comment ()))))
   (type_ignores ()))
  $ parse module function_def_with_decorator
  ((body
    ((FunctionDef
      (location ((start ((line 3) (column 0))) (stop ((line 3) (column 15)))))
      (name foo)
      (args
       ((posonlyargs ()) (args ()) (vararg ()) (kwonlyargs ()) (kw_defaults ())
        (kwarg ()) (defaults ())))
      (body
       ((Pass
         (location
          ((start ((line 3) (column 11))) (stop ((line 3) (column 15))))))))
      (decorator_list
       ((Name
         (location
          ((start ((line 1) (column 1))) (stop ((line 1) (column 4)))))
         (id bar) (ctx Load))
        (Call
         (location
          ((start ((line 2) (column 1))) (stop ((line 2) (column 13)))))
         (func
          (Name
           (location
            ((start ((line 2) (column 1))) (stop ((line 2) (column 4)))))
           (id baz) (ctx Load)))
         (args
          ((Name
            (location
             ((start ((line 2) (column 5))) (stop ((line 2) (column 8)))))
            (id qux) (ctx Load))
           (Constant
            (location
             ((start ((line 2) (column 10))) (stop ((line 2) (column 12)))))
            (value (Integer 42)) (kind ()))))
         (keywords ()))))
      (returns ()) (type_comment ()))))
   (type_ignores ()))
  $ echo "def foo(x: int, y: int) -> int: pass" | parse module -
  ((body
    ((FunctionDef
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 36)))))
      (name foo)
      (args
       ((posonlyargs ())
        (args
         (((location
            ((start ((line 1) (column 8))) (stop ((line 1) (column 14)))))
           (identifier x)
           (annotation
            ((Name
              (location
               ((start ((line 1) (column 11))) (stop ((line 1) (column 14)))))
              (id int) (ctx Load))))
           (type_comment ()))
          ((location
            ((start ((line 1) (column 16))) (stop ((line 1) (column 22)))))
           (identifier y)
           (annotation
            ((Name
              (location
               ((start ((line 1) (column 19))) (stop ((line 1) (column 22)))))
              (id int) (ctx Load))))
           (type_comment ()))))
        (vararg ()) (kwonlyargs ()) (kw_defaults ()) (kwarg ()) (defaults ())))
      (body
       ((Pass
         (location
          ((start ((line 1) (column 32))) (stop ((line 1) (column 36))))))))
      (decorator_list ())
      (returns
       ((Name
         (location
          ((start ((line 1) (column 27))) (stop ((line 1) (column 30)))))
         (id int) (ctx Load))))
      (type_comment ()))))
   (type_ignores ()))
  $ parse module --enable-type-comment function_def_with_type_comment
  ((body
    ((FunctionDef
      (location ((start ((line 1) (column 0))) (stop ((line 2) (column 5)))))
      (name foo)
      (args
       ((posonlyargs ())
        (args
         (((location
            ((start ((line 1) (column 8))) (stop ((line 1) (column 9)))))
           (identifier x) (annotation ()) (type_comment ()))))
        (vararg ()) (kwonlyargs ()) (kw_defaults ()) (kwarg ()) (defaults ())))
      (body
       ((Expr
         (location
          ((start ((line 2) (column 2))) (stop ((line 2) (column 5)))))
         (value
          (Constant
           (location
            ((start ((line 2) (column 2))) (stop ((line 2) (column 5)))))
           (value Ellipsis) (kind ()))))))
      (decorator_list ()) (returns ()) (type_comment ("(int) -> int")))))
   (type_ignores ()))
  $ parse module --enable-type-comment param_type_comment
  ((body
    ((FunctionDef
      (location ((start ((line 1) (column 0))) (stop ((line 9) (column 8)))))
      (name send_email)
      (args
       ((posonlyargs ())
        (args
         (((location
            ((start ((line 1) (column 15))) (stop ((line 1) (column 22)))))
           (identifier address) (annotation ())
           (type_comment ("Union[str, List[str]]")))
          ((location
            ((start ((line 2) (column 15))) (stop ((line 2) (column 21)))))
           (identifier sender) (annotation ()) (type_comment (str)))
          ((location
            ((start ((line 3) (column 15))) (stop ((line 3) (column 17)))))
           (identifier cc) (annotation ())
           (type_comment (Optional[List[str]])))
          ((location
            ((start ((line 4) (column 15))) (stop ((line 4) (column 18)))))
           (identifier bcc) (annotation ())
           (type_comment (Optional[List[str]])))
          ((location
            ((start ((line 5) (column 15))) (stop ((line 5) (column 22)))))
           (identifier subject) (annotation ()) (type_comment ()))
          ((location
            ((start ((line 6) (column 15))) (stop ((line 6) (column 19)))))
           (identifier body) (annotation ()) (type_comment (List[str])))))
        (vararg ()) (kwonlyargs ()) (kw_defaults ()) (kwarg ())
        (defaults
         ((Constant
           (location
            ((start ((line 5) (column 23))) (stop ((line 5) (column 25)))))
           (value (String "")) (kind ()))
          (Constant
           (location
            ((start ((line 6) (column 20))) (stop ((line 6) (column 24)))))
           (value None) (kind ()))))))
      (body
       ((Pass
         (location
          ((start ((line 9) (column 4))) (stop ((line 9) (column 8))))))))
      (decorator_list ()) (returns ()) (type_comment ("(...) -> bool")))))
   (type_ignores ()))
  $ parse module function_def_multiline
  ((body
    ((FunctionDef
      (location ((start ((line 1) (column 0))) (stop ((line 3) (column 3)))))
      (name foo)
      (args
       ((posonlyargs ()) (args ()) (vararg ()) (kwonlyargs ()) (kw_defaults ())
        (kwarg ()) (defaults ())))
      (body
       ((Expr
         (location
          ((start ((line 2) (column 2))) (stop ((line 2) (column 3)))))
         (value
          (Name
           (location
            ((start ((line 2) (column 2))) (stop ((line 2) (column 3)))))
           (id x) (ctx Load))))
        (Expr
         (location
          ((start ((line 3) (column 2))) (stop ((line 3) (column 3)))))
         (value
          (Name
           (location
            ((start ((line 3) (column 2))) (stop ((line 3) (column 3)))))
           (id y) (ctx Load))))))
      (decorator_list ()) (returns ()) (type_comment ()))))
   (type_ignores ()))

Test async function def stmt
  $ echo "async def foo(x: int, y: bool = False): pass" | parse module -
  ((body
    ((AsyncFunctionDef
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 44)))))
      (name foo)
      (args
       ((posonlyargs ())
        (args
         (((location
            ((start ((line 1) (column 14))) (stop ((line 1) (column 20)))))
           (identifier x)
           (annotation
            ((Name
              (location
               ((start ((line 1) (column 17))) (stop ((line 1) (column 20)))))
              (id int) (ctx Load))))
           (type_comment ()))
          ((location
            ((start ((line 1) (column 22))) (stop ((line 1) (column 29)))))
           (identifier y)
           (annotation
            ((Name
              (location
               ((start ((line 1) (column 25))) (stop ((line 1) (column 29)))))
              (id bool) (ctx Load))))
           (type_comment ()))))
        (vararg ()) (kwonlyargs ()) (kw_defaults ()) (kwarg ())
        (defaults
         ((Constant
           (location
            ((start ((line 1) (column 32))) (stop ((line 1) (column 37)))))
           (value False) (kind ()))))))
      (body
       ((Pass
         (location
          ((start ((line 1) (column 40))) (stop ((line 1) (column 44))))))))
      (decorator_list ()) (returns ()) (type_comment ()))))
   (type_ignores ()))

Test class def stmt
  $ echo "class Foo: pass" | parse module -
  ((body
    ((ClassDef
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 15)))))
      (name Foo) (bases ()) (keywords ())
      (body
       ((Pass
         (location
          ((start ((line 1) (column 11))) (stop ((line 1) (column 15))))))))
      (decorator_list ()))))
   (type_ignores ()))
  $ echo "class Foo(Bar, Baz, metaclass=FooMeta): pass" | parse module -
  ((body
    ((ClassDef
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 44)))))
      (name Foo)
      (bases
       ((Name
         (location
          ((start ((line 1) (column 10))) (stop ((line 1) (column 13)))))
         (id Bar) (ctx Load))
        (Name
         (location
          ((start ((line 1) (column 15))) (stop ((line 1) (column 18)))))
         (id Baz) (ctx Load))))
      (keywords
       (((location
          ((start ((line 1) (column 20))) (stop ((line 1) (column 37)))))
         (arg (metaclass))
         (value
          (Name
           (location
            ((start ((line 1) (column 30))) (stop ((line 1) (column 37)))))
           (id FooMeta) (ctx Load))))))
      (body
       ((Pass
         (location
          ((start ((line 1) (column 40))) (stop ((line 1) (column 44))))))))
      (decorator_list ()))))
   (type_ignores ()))
  $ parse module class_def_with_method
  ((body
    ((ClassDef
      (location ((start ((line 1) (column 0))) (stop ((line 3) (column 13)))))
      (name Foo) (bases ()) (keywords ())
      (body
       ((FunctionDef
         (location
          ((start ((line 2) (column 2))) (stop ((line 3) (column 13)))))
         (name foo)
         (args
          ((posonlyargs ())
           (args
            (((location
               ((start ((line 2) (column 10))) (stop ((line 2) (column 14)))))
              (identifier self) (annotation ()) (type_comment ()))))
           (vararg ()) (kwonlyargs ()) (kw_defaults ()) (kwarg ())
           (defaults ())))
         (body
          ((Return
            (location
             ((start ((line 3) (column 4))) (stop ((line 3) (column 13)))))
            (value
             ((Constant
               (location
                ((start ((line 3) (column 11))) (stop ((line 3) (column 13)))))
               (value (Integer 42)) (kind ())))))))
         (decorator_list ())
         (returns
          ((Name
            (location
             ((start ((line 2) (column 19))) (stop ((line 2) (column 22)))))
            (id int) (ctx Load))))
         (type_comment ()))))
      (decorator_list ()))))
   (type_ignores ()))
  $ parse module class_def_with_decorator
  ((body
    ((ClassDef
      (location ((start ((line 3) (column 0))) (stop ((line 4) (column 6)))))
      (name Foo) (bases ()) (keywords ())
      (body
       ((Pass
         (location
          ((start ((line 4) (column 2))) (stop ((line 4) (column 6))))))))
      (decorator_list
       ((Name
         (location
          ((start ((line 1) (column 1))) (stop ((line 1) (column 4)))))
         (id bar) (ctx Load))
        (Call
         (location
          ((start ((line 2) (column 1))) (stop ((line 2) (column 13)))))
         (func
          (Name
           (location
            ((start ((line 2) (column 1))) (stop ((line 2) (column 4)))))
           (id baz) (ctx Load)))
         (args
          ((Name
            (location
             ((start ((line 2) (column 5))) (stop ((line 2) (column 8)))))
            (id qux) (ctx Load))
           (Constant
            (location
             ((start ((line 2) (column 10))) (stop ((line 2) (column 12)))))
            (value (Integer 42)) (kind ()))))
         (keywords ())))))))
   (type_ignores ()))

Test return stmt
  $ echo "return" | parse module -
  ((body
    ((Return
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
      (value ()))))
   (type_ignores ()))
  $ echo "return 42" | parse module -
  ((body
    ((Return
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 9)))))
      (value
       ((Constant
         (location
          ((start ((line 1) (column 7))) (stop ((line 1) (column 9)))))
         (value (Integer 42)) (kind ())))))))
   (type_ignores ()))

Test delete stmt
  $ echo "del x" | parse module -
  ((body
    ((Delete
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
      (targets
       ((Name
         (location
          ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
         (id x) (ctx Del)))))))
   (type_ignores ()))
  $ echo "del x.y, z[0]" | parse module -
  ((body
    ((Delete
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 13)))))
      (targets
       ((Attribute
         (location
          ((start ((line 1) (column 4))) (stop ((line 1) (column 7)))))
         (value
          (Name
           (location
            ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
           (id x) (ctx Load)))
         (attr y) (ctx Del))
        (Subscript
         (location
          ((start ((line 1) (column 9))) (stop ((line 1) (column 13)))))
         (value
          (Name
           (location
            ((start ((line 1) (column 9))) (stop ((line 1) (column 10)))))
           (id z) (ctx Load)))
         (slice
          (Constant
           (location
            ((start ((line 1) (column 11))) (stop ((line 1) (column 12)))))
           (value (Integer 0)) (kind ())))
         (ctx Del)))))))
   (type_ignores ()))

Test assign stmt
  $ echo "x = 42" | parse module -
  ((body
    ((Assign
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
      (targets
       ((Name
         (location
          ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
         (id x) (ctx Store))))
      (value
       (Constant
        (location ((start ((line 1) (column 4))) (stop ((line 1) (column 6)))))
        (value (Integer 42)) (kind ())))
      (type_comment ()))))
   (type_ignores ()))
  $ echo "x = 42 # type: int" | parse module --enable-type-comment -
  ((body
    ((Assign
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 18)))))
      (targets
       ((Name
         (location
          ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
         (id x) (ctx Store))))
      (value
       (Constant
        (location ((start ((line 1) (column 4))) (stop ((line 1) (column 6)))))
        (value (Integer 42)) (kind ())))
      (type_comment (int)))))
   (type_ignores ()))
  $ echo "x.y = 42" | parse module -
  ((body
    ((Assign
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 8)))))
      (targets
       ((Attribute
         (location
          ((start ((line 1) (column 0))) (stop ((line 1) (column 3)))))
         (value
          (Name
           (location
            ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
           (id x) (ctx Load)))
         (attr y) (ctx Store))))
      (value
       (Constant
        (location ((start ((line 1) (column 6))) (stop ((line 1) (column 8)))))
        (value (Integer 42)) (kind ())))
      (type_comment ()))))
   (type_ignores ()))
  $ echo "x[y:z] = 42" | parse module -
  ((body
    ((Assign
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 11)))))
      (targets
       ((Subscript
         (location
          ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
         (value
          (Name
           (location
            ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
           (id x) (ctx Load)))
         (slice
          (Slice
           (location
            ((start ((line 1) (column 2))) (stop ((line 1) (column 5)))))
           (lower
            ((Name
              (location
               ((start ((line 1) (column 2))) (stop ((line 1) (column 3)))))
              (id y) (ctx Load))))
           (upper
            ((Name
              (location
               ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
              (id z) (ctx Load))))
           (step ())))
         (ctx Store))))
      (value
       (Constant
        (location
         ((start ((line 1) (column 9))) (stop ((line 1) (column 11)))))
        (value (Integer 42)) (kind ())))
      (type_comment ()))))
   (type_ignores ()))
  $ echo "*x = 42" | parse module -
  ((body
    ((Assign
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 7)))))
      (targets
       ((Starred
         (location
          ((start ((line 1) (column 0))) (stop ((line 1) (column 2)))))
         (value
          (Name
           (location
            ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
           (id x) (ctx Store)))
         (ctx Store))))
      (value
       (Constant
        (location ((start ((line 1) (column 5))) (stop ((line 1) (column 7)))))
        (value (Integer 42)) (kind ())))
      (type_comment ()))))
   (type_ignores ()))
  $ echo "x, y = 42" | parse module -
  ((body
    ((Assign
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 9)))))
      (targets
       ((Tuple
         (location
          ((start ((line 1) (column 0))) (stop ((line 1) (column 4)))))
         (elts
          ((Name
            (location
             ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
            (id x) (ctx Store))
           (Name
            (location
             ((start ((line 1) (column 3))) (stop ((line 1) (column 4)))))
            (id y) (ctx Store))))
         (ctx Store))))
      (value
       (Constant
        (location ((start ((line 1) (column 7))) (stop ((line 1) (column 9)))))
        (value (Integer 42)) (kind ())))
      (type_comment ()))))
   (type_ignores ()))
  $ echo "[(x, y), *z] = 42" | parse module -
  ((body
    ((Assign
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 17)))))
      (targets
       ((List
         (location
          ((start ((line 1) (column 0))) (stop ((line 1) (column 12)))))
         (elts
          ((Tuple
            (location
             ((start ((line 1) (column 1))) (stop ((line 1) (column 7)))))
            (elts
             ((Name
               (location
                ((start ((line 1) (column 2))) (stop ((line 1) (column 3)))))
               (id x) (ctx Store))
              (Name
               (location
                ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
               (id y) (ctx Store))))
            (ctx Store))
           (Starred
            (location
             ((start ((line 1) (column 9))) (stop ((line 1) (column 11)))))
            (value
             (Name
              (location
               ((start ((line 1) (column 10))) (stop ((line 1) (column 11)))))
              (id z) (ctx Store)))
            (ctx Store))))
         (ctx Store))))
      (value
       (Constant
        (location
         ((start ((line 1) (column 15))) (stop ((line 1) (column 17)))))
        (value (Integer 42)) (kind ())))
      (type_comment ()))))
   (type_ignores ()))

Test augmented assign stmt
  $ echo "x += 42" | parse module -
  ((body
    ((AugAssign
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 7)))))
      (target
       (Name
        (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
        (id x) (ctx Store)))
      (op Add)
      (value
       (Constant
        (location ((start ((line 1) (column 5))) (stop ((line 1) (column 7)))))
        (value (Integer 42)) (kind ()))))))
   (type_ignores ()))
  $ echo "x.y += 42" | parse module -
  ((body
    ((AugAssign
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 9)))))
      (target
       (Attribute
        (location ((start ((line 1) (column 0))) (stop ((line 1) (column 3)))))
        (value
         (Name
          (location
           ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
          (id x) (ctx Load)))
        (attr y) (ctx Store)))
      (op Add)
      (value
       (Constant
        (location ((start ((line 1) (column 7))) (stop ((line 1) (column 9)))))
        (value (Integer 42)) (kind ()))))))
   (type_ignores ()))
  $ echo "x[y] += 42" | parse module -
  ((body
    ((AugAssign
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 10)))))
      (target
       (Subscript
        (location ((start ((line 1) (column 0))) (stop ((line 1) (column 4)))))
        (value
         (Name
          (location
           ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
          (id x) (ctx Load)))
        (slice
         (Name
          (location
           ((start ((line 1) (column 2))) (stop ((line 1) (column 3)))))
          (id y) (ctx Load)))
        (ctx Store)))
      (op Add)
      (value
       (Constant
        (location
         ((start ((line 1) (column 8))) (stop ((line 1) (column 10)))))
        (value (Integer 42)) (kind ()))))))
   (type_ignores ()))

Test annotated assign stmt
  $ echo "x: int = 42" | parse module -
  ((body
    ((AnnAssign
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 11)))))
      (target
       (Name
        (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
        (id x) (ctx Store)))
      (annotation
       (Name
        (location ((start ((line 1) (column 3))) (stop ((line 1) (column 6)))))
        (id int) (ctx Load)))
      (value
       ((Constant
         (location
          ((start ((line 1) (column 9))) (stop ((line 1) (column 11)))))
         (value (Integer 42)) (kind ()))))
      (simple true))))
   (type_ignores ()))
  $ echo "(x): int = 42" | parse module -
  ((body
    ((AnnAssign
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 13)))))
      (target
       (Name
        (location ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
        (id x) (ctx Store)))
      (annotation
       (Name
        (location ((start ((line 1) (column 5))) (stop ((line 1) (column 8)))))
        (id int) (ctx Load)))
      (value
       ((Constant
         (location
          ((start ((line 1) (column 11))) (stop ((line 1) (column 13)))))
         (value (Integer 42)) (kind ()))))
      (simple false))))
   (type_ignores ()))
  $ echo "x.y: int = 42" | parse module -
  ((body
    ((AnnAssign
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 13)))))
      (target
       (Attribute
        (location ((start ((line 1) (column 0))) (stop ((line 1) (column 3)))))
        (value
         (Name
          (location
           ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
          (id x) (ctx Load)))
        (attr y) (ctx Store)))
      (annotation
       (Name
        (location ((start ((line 1) (column 5))) (stop ((line 1) (column 8)))))
        (id int) (ctx Load)))
      (value
       ((Constant
         (location
          ((start ((line 1) (column 11))) (stop ((line 1) (column 13)))))
         (value (Integer 42)) (kind ()))))
      (simple false))))
   (type_ignores ()))
  $ echo "x[y]: int = 42" | parse module -
  ((body
    ((AnnAssign
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 14)))))
      (target
       (Subscript
        (location ((start ((line 1) (column 0))) (stop ((line 1) (column 4)))))
        (value
         (Name
          (location
           ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
          (id x) (ctx Load)))
        (slice
         (Name
          (location
           ((start ((line 1) (column 2))) (stop ((line 1) (column 3)))))
          (id y) (ctx Load)))
        (ctx Store)))
      (annotation
       (Name
        (location ((start ((line 1) (column 6))) (stop ((line 1) (column 9)))))
        (id int) (ctx Load)))
      (value
       ((Constant
         (location
          ((start ((line 1) (column 12))) (stop ((line 1) (column 14)))))
         (value (Integer 42)) (kind ()))))
      (simple false))))
   (type_ignores ()))
  $ echo "x: int" | parse module -
  ((body
    ((AnnAssign
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
      (target
       (Name
        (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
        (id x) (ctx Store)))
      (annotation
       (Name
        (location ((start ((line 1) (column 3))) (stop ((line 1) (column 6)))))
        (id int) (ctx Load)))
      (value ()) (simple true))))
   (type_ignores ()))

Test for stmt
  $ echo "for x in y: pass" | parse module -
  ((body
    ((For
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 16)))))
      (target
       (Name
        (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
        (id x) (ctx Store)))
      (iter
       (Name
        (location
         ((start ((line 1) (column 9))) (stop ((line 1) (column 10)))))
        (id y) (ctx Load)))
      (body
       ((Pass
         (location
          ((start ((line 1) (column 12))) (stop ((line 1) (column 16))))))))
      (orelse ()) (type_comment ()))))
   (type_ignores ()))
  $ parse module --enable-type-comment for_with_else
  ((body
    ((For
      (location ((start ((line 1) (column 0))) (stop ((line 4) (column 8)))))
      (target
       (Tuple
        (location ((start ((line 1) (column 4))) (stop ((line 1) (column 8)))))
        (elts
         ((Name
           (location
            ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
           (id x) (ctx Store))
          (Name
           (location
            ((start ((line 1) (column 7))) (stop ((line 1) (column 8)))))
           (id y) (ctx Store))))
        (ctx Store)))
      (iter
       (Call
        (location
         ((start ((line 1) (column 12))) (stop ((line 1) (column 15)))))
        (func
         (Name
          (location
           ((start ((line 1) (column 12))) (stop ((line 1) (column 13)))))
          (id l) (ctx Load)))
        (args ()) (keywords ())))
      (body
       ((Expr
         (location
          ((start ((line 2) (column 4))) (stop ((line 2) (column 13)))))
         (value
          (Call
           (location
            ((start ((line 2) (column 4))) (stop ((line 2) (column 13)))))
           (func
            (Name
             (location
              ((start ((line 2) (column 4))) (stop ((line 2) (column 7)))))
             (id foo) (ctx Load)))
           (args
            ((Name
              (location
               ((start ((line 2) (column 8))) (stop ((line 2) (column 9)))))
              (id x) (ctx Load))
             (Name
              (location
               ((start ((line 2) (column 11))) (stop ((line 2) (column 12)))))
              (id y) (ctx Load))))
           (keywords ()))))))
      (orelse
       ((Pass
         (location
          ((start ((line 4) (column 4))) (stop ((line 4) (column 8))))))))
      (type_comment ("Tuple[int, int]")))))
   (type_ignores ()))

Test async for stmt
  $ echo "async for x in y: pass" | parse module -
  ((body
    ((AsyncFor
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 22)))))
      (target
       (Name
        (location
         ((start ((line 1) (column 10))) (stop ((line 1) (column 11)))))
        (id x) (ctx Store)))
      (iter
       (Name
        (location
         ((start ((line 1) (column 15))) (stop ((line 1) (column 16)))))
        (id y) (ctx Load)))
      (body
       ((Pass
         (location
          ((start ((line 1) (column 18))) (stop ((line 1) (column 22))))))))
      (orelse ()) (type_comment ()))))
   (type_ignores ()))

Test while stmt
  $ echo "while x < y: pass" | parse module -
  ((body
    ((While
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 17)))))
      (test
       (Compare
        (location
         ((start ((line 1) (column 6))) (stop ((line 1) (column 11)))))
        (left
         (Name
          (location
           ((start ((line 1) (column 6))) (stop ((line 1) (column 7)))))
          (id x) (ctx Load)))
        (ops (Lt))
        (comparators
         ((Name
           (location
            ((start ((line 1) (column 10))) (stop ((line 1) (column 11)))))
           (id y) (ctx Load))))))
      (body
       ((Pass
         (location
          ((start ((line 1) (column 13))) (stop ((line 1) (column 17))))))))
      (orelse ()))))
   (type_ignores ()))
  $ parse module while_multiline
  ((body
    ((While
      (location ((start ((line 1) (column 0))) (stop ((line 4) (column 3)))))
      (test
       (Call
        (location
         ((start ((line 1) (column 6))) (stop ((line 1) (column 12)))))
        (func
         (Name
          (location
           ((start ((line 1) (column 6))) (stop ((line 1) (column 10)))))
          (id derp) (ctx Load)))
        (args ()) (keywords ())))
      (body
       ((Expr
         (location
          ((start ((line 2) (column 2))) (stop ((line 2) (column 3)))))
         (value
          (Name
           (location
            ((start ((line 2) (column 2))) (stop ((line 2) (column 3)))))
           (id z) (ctx Load))))))
      (orelse
       ((Expr
         (location
          ((start ((line 4) (column 2))) (stop ((line 4) (column 3)))))
         (value
          (Name
           (location
            ((start ((line 4) (column 2))) (stop ((line 4) (column 3)))))
           (id w) (ctx Load)))))))))
   (type_ignores ()))

Test if stmt
  $ echo "if x < y: pass" | parse module -
  ((body
    ((If
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 14)))))
      (test
       (Compare
        (location ((start ((line 1) (column 3))) (stop ((line 1) (column 8)))))
        (left
         (Name
          (location
           ((start ((line 1) (column 3))) (stop ((line 1) (column 4)))))
          (id x) (ctx Load)))
        (ops (Lt))
        (comparators
         ((Name
           (location
            ((start ((line 1) (column 7))) (stop ((line 1) (column 8)))))
           (id y) (ctx Load))))))
      (body
       ((Pass
         (location
          ((start ((line 1) (column 10))) (stop ((line 1) (column 14))))))))
      (orelse ()))))
   (type_ignores ()))
  $ parse module if_stmt_multiline
  ((body
    ((If
      (location ((start ((line 1) (column 0))) (stop ((line 4) (column 7)))))
      (test
       (Call
        (location ((start ((line 1) (column 3))) (stop ((line 1) (column 9)))))
        (func
         (Name
          (location
           ((start ((line 1) (column 3))) (stop ((line 1) (column 7)))))
          (id derp) (ctx Load)))
        (args ()) (keywords ())))
      (body
       ((Expr
         (location
          ((start ((line 2) (column 2))) (stop ((line 2) (column 6)))))
         (value
          (Constant
           (location
            ((start ((line 2) (column 2))) (stop ((line 2) (column 6)))))
           (value True) (kind ()))))))
      (orelse
       ((Expr
         (location
          ((start ((line 4) (column 2))) (stop ((line 4) (column 7)))))
         (value
          (Constant
           (location
            ((start ((line 4) (column 2))) (stop ((line 4) (column 7)))))
           (value False) (kind ())))))))))
   (type_ignores ()))

Test with stmt
  $ parse module --enable-type-comment with_stmt_with_type_comment
  ((body
    ((With
      (location ((start ((line 1) (column 0))) (stop ((line 2) (column 6)))))
      (items
       (((context_expr
          (Call
           (location
            ((start ((line 1) (column 5))) (stop ((line 1) (column 10)))))
           (func
            (Name
             (location
              ((start ((line 1) (column 5))) (stop ((line 1) (column 8)))))
             (id foo) (ctx Load)))
           (args ()) (keywords ())))
         (optional_vars
          ((Name
            (location
             ((start ((line 1) (column 14))) (stop ((line 1) (column 17)))))
            (id bar) (ctx Store)))))))
      (body
       ((Pass
         (location
          ((start ((line 2) (column 2))) (stop ((line 2) (column 6))))))))
      (type_comment (int)))))
   (type_ignores ()))
  $ parse module multiple_with_items
  ((body
    ((With
      (location ((start ((line 1) (column 0))) (stop ((line 5) (column 8)))))
      (items
       (((context_expr
          (Call
           (location
            ((start ((line 2) (column 4))) (stop ((line 2) (column 17)))))
           (func
            (Name
             (location
              ((start ((line 2) (column 4))) (stop ((line 2) (column 15)))))
             (id CtxManager1) (ctx Load)))
           (args ()) (keywords ())))
         (optional_vars ()))
        ((context_expr
          (Call
           (location
            ((start ((line 3) (column 4))) (stop ((line 3) (column 17)))))
           (func
            (Name
             (location
              ((start ((line 3) (column 4))) (stop ((line 3) (column 15)))))
             (id CtxManager2) (ctx Load)))
           (args ()) (keywords ())))
         (optional_vars
          ((Name
            (location
             ((start ((line 3) (column 21))) (stop ((line 3) (column 28)))))
            (id example) (ctx Store)))))))
      (body
       ((Pass
         (location
          ((start ((line 5) (column 4))) (stop ((line 5) (column 8))))))))
      (type_comment ()))))
   (type_ignores ()))

Test async with stmt
  $ echo "async with foo() as bar: pass" | parse module -
  ((body
    ((AsyncWith
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 29)))))
      (items
       (((context_expr
          (Call
           (location
            ((start ((line 1) (column 11))) (stop ((line 1) (column 16)))))
           (func
            (Name
             (location
              ((start ((line 1) (column 11))) (stop ((line 1) (column 14)))))
             (id foo) (ctx Load)))
           (args ()) (keywords ())))
         (optional_vars
          ((Name
            (location
             ((start ((line 1) (column 20))) (stop ((line 1) (column 23)))))
            (id bar) (ctx Store)))))))
      (body
       ((Pass
         (location
          ((start ((line 1) (column 25))) (stop ((line 1) (column 29))))))))
      (type_comment ()))))
   (type_ignores ()))

Test raise stmt
  $ echo "raise" | parse module -
  ((body
    ((Raise
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
      (exc ()) (cause ()))))
   (type_ignores ()))
  $ echo "raise foo" | parse module -
  ((body
    ((Raise
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 9)))))
      (exc
       ((Name
         (location
          ((start ((line 1) (column 6))) (stop ((line 1) (column 9)))))
         (id foo) (ctx Load))))
      (cause ()))))
   (type_ignores ()))
  $ echo "raise foo from bar" | parse module -
  ((body
    ((Raise
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 18)))))
      (exc
       ((Name
         (location
          ((start ((line 1) (column 6))) (stop ((line 1) (column 9)))))
         (id foo) (ctx Load))))
      (cause
       ((Name
         (location
          ((start ((line 1) (column 15))) (stop ((line 1) (column 18)))))
         (id bar) (ctx Load)))))))
   (type_ignores ()))

Test try stmt
  $ parse module try_except_finally_else
  ((body
    ((Try
      (location ((start ((line 1) (column 0))) (stop ((line 11) (column 9)))))
      (body
       ((Assign
         (location
          ((start ((line 2) (column 4))) (stop ((line 2) (column 9)))))
         (targets
          ((Name
            (location
             ((start ((line 2) (column 4))) (stop ((line 2) (column 5)))))
            (id x) (ctx Store))))
         (value
          (Name
           (location
            ((start ((line 2) (column 8))) (stop ((line 2) (column 9)))))
           (id y) (ctx Load)))
         (type_comment ()))
        (Return
         (location
          ((start ((line 3) (column 4))) (stop ((line 3) (column 12)))))
         (value
          ((Name
            (location
             ((start ((line 3) (column 11))) (stop ((line 3) (column 12)))))
            (id x) (ctx Load)))))))
      (handlers
       (((location
          ((start ((line 4) (column 0))) (stop ((line 5) (column 14)))))
         (type_
          ((Name
            (location
             ((start ((line 4) (column 7))) (stop ((line 4) (column 10)))))
            (id Foo) (ctx Load))))
         (name (foo))
         (body
          ((Return
            (location
             ((start ((line 5) (column 4))) (stop ((line 5) (column 14)))))
            (value
             ((Name
               (location
                ((start ((line 5) (column 11))) (stop ((line 5) (column 14)))))
               (id foo) (ctx Load))))))))
        ((location
          ((start ((line 6) (column 0))) (stop ((line 7) (column 15)))))
         (type_
          ((Name
            (location
             ((start ((line 6) (column 7))) (stop ((line 6) (column 10)))))
            (id Bar) (ctx Load))))
         (name ())
         (body
          ((Return
            (location
             ((start ((line 7) (column 4))) (stop ((line 7) (column 15)))))
            (value
             ((Constant
               (location
                ((start ((line 7) (column 11))) (stop ((line 7) (column 15)))))
               (value None) (kind ()))))))))))
      (orelse
       ((Expr
         (location
          ((start ((line 9) (column 4))) (stop ((line 9) (column 9)))))
         (value
          (Call
           (location
            ((start ((line 9) (column 4))) (stop ((line 9) (column 9)))))
           (func
            (Name
             (location
              ((start ((line 9) (column 4))) (stop ((line 9) (column 7)))))
             (id baz) (ctx Load)))
           (args ()) (keywords ()))))))
      (finalbody
       ((Expr
         (location
          ((start ((line 11) (column 4))) (stop ((line 11) (column 9)))))
         (value
          (Call
           (location
            ((start ((line 11) (column 4))) (stop ((line 11) (column 9)))))
           (func
            (Name
             (location
              ((start ((line 11) (column 4))) (stop ((line 11) (column 7)))))
             (id qux) (ctx Load)))
           (args ()) (keywords ())))))))))
   (type_ignores ()))

Test wildcard try
  $ parse module wildcard_try
  ((body
    ((Try
      (location ((start ((line 1) (column 0))) (stop ((line 4) (column 6)))))
      (body
       ((Pass
         (location
          ((start ((line 2) (column 2))) (stop ((line 2) (column 6))))))))
      (handlers
       (((location
          ((start ((line 3) (column 0))) (stop ((line 4) (column 6)))))
         (type_ ()) (name ())
         (body
          ((Pass
            (location
             ((start ((line 4) (column 2))) (stop ((line 4) (column 6)))))))))))
      (orelse ()) (finalbody ()))))
   (type_ignores ()))

Test try star stmt
  $ parse module try_star_except_finally_else
  ((body
    ((TryStar
      (location ((start ((line 1) (column 0))) (stop ((line 11) (column 9)))))
      (body
       ((Assign
         (location
          ((start ((line 2) (column 4))) (stop ((line 2) (column 9)))))
         (targets
          ((Name
            (location
             ((start ((line 2) (column 4))) (stop ((line 2) (column 5)))))
            (id x) (ctx Store))))
         (value
          (Name
           (location
            ((start ((line 2) (column 8))) (stop ((line 2) (column 9)))))
           (id y) (ctx Load)))
         (type_comment ()))
        (Return
         (location
          ((start ((line 3) (column 4))) (stop ((line 3) (column 12)))))
         (value
          ((Name
            (location
             ((start ((line 3) (column 11))) (stop ((line 3) (column 12)))))
            (id x) (ctx Load)))))))
      (handlers
       (((location
          ((start ((line 4) (column 0))) (stop ((line 5) (column 14)))))
         (type_
          ((Name
            (location
             ((start ((line 4) (column 8))) (stop ((line 4) (column 11)))))
            (id Foo) (ctx Load))))
         (name (foo))
         (body
          ((Return
            (location
             ((start ((line 5) (column 4))) (stop ((line 5) (column 14)))))
            (value
             ((Name
               (location
                ((start ((line 5) (column 11))) (stop ((line 5) (column 14)))))
               (id foo) (ctx Load))))))))
        ((location
          ((start ((line 6) (column 0))) (stop ((line 7) (column 15)))))
         (type_
          ((Name
            (location
             ((start ((line 6) (column 8))) (stop ((line 6) (column 11)))))
            (id Bar) (ctx Load))))
         (name ())
         (body
          ((Return
            (location
             ((start ((line 7) (column 4))) (stop ((line 7) (column 15)))))
            (value
             ((Constant
               (location
                ((start ((line 7) (column 11))) (stop ((line 7) (column 15)))))
               (value None) (kind ()))))))))))
      (orelse
       ((Expr
         (location
          ((start ((line 9) (column 4))) (stop ((line 9) (column 9)))))
         (value
          (Call
           (location
            ((start ((line 9) (column 4))) (stop ((line 9) (column 9)))))
           (func
            (Name
             (location
              ((start ((line 9) (column 4))) (stop ((line 9) (column 7)))))
             (id baz) (ctx Load)))
           (args ()) (keywords ()))))))
      (finalbody
       ((Expr
         (location
          ((start ((line 11) (column 4))) (stop ((line 11) (column 9)))))
         (value
          (Call
           (location
            ((start ((line 11) (column 4))) (stop ((line 11) (column 9)))))
           (func
            (Name
             (location
              ((start ((line 11) (column 4))) (stop ((line 11) (column 7)))))
             (id qux) (ctx Load)))
           (args ()) (keywords ())))))))))
   (type_ignores ()))

Test mixing try and try star
  $ parse module mix_try_and_try_star
  Parse error at line 6, column 9 to line 6, column -1: cannot have both 'except' and 'except*' on the same 'try'

Test wildcard try star
  $ parse module wildcard_try_star
  Parse error at line 3, column 8 to line 3, column 9: expected one or more exception types

Test assert stmt
  $ echo "assert foo" | parse module -
  ((body
    ((Assert
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 10)))))
      (test
       (Name
        (location
         ((start ((line 1) (column 7))) (stop ((line 1) (column 10)))))
        (id foo) (ctx Load)))
      (msg ()))))
   (type_ignores ()))
  $ echo "assert foo, 'derp'" | parse module -
  ((body
    ((Assert
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 18)))))
      (test
       (Name
        (location
         ((start ((line 1) (column 7))) (stop ((line 1) (column 10)))))
        (id foo) (ctx Load)))
      (msg
       ((Constant
         (location
          ((start ((line 1) (column 12))) (stop ((line 1) (column 18)))))
         (value (String derp)) (kind ())))))))
   (type_ignores ()))

Test import stmt
  $ echo "import foo, bar" | parse module -
  ((body
    ((Import
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 15)))))
      (names
       (((location
          ((start ((line 1) (column 7))) (stop ((line 1) (column 10)))))
         (name foo) (asname ()))
        ((location
          ((start ((line 1) (column 12))) (stop ((line 1) (column 15)))))
         (name bar) (asname ())))))))
   (type_ignores ()))
  $ echo "import foo as bar" | parse module -
  ((body
    ((Import
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 17)))))
      (names
       (((location
          ((start ((line 1) (column 7))) (stop ((line 1) (column 17)))))
         (name foo) (asname (bar))))))))
   (type_ignores ()))

Test import from stmt
  $ echo "from foo import bar, baz as qux" | parse module -
  ((body
    ((ImportFrom
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 31)))))
      (module_ (foo))
      (names
       (((location
          ((start ((line 1) (column 16))) (stop ((line 1) (column 19)))))
         (name bar) (asname ()))
        ((location
          ((start ((line 1) (column 21))) (stop ((line 1) (column 31)))))
         (name baz) (asname (qux)))))
      (level 0))))
   (type_ignores ()))
  $ echo "from . import derp" | parse module -
  ((body
    ((ImportFrom
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 18)))))
      (module_ ())
      (names
       (((location
          ((start ((line 1) (column 14))) (stop ((line 1) (column 18)))))
         (name derp) (asname ()))))
      (level 1))))
   (type_ignores ()))
  $ echo "from ...x import y, z as w" | parse module -
  ((body
    ((ImportFrom
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 26)))))
      (module_ (x))
      (names
       (((location
          ((start ((line 1) (column 17))) (stop ((line 1) (column 18)))))
         (name y) (asname ()))
        ((location
          ((start ((line 1) (column 20))) (stop ((line 1) (column 26)))))
         (name z) (asname (w)))))
      (level 3))))
   (type_ignores ()))

Test global stmt
  $ echo "global x, y" | parse module -
  ((body
    ((Global
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 11)))))
      (names (x y)))))
   (type_ignores ()))

Test nonlocal stmt
  $ echo "nonlocal x, y" | parse module -
  ((body
    ((Nonlocal
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 13)))))
      (names (x y)))))
   (type_ignores ()))

Test expr stmt
  $ echo "42" | parse module -
  ((body
    ((Expr
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 2)))))
      (value
       (Constant
        (location ((start ((line 1) (column 0))) (stop ((line 1) (column 2)))))
        (value (Integer 42)) (kind ()))))))
   (type_ignores ()))

Test pass stmt
  $ echo "pass" | parse module -
  ((body
    ((Pass
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 4))))))))
   (type_ignores ()))

Test break stmt
  $ echo "break" | parse module -
  ((body
    ((Break
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5))))))))
   (type_ignores ()))

Test continue stmt
  $ echo "continue" | parse module -
  ((body
    ((Continue
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 8))))))))
   (type_ignores ()))

Test match stmt
  $ parse module matches
  ((body
    ((Match
      (location ((start ((line 1) (column 0))) (stop ((line 13) (column 8)))))
      (subject
       (Name
        (location ((start ((line 1) (column 6))) (stop ((line 1) (column 7)))))
        (id x) (ctx Load)))
      (cases
       (((pattern
          (MatchSingleton
           (location
            ((start ((line 2) (column 7))) (stop ((line 2) (column 11)))))
           (value None)))
         (guard ())
         (body
          ((Pass
            (location
             ((start ((line 3) (column 4))) (stop ((line 3) (column 8)))))))))
        ((pattern
          (MatchValue
           (location
            ((start ((line 4) (column 7))) (stop ((line 4) (column 9)))))
           (value
            (Constant
             (location
              ((start ((line 4) (column 7))) (stop ((line 4) (column 9)))))
             (value (Integer 42)) (kind ())))))
         (guard ())
         (body
          ((Pass
            (location
             ((start ((line 5) (column 4))) (stop ((line 5) (column 8)))))))))
        ((pattern
          (MatchAs
           (location
            ((start ((line 6) (column 7))) (stop ((line 6) (column 10)))))
           (pattern ()) (name (foo))))
         (guard
          ((Compare
            (location
             ((start ((line 6) (column 14))) (stop ((line 6) (column 21)))))
            (left
             (Name
              (location
               ((start ((line 6) (column 14))) (stop ((line 6) (column 17)))))
              (id foo) (ctx Load)))
            (ops (Gt))
            (comparators
             ((Constant
               (location
                ((start ((line 6) (column 20))) (stop ((line 6) (column 21)))))
               (value (Integer 0)) (kind ())))))))
         (body
          ((Pass
            (location
             ((start ((line 7) (column 4))) (stop ((line 7) (column 8)))))))))
        ((pattern
          (MatchAs
           (location
            ((start ((line 8) (column 7))) (stop ((line 8) (column 17)))))
           (pattern
            ((MatchAs
              (location
               ((start ((line 8) (column 7))) (stop ((line 8) (column 10)))))
              (pattern ()) (name (foo)))))
           (name (bar))))
         (guard ())
         (body
          ((Pass
            (location
             ((start ((line 9) (column 4))) (stop ((line 9) (column 8)))))))))
        ((pattern
          (MatchSequence
           (location
            ((start ((line 10) (column 7))) (stop ((line 10) (column 27)))))
           (patterns
            ((MatchValue
              (location
               ((start ((line 10) (column 8))) (stop ((line 10) (column 13)))))
              (value
               (Constant
                (location
                 ((start ((line 10) (column 8)))
                  (stop ((line 10) (column 13)))))
                (value (String foo)) (kind ()))))
             (MatchStar
              (location
               ((start ((line 10) (column 15))) (stop ((line 10) (column 19)))))
              (name (bar)))
             (MatchValue
              (location
               ((start ((line 10) (column 21))) (stop ((line 10) (column 26)))))
              (value
               (Constant
                (location
                 ((start ((line 10) (column 21)))
                  (stop ((line 10) (column 26)))))
                (value (String baz)) (kind ()))))))))
         (guard ())
         (body
          ((Pass
            (location
             ((start ((line 11) (column 4))) (stop ((line 11) (column 8)))))))))
        ((pattern
          (MatchMapping
           (location
            ((start ((line 12) (column 7))) (stop ((line 12) (column 49)))))
           (keys
            ((Constant
              (location
               ((start ((line 12) (column 9))) (stop ((line 12) (column 12)))))
              (value (String x)) (kind ()))
             (Constant
              (location
               ((start ((line 12) (column 31))) (stop ((line 12) (column 34)))))
              (value (String y)) (kind ()))))
           (patterns
            ((MatchOr
              (location
               ((start ((line 12) (column 15))) (stop ((line 12) (column 28)))))
              (patterns
               ((MatchValue
                 (location
                  ((start ((line 12) (column 15)))
                   (stop ((line 12) (column 20)))))
                 (value
                  (Constant
                   (location
                    ((start ((line 12) (column 15)))
                     (stop ((line 12) (column 20)))))
                   (value (String foo)) (kind ()))))
                (MatchValue
                 (location
                  ((start ((line 12) (column 23)))
                   (stop ((line 12) (column 28)))))
                 (value
                  (Constant
                   (location
                    ((start ((line 12) (column 23)))
                     (stop ((line 12) (column 28)))))
                   (value (String bar)) (kind ())))))))
             (MatchAs
              (location
               ((start ((line 12) (column 36))) (stop ((line 12) (column 39)))))
              (pattern ()) (name (bar)))))
           (rest (rest))))
         (guard ())
         (body
          ((Pass
            (location
             ((start ((line 13) (column 4))) (stop ((line 13) (column 8))))))))))))))
   (type_ignores ()))
