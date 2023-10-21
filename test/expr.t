Test basic constants
  $ echo "None" | parse expression -
  (Constant
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 4)))))
   (value None) (kind ()))
  $ echo "False" | parse expression -
  (Constant
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (value False) (kind ()))
  $ echo "True" | parse expression -
  (Constant
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 4)))))
   (value True) (kind ()))
  $ echo "..." | parse expression -
  (Constant
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 3)))))
   (value Ellipsis) (kind ()))
  $ echo "42" | parse expression -
  (Constant
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 2)))))
   (value (Integer 42)) (kind ()))
  $ echo "12345678901234567890123" | parse expression -
  (Constant
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 23)))))
   (value (BigInteger 12345678901234567890123)) (kind ()))
  $ echo "12.34" | parse expression -
  (Constant
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (value (Float 12.34)) (kind ()))
  $ echo "4.2j" | parse expression -
  (Constant
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 4)))))
   (value (Complex 4.2)) (kind ()))
  $ echo '"foo"' | parse expression -
  (Constant
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (value (String foo)) (kind ()))
  $ echo 'u"foo"' | parse expression -
  (Constant
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (value (String foo)) (kind (u)))
  $ echo 'r".\*"' | parse expression -
  (Constant
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (value (String ".\\*")) (kind ()))
  $ echo '"foo""bar"' | parse expression -
  (Constant
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 10)))))
   (value (String foobar)) (kind ()))
  $ echo 'b"foo"' | parse expression -
  (Constant
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (value (ByteString foo)) (kind ()))

Test basic identifier
  $ echo 'derp' | parse expression -
  (Name (location ((start ((line 1) (column 0))) (stop ((line 1) (column 4)))))
   (id derp) (ctx Load))
  $ echo '(derp)' | parse expression -
  (Name (location ((start ((line 1) (column 1))) (stop ((line 1) (column 5)))))
   (id derp) (ctx Load))

Test bool expression
  $ echo 'True and False' | parse expression -
  (BoolOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 14)))))
   (op And)
   (values
    ((Constant
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 4)))))
      (value True) (kind ()))
     (Constant
      (location ((start ((line 1) (column 9))) (stop ((line 1) (column 14)))))
      (value False) (kind ())))))
  $ echo 'True or False' | parse expression -
  (BoolOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 13)))))
   (op Or)
   (values
    ((Constant
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 4)))))
      (value True) (kind ()))
     (Constant
      (location ((start ((line 1) (column 8))) (stop ((line 1) (column 13)))))
      (value False) (kind ())))))
  $ echo 'x and y and z' | parse expression -
  (BoolOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 13)))))
   (op And)
   (values
    ((Name
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
      (id x) (ctx Load))
     (Name
      (location ((start ((line 1) (column 6))) (stop ((line 1) (column 7)))))
      (id y) (ctx Load))
     (Name
      (location ((start ((line 1) (column 12))) (stop ((line 1) (column 13)))))
      (id z) (ctx Load)))))
  $ echo 'x or y or z' | parse expression -
  (BoolOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 11)))))
   (op Or)
   (values
    ((Name
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
      (id x) (ctx Load))
     (Name
      (location ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
      (id y) (ctx Load))
     (Name
      (location ((start ((line 1) (column 10))) (stop ((line 1) (column 11)))))
      (id z) (ctx Load)))))
  $ echo 'x and y or z' | parse expression -
  (BoolOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 12)))))
   (op Or)
   (values
    ((BoolOp
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 7)))))
      (op And)
      (values
       ((Name
         (location
          ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
         (id x) (ctx Load))
        (Name
         (location
          ((start ((line 1) (column 6))) (stop ((line 1) (column 7)))))
         (id y) (ctx Load)))))
     (Name
      (location ((start ((line 1) (column 11))) (stop ((line 1) (column 12)))))
      (id z) (ctx Load)))))
  $ echo 'x or y and z' | parse expression -
  (BoolOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 12)))))
   (op Or)
   (values
    ((Name
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
      (id x) (ctx Load))
     (BoolOp
      (location ((start ((line 1) (column 5))) (stop ((line 1) (column 12)))))
      (op And)
      (values
       ((Name
         (location
          ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
         (id y) (ctx Load))
        (Name
         (location
          ((start ((line 1) (column 11))) (stop ((line 1) (column 12)))))
         (id z) (ctx Load))))))))

Test assignment expression
  $ echo '(x := y)' | parse expression -
  (NamedExpr
   (location ((start ((line 1) (column 1))) (stop ((line 1) (column 7)))))
   (target
    (Name
     (location ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
     (id x) (ctx Store)))
   (value
    (Name
     (location ((start ((line 1) (column 6))) (stop ((line 1) (column 7)))))
     (id y) (ctx Load))))

Test binary expression
  $ echo 'x + y' | parse expression -
  (BinOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (op Add)
   (right
    (Name
     (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
     (id y) (ctx Load))))
  $ echo 'x - y' | parse expression -
  (BinOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (op Sub)
   (right
    (Name
     (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
     (id y) (ctx Load))))
  $ echo 'x * y' | parse expression -
  (BinOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (op Mult)
   (right
    (Name
     (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
     (id y) (ctx Load))))
  $ echo 'x @ y' | parse expression -
  (BinOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (op MatMult)
   (right
    (Name
     (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
     (id y) (ctx Load))))
  $ echo 'x / y' | parse expression -
  (BinOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (op Div)
   (right
    (Name
     (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
     (id y) (ctx Load))))
  $ echo 'x % y' | parse expression -
  (BinOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (op Mod)
   (right
    (Name
     (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
     (id y) (ctx Load))))
  $ echo 'x ** y' | parse expression -
  (BinOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (op Pow)
   (right
    (Name
     (location ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
     (id y) (ctx Load))))
  $ echo 'x << y' | parse expression -
  (BinOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (op LShift)
   (right
    (Name
     (location ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
     (id y) (ctx Load))))
  $ echo 'x >> y' | parse expression -
  (BinOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (op RShift)
   (right
    (Name
     (location ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
     (id y) (ctx Load))))
  $ echo 'x | y' | parse expression -
  (BinOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (op BitOr)
   (right
    (Name
     (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
     (id y) (ctx Load))))
  $ echo 'x ^ y' | parse expression -
  (BinOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (op BitXor)
   (right
    (Name
     (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
     (id y) (ctx Load))))
  $ echo 'x & y' | parse expression -
  (BinOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (op BitAnd)
   (right
    (Name
     (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
     (id y) (ctx Load))))
  $ echo 'x // y' | parse expression -
  (BinOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (op FloorDiv)
   (right
    (Name
     (location ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
     (id y) (ctx Load))))

Test unary expression
  $ echo '~x' | parse expression -
  (UnaryOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 2)))))
   (op Invert)
   (operand
    (Name
     (location ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
     (id x) (ctx Load))))
  $ echo 'not x' | parse expression -
  (UnaryOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (op Not)
   (operand
    (Name
     (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
     (id x) (ctx Load))))
  $ echo '+x' | parse expression -
  (UnaryOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 2)))))
   (op UAdd)
   (operand
    (Name
     (location ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
     (id x) (ctx Load))))
  $ echo '-x' | parse expression -
  (UnaryOp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 2)))))
   (op USub)
   (operand
    (Name
     (location ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
     (id x) (ctx Load))))

Test lambda expression
  $ echo 'lambda: x' | parse expression -
  (Lambda
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 9)))))
   (args
    ((posonlyargs ()) (args ()) (vararg ()) (kwonlyargs ()) (kw_defaults ())
     (kwarg ()) (defaults ())))
   (body
    (Name
     (location ((start ((line 1) (column 8))) (stop ((line 1) (column 9)))))
     (id x) (ctx Load))))
  $ echo 'lambda x : x' | parse expression -
  (Lambda
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 12)))))
   (args
    ((posonlyargs ())
     (args
      (((location ((start ((line 1) (column 7))) (stop ((line 1) (column 8)))))
        (identifier x) (annotation ()) (type_comment ()))))
     (vararg ()) (kwonlyargs ()) (kw_defaults ()) (kwarg ()) (defaults ())))
   (body
    (Name
     (location ((start ((line 1) (column 11))) (stop ((line 1) (column 12)))))
     (id x) (ctx Load))))
  $ echo 'lambda x, y=2 : x' | parse expression -
  (Lambda
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 17)))))
   (args
    ((posonlyargs ())
     (args
      (((location ((start ((line 1) (column 7))) (stop ((line 1) (column 8)))))
        (identifier x) (annotation ()) (type_comment ()))
       ((location
         ((start ((line 1) (column 10))) (stop ((line 1) (column 11)))))
        (identifier y) (annotation ()) (type_comment ()))))
     (vararg ()) (kwonlyargs ()) (kw_defaults ()) (kwarg ())
     (defaults
      ((Constant
        (location
         ((start ((line 1) (column 12))) (stop ((line 1) (column 13)))))
        (value (Integer 2)) (kind ()))))))
   (body
    (Name
     (location ((start ((line 1) (column 16))) (stop ((line 1) (column 17)))))
     (id x) (ctx Load))))
  $ echo 'lambda x, *, y=2 : x' | parse expression -
  (Lambda
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 20)))))
   (args
    ((posonlyargs ())
     (args
      (((location ((start ((line 1) (column 7))) (stop ((line 1) (column 8)))))
        (identifier x) (annotation ()) (type_comment ()))))
     (vararg ())
     (kwonlyargs
      (((location
         ((start ((line 1) (column 13))) (stop ((line 1) (column 14)))))
        (identifier y) (annotation ()) (type_comment ()))))
     (kw_defaults
      (((Constant
         (location
          ((start ((line 1) (column 15))) (stop ((line 1) (column 16)))))
         (value (Integer 2)) (kind ())))))
     (kwarg ()) (defaults ())))
   (body
    (Name
     (location ((start ((line 1) (column 19))) (stop ((line 1) (column 20)))))
     (id x) (ctx Load))))
  $ echo 'lambda x, /, y, *, z : x' | parse expression -
  (Lambda
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 24)))))
   (args
    ((posonlyargs
      (((location ((start ((line 1) (column 7))) (stop ((line 1) (column 8)))))
        (identifier x) (annotation ()) (type_comment ()))))
     (args
      (((location
         ((start ((line 1) (column 13))) (stop ((line 1) (column 14)))))
        (identifier y) (annotation ()) (type_comment ()))))
     (vararg ())
     (kwonlyargs
      (((location
         ((start ((line 1) (column 19))) (stop ((line 1) (column 20)))))
        (identifier z) (annotation ()) (type_comment ()))))
     (kw_defaults (())) (kwarg ()) (defaults ())))
   (body
    (Name
     (location ((start ((line 1) (column 23))) (stop ((line 1) (column 24)))))
     (id x) (ctx Load))))

Test if expression
  $ echo 'x if y else z' | parse expression -
  (IfExp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 13)))))
   (test
    (Name
     (location ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
     (id y) (ctx Load)))
   (body
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (orelse
    (Name
     (location ((start ((line 1) (column 12))) (stop ((line 1) (column 13)))))
     (id z) (ctx Load))))

Test dict expression
  $ echo '{}' | parse expression -
  (Dict (location ((start ((line 1) (column 0))) (stop ((line 1) (column 2)))))
   (keys ()) (values ()))
  $ echo '{ 1:2 }' | parse expression -
  (Dict (location ((start ((line 1) (column 0))) (stop ((line 1) (column 7)))))
   (keys
    (((Constant
       (location ((start ((line 1) (column 2))) (stop ((line 1) (column 3)))))
       (value (Integer 1)) (kind ())))))
   (values
    ((Constant
      (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
      (value (Integer 2)) (kind ())))))
  $ echo '{ 1:2, 3:4 }' | parse expression -
  (Dict
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 12)))))
   (keys
    (((Constant
       (location ((start ((line 1) (column 2))) (stop ((line 1) (column 3)))))
       (value (Integer 1)) (kind ())))
     ((Constant
       (location ((start ((line 1) (column 7))) (stop ((line 1) (column 8)))))
       (value (Integer 3)) (kind ())))))
   (values
    ((Constant
      (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
      (value (Integer 2)) (kind ()))
     (Constant
      (location ((start ((line 1) (column 9))) (stop ((line 1) (column 10)))))
      (value (Integer 4)) (kind ())))))
  $ echo '{ 1:2, **x }' | parse expression -
  (Dict
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 12)))))
   (keys
    (((Constant
       (location ((start ((line 1) (column 2))) (stop ((line 1) (column 3)))))
       (value (Integer 1)) (kind ())))
     ()))
   (values
    ((Constant
      (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
      (value (Integer 2)) (kind ()))
     (Name
      (location ((start ((line 1) (column 9))) (stop ((line 1) (column 10)))))
      (id x) (ctx Load)))))

Test set expression
  $ echo '{ 1 }' | parse expression -
  (Set (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (elts
    ((Constant
      (location ((start ((line 1) (column 2))) (stop ((line 1) (column 3)))))
      (value (Integer 1)) (kind ())))))
  $ echo '{ 1, 2 }' | parse expression -
  (Set (location ((start ((line 1) (column 0))) (stop ((line 1) (column 8)))))
   (elts
    ((Constant
      (location ((start ((line 1) (column 2))) (stop ((line 1) (column 3)))))
      (value (Integer 1)) (kind ()))
     (Constant
      (location ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
      (value (Integer 2)) (kind ())))))

Test list comprehension expression
  $ echo '[x for x in y]' | parse expression -
  (ListComp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 14)))))
   (elt
    (Name
     (location ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
     (id x) (ctx Load)))
   (generators
    (((target
       (Name
        (location ((start ((line 1) (column 7))) (stop ((line 1) (column 8)))))
        (id x) (ctx Store)))
      (iter
       (Name
        (location
         ((start ((line 1) (column 12))) (stop ((line 1) (column 13)))))
        (id y) (ctx Load)))
      (ifs ()) (is_async false)))))
  $ echo '[x+y for x in a for y in b]' | parse expression -
  (ListComp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 27)))))
   (elt
    (BinOp
     (location ((start ((line 1) (column 1))) (stop ((line 1) (column 4)))))
     (left
      (Name
       (location ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
       (id x) (ctx Load)))
     (op Add)
     (right
      (Name
       (location ((start ((line 1) (column 3))) (stop ((line 1) (column 4)))))
       (id y) (ctx Load)))))
   (generators
    (((target
       (Name
        (location
         ((start ((line 1) (column 9))) (stop ((line 1) (column 10)))))
        (id x) (ctx Store)))
      (iter
       (Name
        (location
         ((start ((line 1) (column 14))) (stop ((line 1) (column 15)))))
        (id a) (ctx Load)))
      (ifs ()) (is_async false))
     ((target
       (Name
        (location
         ((start ((line 1) (column 20))) (stop ((line 1) (column 21)))))
        (id y) (ctx Store)))
      (iter
       (Name
        (location
         ((start ((line 1) (column 25))) (stop ((line 1) (column 26)))))
        (id b) (ctx Load)))
      (ifs ()) (is_async false)))))
  $ echo '[x async for x in y]' | parse expression -
  (ListComp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 20)))))
   (elt
    (Name
     (location ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
     (id x) (ctx Load)))
   (generators
    (((target
       (Name
        (location
         ((start ((line 1) (column 13))) (stop ((line 1) (column 14)))))
        (id x) (ctx Store)))
      (iter
       (Name
        (location
         ((start ((line 1) (column 18))) (stop ((line 1) (column 19)))))
        (id y) (ctx Load)))
      (ifs ()) (is_async true)))))
  $ echo '[x for x in y if x < 0]' | parse expression -
  (ListComp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 23)))))
   (elt
    (Name
     (location ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
     (id x) (ctx Load)))
   (generators
    (((target
       (Name
        (location ((start ((line 1) (column 7))) (stop ((line 1) (column 8)))))
        (id x) (ctx Store)))
      (iter
       (Name
        (location
         ((start ((line 1) (column 12))) (stop ((line 1) (column 13)))))
        (id y) (ctx Load)))
      (ifs
       ((Compare
         (location
          ((start ((line 1) (column 17))) (stop ((line 1) (column 22)))))
         (left
          (Name
           (location
            ((start ((line 1) (column 17))) (stop ((line 1) (column 18)))))
           (id x) (ctx Load)))
         (ops (Lt))
         (comparators
          ((Constant
            (location
             ((start ((line 1) (column 21))) (stop ((line 1) (column 22)))))
            (value (Integer 0)) (kind ())))))))
      (is_async false)))))
  $ echo '[x for x in y+z if x > 0 if x < 42]' | parse expression -
  (ListComp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 35)))))
   (elt
    (Name
     (location ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
     (id x) (ctx Load)))
   (generators
    (((target
       (Name
        (location ((start ((line 1) (column 7))) (stop ((line 1) (column 8)))))
        (id x) (ctx Store)))
      (iter
       (BinOp
        (location
         ((start ((line 1) (column 12))) (stop ((line 1) (column 15)))))
        (left
         (Name
          (location
           ((start ((line 1) (column 12))) (stop ((line 1) (column 13)))))
          (id y) (ctx Load)))
        (op Add)
        (right
         (Name
          (location
           ((start ((line 1) (column 14))) (stop ((line 1) (column 15)))))
          (id z) (ctx Load)))))
      (ifs
       ((Compare
         (location
          ((start ((line 1) (column 19))) (stop ((line 1) (column 24)))))
         (left
          (Name
           (location
            ((start ((line 1) (column 19))) (stop ((line 1) (column 20)))))
           (id x) (ctx Load)))
         (ops (Gt))
         (comparators
          ((Constant
            (location
             ((start ((line 1) (column 23))) (stop ((line 1) (column 24)))))
            (value (Integer 0)) (kind ())))))
        (Compare
         (location
          ((start ((line 1) (column 28))) (stop ((line 1) (column 34)))))
         (left
          (Name
           (location
            ((start ((line 1) (column 28))) (stop ((line 1) (column 29)))))
           (id x) (ctx Load)))
         (ops (Lt))
         (comparators
          ((Constant
            (location
             ((start ((line 1) (column 32))) (stop ((line 1) (column 34)))))
            (value (Integer 42)) (kind ())))))))
      (is_async false)))))
  $ echo '[x+y for x, y in z if x > 0 if y < 0]' | parse expression -
  (ListComp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 37)))))
   (elt
    (BinOp
     (location ((start ((line 1) (column 1))) (stop ((line 1) (column 4)))))
     (left
      (Name
       (location ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
       (id x) (ctx Load)))
     (op Add)
     (right
      (Name
       (location ((start ((line 1) (column 3))) (stop ((line 1) (column 4)))))
       (id y) (ctx Load)))))
   (generators
    (((target
       (Tuple
        (location
         ((start ((line 1) (column 9))) (stop ((line 1) (column 13)))))
        (elts
         ((Name
           (location
            ((start ((line 1) (column 9))) (stop ((line 1) (column 10)))))
           (id x) (ctx Store))
          (Name
           (location
            ((start ((line 1) (column 12))) (stop ((line 1) (column 13)))))
           (id y) (ctx Store))))
        (ctx Store)))
      (iter
       (Name
        (location
         ((start ((line 1) (column 17))) (stop ((line 1) (column 18)))))
        (id z) (ctx Load)))
      (ifs
       ((Compare
         (location
          ((start ((line 1) (column 22))) (stop ((line 1) (column 27)))))
         (left
          (Name
           (location
            ((start ((line 1) (column 22))) (stop ((line 1) (column 23)))))
           (id x) (ctx Load)))
         (ops (Gt))
         (comparators
          ((Constant
            (location
             ((start ((line 1) (column 26))) (stop ((line 1) (column 27)))))
            (value (Integer 0)) (kind ())))))
        (Compare
         (location
          ((start ((line 1) (column 31))) (stop ((line 1) (column 36)))))
         (left
          (Name
           (location
            ((start ((line 1) (column 31))) (stop ((line 1) (column 32)))))
           (id y) (ctx Load)))
         (ops (Lt))
         (comparators
          ((Constant
            (location
             ((start ((line 1) (column 35))) (stop ((line 1) (column 36)))))
            (value (Integer 0)) (kind ())))))))
      (is_async false)))))

Test set comprehension expression
  $ echo '{x for x in y}' | parse expression -
  (SetComp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 14)))))
   (elt
    (Name
     (location ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
     (id x) (ctx Load)))
   (generators
    (((target
       (Name
        (location ((start ((line 1) (column 7))) (stop ((line 1) (column 8)))))
        (id x) (ctx Store)))
      (iter
       (Name
        (location
         ((start ((line 1) (column 12))) (stop ((line 1) (column 13)))))
        (id y) (ctx Load)))
      (ifs ()) (is_async false)))))

Test dict comprehension expression
  $ echo '{x:y for x, y in z}' | parse expression -
  (DictComp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 19)))))
   (key
    (Name
     (location ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
     (id x) (ctx Load)))
   (value
    (Name
     (location ((start ((line 1) (column 3))) (stop ((line 1) (column 4)))))
     (id y) (ctx Load)))
   (generators
    (((target
       (Tuple
        (location
         ((start ((line 1) (column 9))) (stop ((line 1) (column 13)))))
        (elts
         ((Name
           (location
            ((start ((line 1) (column 9))) (stop ((line 1) (column 10)))))
           (id x) (ctx Store))
          (Name
           (location
            ((start ((line 1) (column 12))) (stop ((line 1) (column 13)))))
           (id y) (ctx Store))))
        (ctx Store)))
      (iter
       (Name
        (location
         ((start ((line 1) (column 17))) (stop ((line 1) (column 18)))))
        (id z) (ctx Load)))
      (ifs ()) (is_async false)))))

Test generator expression
  $ echo '(x for x, y in z)' | parse expression -
  (GeneratorExp
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 17)))))
   (elt
    (Name
     (location ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
     (id x) (ctx Load)))
   (generators
    (((target
       (Tuple
        (location
         ((start ((line 1) (column 7))) (stop ((line 1) (column 11)))))
        (elts
         ((Name
           (location
            ((start ((line 1) (column 7))) (stop ((line 1) (column 8)))))
           (id x) (ctx Store))
          (Name
           (location
            ((start ((line 1) (column 10))) (stop ((line 1) (column 11)))))
           (id y) (ctx Store))))
        (ctx Store)))
      (iter
       (Name
        (location
         ((start ((line 1) (column 15))) (stop ((line 1) (column 16)))))
        (id z) (ctx Load)))
      (ifs ()) (is_async false)))))

Test await expression
  $ echo 'await x' | parse expression -
  (Await
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 7)))))
   (value
    (Name
     (location ((start ((line 1) (column 6))) (stop ((line 1) (column 7)))))
     (id x) (ctx Load))))

Test yield expression
  $ echo '(yield)' | parse expression -
  (Yield
   (location ((start ((line 1) (column 1))) (stop ((line 1) (column 6)))))
   (value ()))
  $ echo '(yield x)' | parse expression -
  (Yield
   (location ((start ((line 1) (column 1))) (stop ((line 1) (column 8)))))
   (value
    ((Name
      (location ((start ((line 1) (column 7))) (stop ((line 1) (column 8)))))
      (id x) (ctx Load)))))

Test yield from expression
  $ echo '(yield from x)' | parse expression -
  (YieldFrom
   (location ((start ((line 1) (column 1))) (stop ((line 1) (column 13)))))
   (value
    (Name
     (location ((start ((line 1) (column 12))) (stop ((line 1) (column 13)))))
     (id x) (ctx Load))))

Test compare expression
  $ echo 'x < y' | parse expression -
  (Compare
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (ops (Lt))
   (comparators
    ((Name
      (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
      (id y) (ctx Load)))))
  $ echo 'x < y < z' | parse expression -
  (Compare
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 9)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (ops (Lt Lt))
   (comparators
    ((Name
      (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
      (id y) (ctx Load))
     (Name
      (location ((start ((line 1) (column 8))) (stop ((line 1) (column 9)))))
      (id z) (ctx Load)))))
  $ echo 'x < (y < z)' | parse expression -
  (Compare
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 11)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (ops (Lt))
   (comparators
    ((Compare
      (location ((start ((line 1) (column 5))) (stop ((line 1) (column 10)))))
      (left
       (Name
        (location ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
        (id y) (ctx Load)))
      (ops (Lt))
      (comparators
       ((Name
         (location
          ((start ((line 1) (column 9))) (stop ((line 1) (column 10)))))
         (id z) (ctx Load))))))))
  $ echo 'x > y' | parse expression -
  (Compare
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (ops (Gt))
   (comparators
    ((Name
      (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
      (id y) (ctx Load)))))
  $ echo 'x > y < z' | parse expression -
  (Compare
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 9)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (ops (Gt Lt))
   (comparators
    ((Name
      (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
      (id y) (ctx Load))
     (Name
      (location ((start ((line 1) (column 8))) (stop ((line 1) (column 9)))))
      (id z) (ctx Load)))))
  $ echo 'x < y > z' | parse expression -
  (Compare
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 9)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (ops (Lt Gt))
   (comparators
    ((Name
      (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
      (id y) (ctx Load))
     (Name
      (location ((start ((line 1) (column 8))) (stop ((line 1) (column 9)))))
      (id z) (ctx Load)))))
  $ echo 'x <= y' | parse expression -
  (Compare
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (ops (Lte))
   (comparators
    ((Name
      (location ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
      (id y) (ctx Load)))))
  $ echo 'x >= y' | parse expression -
  (Compare
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (ops (Gte))
   (comparators
    ((Name
      (location ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
      (id y) (ctx Load)))))
  $ echo 'x == y' | parse expression -
  (Compare
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (ops (Eq))
   (comparators
    ((Name
      (location ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
      (id y) (ctx Load)))))
  $ echo 'x != y' | parse expression -
  (Compare
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (ops (NotEq))
   (comparators
    ((Name
      (location ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
      (id y) (ctx Load)))))
  $ echo 'x is y' | parse expression -
  (Compare
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (ops (Is))
   (comparators
    ((Name
      (location ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
      (id y) (ctx Load)))))
  $ echo 'x is not y' | parse expression -
  (Compare
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 10)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (ops (IsNot))
   (comparators
    ((Name
      (location ((start ((line 1) (column 9))) (stop ((line 1) (column 10)))))
      (id y) (ctx Load)))))
  $ echo 'x in y' | parse expression -
  (Compare
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (ops (In))
   (comparators
    ((Name
      (location ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
      (id y) (ctx Load)))))
  $ echo 'x not in y' | parse expression -
  (Compare
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 10)))))
   (left
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id x) (ctx Load)))
   (ops (NotIn))
   (comparators
    ((Name
      (location ((start ((line 1) (column 9))) (stop ((line 1) (column 10)))))
      (id y) (ctx Load)))))

Test call expression
  $ echo 'f()' | parse expression -
  (Call (location ((start ((line 1) (column 0))) (stop ((line 1) (column 3)))))
   (func
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id f) (ctx Load)))
   (args ()) (keywords ()))
  $ echo 'f(x)' | parse expression -
  (Call (location ((start ((line 1) (column 0))) (stop ((line 1) (column 4)))))
   (func
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id f) (ctx Load)))
   (args
    ((Name
      (location ((start ((line 1) (column 2))) (stop ((line 1) (column 3)))))
      (id x) (ctx Load))))
   (keywords ()))
  $ echo 'f(x, y)' | parse expression -
  (Call (location ((start ((line 1) (column 0))) (stop ((line 1) (column 7)))))
   (func
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id f) (ctx Load)))
   (args
    ((Name
      (location ((start ((line 1) (column 2))) (stop ((line 1) (column 3)))))
      (id x) (ctx Load))
     (Name
      (location ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
      (id y) (ctx Load))))
   (keywords ()))
  $ echo 'f(x, *y, **z)' | parse expression -
  (Call
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 13)))))
   (func
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id f) (ctx Load)))
   (args
    ((Name
      (location ((start ((line 1) (column 2))) (stop ((line 1) (column 3)))))
      (id x) (ctx Load))
     (Starred
      (location ((start ((line 1) (column 5))) (stop ((line 1) (column 7)))))
      (value
       (Name
        (location ((start ((line 1) (column 6))) (stop ((line 1) (column 7)))))
        (id y) (ctx Load)))
      (ctx Load))))
   (keywords
    (((location ((start ((line 1) (column 9))) (stop ((line 1) (column 12)))))
      (arg ())
      (value
       (Name
        (location
         ((start ((line 1) (column 11))) (stop ((line 1) (column 12)))))
        (id z) (ctx Load)))))))
  $ echo 'f(*(x+y), **z, **w)' | parse expression -
  (Call
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 19)))))
   (func
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id f) (ctx Load)))
   (args
    ((Starred
      (location ((start ((line 1) (column 2))) (stop ((line 1) (column 8)))))
      (value
       (BinOp
        (location ((start ((line 1) (column 4))) (stop ((line 1) (column 7)))))
        (left
         (Name
          (location
           ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
          (id x) (ctx Load)))
        (op Add)
        (right
         (Name
          (location
           ((start ((line 1) (column 6))) (stop ((line 1) (column 7)))))
          (id y) (ctx Load)))))
      (ctx Load))))
   (keywords
    (((location ((start ((line 1) (column 10))) (stop ((line 1) (column 13)))))
      (arg ())
      (value
       (Name
        (location
         ((start ((line 1) (column 12))) (stop ((line 1) (column 13)))))
        (id z) (ctx Load))))
     ((location ((start ((line 1) (column 15))) (stop ((line 1) (column 18)))))
      (arg ())
      (value
       (Name
        (location
         ((start ((line 1) (column 17))) (stop ((line 1) (column 18)))))
        (id w) (ctx Load)))))))
  $ echo 'f(x=1)' | parse expression -
  (Call (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (func
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id f) (ctx Load)))
   (args ())
   (keywords
    (((location ((start ((line 1) (column 2))) (stop ((line 1) (column 5)))))
      (arg (x))
      (value
       (Constant
        (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
        (value (Integer 1)) (kind ())))))))
  $ echo 'f(x=1, y=2)' | parse expression -
  (Call
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 11)))))
   (func
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id f) (ctx Load)))
   (args ())
   (keywords
    (((location ((start ((line 1) (column 2))) (stop ((line 1) (column 5)))))
      (arg (x))
      (value
       (Constant
        (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
        (value (Integer 1)) (kind ()))))
     ((location ((start ((line 1) (column 7))) (stop ((line 1) (column 10)))))
      (arg (y))
      (value
       (Constant
        (location
         ((start ((line 1) (column 9))) (stop ((line 1) (column 10)))))
        (value (Integer 2)) (kind ())))))))
  $ echo 'f(x, y=2)' | parse expression -
  (Call (location ((start ((line 1) (column 0))) (stop ((line 1) (column 9)))))
   (func
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id f) (ctx Load)))
   (args
    ((Name
      (location ((start ((line 1) (column 2))) (stop ((line 1) (column 3)))))
      (id x) (ctx Load))))
   (keywords
    (((location ((start ((line 1) (column 5))) (stop ((line 1) (column 8)))))
      (arg (y))
      (value
       (Constant
        (location ((start ((line 1) (column 7))) (stop ((line 1) (column 8)))))
        (value (Integer 2)) (kind ())))))))

Test fstrings
  $ echo 'f"derp"' | parse expression -
  (JoinedStr
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 7)))))
   (values
    ((Constant
      (location ((start ((line 1) (column 2))) (stop ((line 1) (column 6)))))
      (value (String derp)) (kind ())))))
  $ echo 'f"{x}"' | parse expression -
  (JoinedStr
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (values
    ((FormattedValue
      (location ((start ((line 1) (column 2))) (stop ((line 1) (column 5)))))
      (value
       (Name
        (location ((start ((line 1) (column 3))) (stop ((line 1) (column 4)))))
        (id x) (ctx Load)))
      (conversion -1) (format_spec ())))))
  $ echo 'f"derp={x}"' | parse expression -
  (JoinedStr
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 11)))))
   (values
    ((Constant
      (location ((start ((line 1) (column 2))) (stop ((line 1) (column 7)))))
      (value (String derp=)) (kind ()))
     (FormattedValue
      (location ((start ((line 1) (column 7))) (stop ((line 1) (column 10)))))
      (value
       (Name
        (location ((start ((line 1) (column 8))) (stop ((line 1) (column 9)))))
        (id x) (ctx Load)))
      (conversion -1) (format_spec ())))))
  $ echo 'f"{x!s}"' | parse expression -
  (JoinedStr
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 8)))))
   (values
    ((FormattedValue
      (location ((start ((line 1) (column 2))) (stop ((line 1) (column 7)))))
      (value
       (Name
        (location ((start ((line 1) (column 3))) (stop ((line 1) (column 4)))))
        (id x) (ctx Load)))
      (conversion 115) (format_spec ())))))
  $ echo 'f"{x!r}"' | parse expression -
  (JoinedStr
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 8)))))
   (values
    ((FormattedValue
      (location ((start ((line 1) (column 2))) (stop ((line 1) (column 7)))))
      (value
       (Name
        (location ((start ((line 1) (column 3))) (stop ((line 1) (column 4)))))
        (id x) (ctx Load)))
      (conversion 114) (format_spec ())))))
  $ echo 'f"{x!a}"' | parse expression -
  (JoinedStr
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 8)))))
   (values
    ((FormattedValue
      (location ((start ((line 1) (column 2))) (stop ((line 1) (column 7)))))
      (value
       (Name
        (location ((start ((line 1) (column 3))) (stop ((line 1) (column 4)))))
        (id x) (ctx Load)))
      (conversion 97) (format_spec ())))))
  $ echo 'f"{x:1.2}"' | parse expression -
  (JoinedStr
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 10)))))
   (values
    ((FormattedValue
      (location ((start ((line 1) (column 2))) (stop ((line 1) (column 9)))))
      (value
       (Name
        (location ((start ((line 1) (column 3))) (stop ((line 1) (column 4)))))
        (id x) (ctx Load)))
      (conversion -1)
      (format_spec
       ((JoinedStr
         (location
          ((start ((line 1) (column 4))) (stop ((line 1) (column 8)))))
         (values
          ((Constant
            (location
             ((start ((line 1) (column 5))) (stop ((line 1) (column 8)))))
            (value (String 1.2)) (kind ())))))))))))
  $ echo 'f"{x:{w+1}.{p}}"' | parse expression -
  (JoinedStr
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 16)))))
   (values
    ((FormattedValue
      (location ((start ((line 1) (column 2))) (stop ((line 1) (column 15)))))
      (value
       (Name
        (location ((start ((line 1) (column 3))) (stop ((line 1) (column 4)))))
        (id x) (ctx Load)))
      (conversion -1)
      (format_spec
       ((JoinedStr
         (location
          ((start ((line 1) (column 4))) (stop ((line 1) (column 14)))))
         (values
          ((FormattedValue
            (location
             ((start ((line 1) (column 5))) (stop ((line 1) (column 10)))))
            (value
             (BinOp
              (location
               ((start ((line 1) (column 6))) (stop ((line 1) (column 9)))))
              (left
               (Name
                (location
                 ((start ((line 1) (column 6))) (stop ((line 1) (column 7)))))
                (id w) (ctx Load)))
              (op Add)
              (right
               (Constant
                (location
                 ((start ((line 1) (column 8))) (stop ((line 1) (column 9)))))
                (value (Integer 1)) (kind ())))))
            (conversion -1) (format_spec ()))
           (Constant
            (location
             ((start ((line 1) (column 10))) (stop ((line 1) (column 11)))))
            (value (String .)) (kind ()))
           (FormattedValue
            (location
             ((start ((line 1) (column 11))) (stop ((line 1) (column 14)))))
            (value
             (Name
              (location
               ((start ((line 1) (column 12))) (stop ((line 1) (column 13)))))
              (id p) (ctx Load)))
            (conversion -1) (format_spec ()))
           (Constant
            (location
             ((start ((line 1) (column 14))) (stop ((line 1) (column 14)))))
            (value (String "")) (kind ())))))))))))
  $ echo 'f"{x}"f"{y}"' | parse expression -
  (JoinedStr
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 12)))))
   (values
    ((FormattedValue
      (location ((start ((line 1) (column 2))) (stop ((line 1) (column 5)))))
      (value
       (Name
        (location ((start ((line 1) (column 3))) (stop ((line 1) (column 4)))))
        (id x) (ctx Load)))
      (conversion -1) (format_spec ()))
     (FormattedValue
      (location ((start ((line 1) (column 8))) (stop ((line 1) (column 11)))))
      (value
       (Name
        (location
         ((start ((line 1) (column 9))) (stop ((line 1) (column 10)))))
        (id y) (ctx Load)))
      (conversion -1) (format_spec ())))))

Test attribute expression
  $ echo 'a.b.c' | parse expression -
  (Attribute
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (value
    (Attribute
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 3)))))
     (value
      (Name
       (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
       (id a) (ctx Load)))
     (attr b) (ctx Load)))
   (attr c) (ctx Load))

Test subscript expression
  $ echo 'foo[42]' | parse expression -
  (Subscript
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 7)))))
   (value
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 3)))))
     (id foo) (ctx Load)))
   (slice
    (Constant
     (location ((start ((line 1) (column 4))) (stop ((line 1) (column 6)))))
     (value (Integer 42)) (kind ())))
   (ctx Load))

Test starred expression
  $ echo 'foo(*x)' | parse expression -
  (Call (location ((start ((line 1) (column 0))) (stop ((line 1) (column 7)))))
   (func
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 3)))))
     (id foo) (ctx Load)))
   (args
    ((Starred
      (location ((start ((line 1) (column 4))) (stop ((line 1) (column 6)))))
      (value
       (Name
        (location ((start ((line 1) (column 5))) (stop ((line 1) (column 6)))))
        (id x) (ctx Load)))
      (ctx Load))))
   (keywords ()))

Test list expression
  $ echo '[]' | parse expression -
  (List (location ((start ((line 1) (column 0))) (stop ((line 1) (column 2)))))
   (elts ()) (ctx Load))
  $ echo '[x]' | parse expression -
  (List (location ((start ((line 1) (column 0))) (stop ((line 1) (column 3)))))
   (elts
    ((Name
      (location ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
      (id x) (ctx Load))))
   (ctx Load))
  $ echo '[x, y]' | parse expression -
  (List (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (elts
    ((Name
      (location ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
      (id x) (ctx Load))
     (Name
      (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
      (id y) (ctx Load))))
   (ctx Load))

Test tuple expression
  $ echo '()' | parse expression -
  (Tuple
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 2)))))
   (elts ()) (ctx Load))
  $ echo '(x, y)' | parse expression -
  (Tuple
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (elts
    ((Name
      (location ((start ((line 1) (column 1))) (stop ((line 1) (column 2)))))
      (id x) (ctx Load))
     (Name
      (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
      (id y) (ctx Load))))
   (ctx Load))

Test slide expression
  $ echo 'a[:]' | parse expression -
  (Subscript
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 4)))))
   (value
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id a) (ctx Load)))
   (slice
    (Slice
     (location ((start ((line 1) (column 2))) (stop ((line 1) (column 3)))))
     (lower ()) (upper ()) (step ())))
   (ctx Load))
  $ echo 'a[::]' | parse expression -
  (Subscript
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (value
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id a) (ctx Load)))
   (slice
    (Slice
     (location ((start ((line 1) (column 2))) (stop ((line 1) (column 4)))))
     (lower ()) (upper ()) (step ())))
   (ctx Load))
  $ echo 'a[1:]' | parse expression -
  (Subscript
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (value
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id a) (ctx Load)))
   (slice
    (Slice
     (location ((start ((line 1) (column 2))) (stop ((line 1) (column 4)))))
     (lower
      ((Constant
        (location ((start ((line 1) (column 2))) (stop ((line 1) (column 3)))))
        (value (Integer 1)) (kind ()))))
     (upper ()) (step ())))
   (ctx Load))
  $ echo 'a[:2]' | parse expression -
  (Subscript
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 5)))))
   (value
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id a) (ctx Load)))
   (slice
    (Slice
     (location ((start ((line 1) (column 2))) (stop ((line 1) (column 4)))))
     (lower ())
     (upper
      ((Constant
        (location ((start ((line 1) (column 3))) (stop ((line 1) (column 4)))))
        (value (Integer 2)) (kind ()))))
     (step ())))
   (ctx Load))
  $ echo 'a[::3]' | parse expression -
  (Subscript
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 6)))))
   (value
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id a) (ctx Load)))
   (slice
    (Slice
     (location ((start ((line 1) (column 2))) (stop ((line 1) (column 5)))))
     (lower ()) (upper ())
     (step
      ((Constant
        (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
        (value (Integer 3)) (kind ()))))))
   (ctx Load))
  $ echo 'a[1:2:3]' | parse expression -
  (Subscript
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 8)))))
   (value
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id a) (ctx Load)))
   (slice
    (Slice
     (location ((start ((line 1) (column 2))) (stop ((line 1) (column 7)))))
     (lower
      ((Constant
        (location ((start ((line 1) (column 2))) (stop ((line 1) (column 3)))))
        (value (Integer 1)) (kind ()))))
     (upper
      ((Constant
        (location ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
        (value (Integer 2)) (kind ()))))
     (step
      ((Constant
        (location ((start ((line 1) (column 6))) (stop ((line 1) (column 7)))))
        (value (Integer 3)) (kind ()))))))
   (ctx Load))
  $ echo 'a[1:2, 3:4]' | parse expression -
  (Subscript
   (location ((start ((line 1) (column 0))) (stop ((line 1) (column 11)))))
   (value
    (Name
     (location ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
     (id a) (ctx Load)))
   (slice
    (Tuple
     (location ((start ((line 1) (column 2))) (stop ((line 1) (column 10)))))
     (elts
      ((Slice
        (location ((start ((line 1) (column 2))) (stop ((line 1) (column 5)))))
        (lower
         ((Constant
           (location
            ((start ((line 1) (column 2))) (stop ((line 1) (column 3)))))
           (value (Integer 1)) (kind ()))))
        (upper
         ((Constant
           (location
            ((start ((line 1) (column 4))) (stop ((line 1) (column 5)))))
           (value (Integer 2)) (kind ()))))
        (step ()))
       (Slice
        (location
         ((start ((line 1) (column 7))) (stop ((line 1) (column 10)))))
        (lower
         ((Constant
           (location
            ((start ((line 1) (column 7))) (stop ((line 1) (column 8)))))
           (value (Integer 3)) (kind ()))))
        (upper
         ((Constant
           (location
            ((start ((line 1) (column 9))) (stop ((line 1) (column 10)))))
           (value (Integer 4)) (kind ()))))
        (step ()))))
     (ctx Load)))
   (ctx Load))
