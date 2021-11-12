Test parse error
  $ echo "def foo(" | parse module -  # The funny end column 0 came from CPython
  Parse error at line 1, column 8 to line 1, column 0: '(' was never closed
  $ echo "del 1+2" | parse module -
  Parse error at line 1, column 5 to line 1, column 8: cannot delete expression

Non-ascii characters in string literals is ok
  $ echo 'x = "😝"' | parse module -
  ((body
    ((Assign
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 10)))))
      (targets
       ((Name
         (location
          ((start ((line 1) (column 0))) (stop ((line 1) (column 1)))))
         (id x) (ctx Store))))
      (value
       (Constant
        (location
         ((start ((line 1) (column 4))) (stop ((line 1) (column 10)))))
        (value (String "\240\159\152\157")) (kind ())))
      (type_comment ()))))
   (type_ignores ()))

Non-ascii characters in variable name is not ok
  $ echo 'Δx' | parse module -
  Parse error at line 1, column 1 to line 1, column 1: CPython runtime raised a non-syntax exception

Unicode surrogate should not crash the parser
  $ echo '"\ud83d"' | parse module -
  ((body
    ((Expr
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 8)))))
      (value
       (Constant
        (location ((start ((line 1) (column 0))) (stop ((line 1) (column 8)))))
        (value (String "\\ud83d")) (kind ()))))))
   (type_ignores ()))

Unicode escape sequence "\N" is not supported
  $ echo '"\N{HORIZONTAL ELLIPSIS}"' | parse module -
  ((body
    ((Expr
      (location ((start ((line 1) (column 0))) (stop ((line 1) (column 25)))))
      (value
       (Constant
        (location
         ((start ((line 1) (column 0))) (stop ((line 1) (column 25)))))
        (value (String "\\N{HORIZONTAL ELLIPSIS}")) (kind ()))))))
   (type_ignores ()))
