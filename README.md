Prettyprint `Show` output
=========================

Nicely formats output from auto-derived `Show` instances. It does not rely on an
existing Haskell parser, and reacts on not much more than different parentheses,
making it fairly robust.


## Examples

### Artificial

```
Hello Foo ("(Bar", Haha) (Baz (A { foo = C, bar = D, qux = (E,"He)llo World!",G,
    H,[A,B,c,d,e,Fghi]) } ) (B,C) [Baz A1 B2, (Baz A3 (B4)), (Baz A5 (B6)), (Baz
    (A7) B8)]) (Foo) (Bar) (Baz (A) (B))

==>

Hello Foo ("(Bar",Haha)
          (Baz (A {foo = C
                  ,bar = D
                  ,qux = (E
                         ,"He)llo World!"
                         ,G
                         ,H
                         ,[A,B,c,d,e,Fghi])})
               (B,C)
               [Baz A1 B2
               ,(Baz A3 (B4))
               ,(Baz A5 (B6))
               ,(Baz (A7) B8)])
          (Foo)
          (Bar)
          (Baz (A) (B))
```

### Inspired by a real AST

```
Set  (fromList [(Name "A string with (parenthesis",Ann  (Entry (Quality 1 1)
    (Ann  False) (Ann  (Map [Ann  (Bound (Ann  (Id "lorem"))),Ann  (Variable
    (Ann  (Id "ipsum")))])))),(Name "string",Ann  (Entry (Quality 1 1) (Ann
    True) (Ann  (Internal (Ann  (Reduce (Ann  (Id "dolor")) (Ann  (Id "sit")))))
    ))),(Name "Another } here",Ann  (Entry (Quality 1 1) (Ann  (Or [Ann  (Not
    (Ann  (Is (Ann  Flagged) (Ann  Type) (Ann  (Multi [Ann  (Literal (Ann  One))
    ]))))),Ann  (Is (Ann  Flagged) (Ann  Type) (Ann  (Multi [Ann  (Literal (Ann
    Three))]))),Ann  (Is (Ann  Flagged) (Ann  Type) (Ann  (Multi [Ann  (Literal
    (Ann  Two))])))])) (Ann  (Internal (Ann  (Concat (Ann  (Id "amet"))))))))])

==>

Set (fromList [(Name "A string with (parenthesis"
               ,Ann (Entry (Quality 1 1)
                           (Ann False)
                           (Ann (Map [Ann (Bound (Ann (Id "lorem")))
                                     ,Ann (Variable (Ann (Id "ipsum")))]))))
              ,(Name "string"
               ,Ann (Entry (Quality 1 1)
                           (Ann True)
                           (Ann (Internal (Ann (Reduce (Ann (Id "dolor"))
                                                       (Ann (Id "sit"))))))))
              ,(Name "Another } here"
               ,Ann (Entry (Quality 1 1)
                           (Ann (Or [Ann (Not (Ann (Is (Ann Flagged)
                                                       (Ann Type)
                                                       (Ann (Multi [Ann (Literal (Ann One))])))))
                                    ,Ann (Is (Ann Flagged)
                                             (Ann Type)
                                             (Ann (Multi [Ann (Literal (Ann Three))])))
                                    ,Ann (Is (Ann Flagged)
                                             (Ann Type)
                                             (Ann (Multi [Ann (Literal (Ann Two))])))]))
                           (Ann (Internal (Ann (Concat (Ann (Id "amet"))))))))])
```
