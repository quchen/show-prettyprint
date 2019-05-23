[![](https://travis-ci.org/quchen/show-prettyprint.svg?branch=master)](https://travis-ci.org/quchen/show-prettyprint)

Prettyprint `Show` output
=========================

Output of nested data structures by `Show` instances is often very hard to read.
This package offers a simple function to insert line breaks and indentation into
that ouput so that the semantics are unchanged, but makes it *much* easier to
read.

The package does not rely on a parser for actual Haskell; instead, it merely
reacts on parentheses, commas and the like. This makes it fairly robust even in
the face of invalid `Show` instances, that may not produce valid Haskell code.



## Examples

### Artificial

```haskell
Hello Foo ("(Bar", Haha) (Baz (A { foo = C, bar = D, qux = (E,"He)llo World!",G,
    H,[A,B,c,d,e,Fghi]) } ) (B,C) [Baz A1 B2, (Baz A3 (B4)), (Baz A5 (B6)), (Baz
    (A7) B8)]) (Foo) (Bar) (Baz (A) (B))

==>


Hello Foo ("(Bar",Haha)
          (Baz (A {foo = C
                  ,bar = D
                  ,qux = (E,"He)llo World!",G,H,[A,B,c,d,e,Fghi])})
               (B,C)
               [Baz A1 B2,(Baz A3 (B4)),(Baz A5 (B6)),(Baz (A7) B8)])
          (Foo)
          (Bar)
          (Baz (A) (B))
```

### Inspired by a real AST

```haskell
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
