Prettyprint `Show` output
=========================

Output of nested data structures by `Show` instances is often very hard to read.
This package offers a simple function to insert line breaks and indentation into
that output so that the semantics are unchanged, but makes it *much* easier to
read.

The package does not rely on a parser for actual Haskell; instead, it merely
reacts on parentheses, commas and the like. This makes it fairly robust even in
the face of invalid `Show` instances, that may not produce valid Haskell code.

For example, consider this nested data structure:

```haskell
nestedExample = fromList
    [ ("hello", Left  (Pair True ()))
    , ("world", Right (Record { r1 = ('c', -1.2e34), r2 = 123 }))
    , ("!"    , Left  (Pair False ())) ]
```

Applying show to it results in the fairly dense representation

```haskell
fromList [("!",Left (Pair False ())),("hello",Left (Pair True ())),("world",Right (Record {r1 = ('c',-1.2e34), r2 = 123}))]
```

With the functions defined in this module, we can make this output a bit more
readable,

```haskell
fromList [("!"
          ,Left (Pair False ()))
         ,("hello",Left (Pair True ()))
         ,("world"
          ,Right (Record {r1 = ('c'
                               ,-1.2e34)
                         ,r2 = 123}))]
```
