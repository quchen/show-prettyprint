# 0.2.3

Fix escaping of backslashes (#5)

# 0.2.{1,2}

Add functions to prettify to `Doc` instead of just supporting `String`,

```haskell
prettifyToDoc :: String -> Doc ann
prettyShowDoc :: Show a => a -> Doc ann
```

# 0.2.0.1

Tagged the wrong version as 0.2 on Github. Releasing a new version with an
updated tag to remedy this.

# 0.2

Prettyprint based on the `prettyprinter` library, instead of `ansi-wl-pprint`.
To support the `Diagnostic` module, the Trifecta-generated `Doc` has to be
rendered still, so we cannot drop the dependency on ansi-wl-pprint just yet.
