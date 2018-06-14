# 0.2.1

Add function

# 0.2.0.1

Tagged the wrong version as 0.2 on Github. Releasing a new version with an
updated tag to remedy this.

# 0.2

Prettyprint based on the `prettyprinter` library, instead of `ansi-wl-pprint`.
To support the `Diagnostic` module, the Trifecta-generated `Doc` has to be
rendered still, so we cannot drop the dependency on ansi-wl-pprint just yet.
