module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest ["src", "-ignore-package prettyprinter-compat-ansi-wl-pprint"]
