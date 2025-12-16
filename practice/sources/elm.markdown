Elm uses haskell's symbol class for identifier management
Elm generates JS using its own implementation of an ecmascript data type before converting to a string
Elm uses String, not Data.Text

https://github.com/elm/compiler/blob/master/compiler/src/Generate/JavaScript/Expression.hs
