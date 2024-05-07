# type-level-show to-dos
## Performance
I'm using `AppendSymbol` left and right. On the term level, this would be
equivalent to concatenating lists to build strings. What you usually do here
instead is use a builder like `a -> a`, which is what `ShowS` and most other
builders appear to be (I think `DList`s are this too).

I might be able to do something like this...? But notably, `AppendSymbol` is a
built-in type family, defined in `compiler/GHC/Builtin/Types/Literals.hs`. It
might be fast enough to disregard. (I'll ask on #ghc and #haskell.)
