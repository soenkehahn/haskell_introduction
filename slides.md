class: center, middle

# Introduction to Haskell

Sönke Hahn, Zalora

---

# Contents

`toc: todo`

---

# What is Haskell?
  - A functional language that is
    - compiled (& interpreted),
    - statically typed,
    - pure,
    - allows polymorphism and
    - is lazy.

---

# How to Run Programs?
  - `ghc`
  - `runhaskell`
  - hackage
  - `cabal`

???

Examples:
``` haskell
main = putStrLn "Hello, World!"
```

---

# Static Checks
  - imports resolution
  - parsing
  - name resolution
  - type checks

???

Example:
``` haskell
import Data.List

main = putStrLn (sort "Hello, World!")
main = putStrLn (sort "Hello, World!"
main = putStrLn (zort "Hello, World!")
main = putStrLn (sort True)
main :: IO ()
```

---

# Types

Types are Haskell's version of describing interfaces. They help to design software

- on a very low-level,
- on a very high-level.

???

low-level: `ghci > :type "huhu"`

mid-level: `ghci > :type sort`

high-level: `hoogle todo`

``` haskell
import Data.List

main :: IO ()
main = putStrLn (reverseWords "Hello, World!")

reverseWords :: String -> String
reverseWords = _
```

---

# Purity
  - no uncontrolled side-effects
    - disk access
    - `stdin`, `stdout`, `stderr`, `argc` and `argv`, environment variables
    - network access
    - global variables
    - object variables
  - A function is nothing more than a mapping from inputs to outputs.
  - Usually, datastructures are immutable.

This means there are lots of design patterns from object-oriented or imperative languages
that cannot be applied nicely in Haskell.

The reverse is also true: There are lots of things that you can easily do in Haskell that you shouldn't do in most other languages.

???

``` haskell
import Data.List

main :: IO ()
main = putStrLn (reverseWords "Hello, World!")

reverseWords :: String -> String
reverseWords x = unwords (map reverse (words x))
```

---

# Higher-Order Functions
  - Open up a lot of opportunities to parameterize code,
  - play nicely with purity.

???

1.
``` haskell
import Data.List

main :: IO ()
main = putStrLn (modifyWords reverse "Hello, World!")

modifyWords :: (String -> String) -> String -> String
modifyWords f x = unwords (map f (words x))
```
2. Replace `reverse` with `sort`.

---

# Polymorphism
  - containers
    - lists (`[a]`)
    - `Tree`
    - `Map`
  - generalized functions
    - `map`
    - `fmap`

???

- aka "Generics" in Java
- `ghci > :type sort`
- `ghci > sort "hello"`
- `ghci > sort [3, 1, 2]`
- `ghci > :type map`
- `ghci > map succ [3, 1, 2]`
- `ghci > map succ [3, 1, 2]`
- `hoogle Tree`
- `ghci > import Data.Tree`
- `ghci > putStrLn $ drawTree $ Node "huhu" [Node "foo" [Node "bar" []], Node "baz" []]`
- `ghci > putStrLn $ drawTree $ fmap reverse $ Node "huhu" [Node "foo" [Node "bar" []], Node "baz" []]`
- `ghci > :type fmap`

Polymorphism is a very powerful concept to generalize code and make it more reusable.

---

# Laziness

Lazy languages evaluate expressions when needed.

(Strict languages evalute immediately when encountering an expression.)

  - can save running time
  - infinite lists and other tricks
  - downsides
    - hard to reason about
    - can introduce subtle spaceleaks

???

- `:type head`
- `head [1, 2, 3]`
- `head []`
- `(42, head [1, 2, 3])`
- `fst (42, head [1, 2, 3])`
- `fst (42, head [])`

---

# Lots of "Language Features" Implemented in Libraries

The combination of Haskell features (laziness, custom infix operators, higher
order functions, purity, etc.) make it very suitable for implementing
- EDSLs

---

# EDSLs
  - `when`
  - diagrams example
