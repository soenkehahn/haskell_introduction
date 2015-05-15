class: center, middle

# Introduction to Haskell

Sönke Hahn, Zalora

---

# What is Haskell?
  - A functional language that
    - is compiled (& interpreted),
    - is statically typed,
    - has algebraic data types,
    - is pure,
    - allows polymorphism and
    - is lazy.

todo: update

---

# Haskell is Different

There are lots of design patterns from object-oriented or imperative languages
that cannot be applied nicely in Haskell.

The reverse is also true: There are lots of things that you can easily do in
Haskell that you couldn't (or shouldn't) do in most other languages.

Haskell is designed to be more robust and safe.

???

This requires a profound change in thinking.

---

# How to Run Programs?
  - `ghc`: compiler
  - `runhaskell`: interpreter
  - `ghci`: repl
  - `hackage.haskell.org`: package database
  - `cabal`: package manager

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

# Algebraic Data Types

The combination of

- product types and
- sum types
- function types

allows to model most domains nicely:

- dogs and cats,
- enumerations,
- error conditions,
- etc.

???

Corresponds roughly to structs and unions.

```
data Animal
  = Dog String
  | Cat String
```

```
data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
```

```
data ParseResult
  = Success Animal
  | ParseError String

parseAnimal :: String -> ParseResult
parseAnimal = _
```

Relation between types and functions is much more flexible as in OOP.

---

# Types

Types are Haskell's version of describing interfaces. They help to design software

- on a very low-level,
- on a very high-level.

???

low-level: `ghci > :type "huhu"`

mid-level: `ghci > :type sort`

high-level:
  http://hackage.haskell.org/package/RSA-2.1.0.1/docs/Codec-Crypto-RSA-Exceptions.html#g:6

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
  - Purity plays very well with parallel programming.

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


# No Loops but Recursion

Haskell does not have built-in loops. Everything boils down to recursion.

???

`map succ [1, 2, 3]`

`sum [1, 2, 3]`

```
sum_ [] = 0
sum_ (a : r) = a + sum r
```

```
sum_ list = foldl' (+) 0 list
```

---

# Lots of "Language Features" Implemented in Libraries

The combination of Haskell features (laziness, custom infix operators, higher
order functions, purity, etc.) make it very suitable for implementing
- control flow constructs,
- EDSLs

???

`when`

`with`-pattern (todo: example)

diagrams

---

# Conclusion

Haskell

- differs heavily from imperative languages,
- is designed to be safe and
- makes you think much more in types.
