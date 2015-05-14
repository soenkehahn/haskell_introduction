class: center, middle

# Introduction to Haskell

Sönke Hahn, Zalora

---

# Contents

* auto-gen TOC:
{:toc}

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

# Static Analysis
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
main = putStrLn (sort 1)
main :: IO ()
```

---

# Types

Types are Haskell's version of todo

# Purity
  - no uncontrolled side-effects
    - disk access
    - `stdin`, `stdout`, `stderr`, `argc` and `argv`, environment variables
    - network access
    - global variables
    - object variables
  - A function is nothing more than a mapping from inputs to outputs.

???

Example:
``` haskell
import Data.List

main = putStrLn (reverseWords "Hello, World!")

reverseWords 
```

---

# Higher-Order Functions
  - plays nicely with purity

---

# Polymorphism
  - lists
  - Tree
  - Functor

---

# Laziness
  - `fst (1, 1 / 4)`
  - infinite lists and other tricks
  - can save running time
  - downsides
    - hard to reason about
    - can introduce subtle spaceleaks

---

# Lots of "Language Features" Implemented in Libraries

---

# EDSLs
  - `when`
  - diagrams example

---

# Introduction
