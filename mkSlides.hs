#!/usr/bin/env runhaskell

import           Data.List

main :: IO ()
main = do
  template <- readFile "template.html"
  slides <- readFile "slides.md"
  writeFile "slides.html" (replace "###" slides template)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace needle replacement text
  | needle `isPrefixOf` text = replacement ++ (drop (length needle) text)
replace needle replacement (a : r) = a : replace needle replacement r
replace _ _ [] = []
