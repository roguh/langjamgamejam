module Util (evens, odds) where

evens :: [t] -> [t]
evens [] = []
evens (x : xs) = x : odds xs

odds :: [t] -> [t]
odds [] = []
odds (_ : xs) = evens xs

