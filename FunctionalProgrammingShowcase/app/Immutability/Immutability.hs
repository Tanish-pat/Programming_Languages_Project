module Immutability where

-- Immutable base list
immutableList :: [Int]
immutableList = [1, 2, 3]

addElement :: Int -> [Int]
addElement x = x : immutableList

multiplyElements :: Int -> [Int]
multiplyElements factor = map (* factor) immutableList

filterElementsGreater :: Int -> [Int]
filterElementsGreater threshold = filter (> threshold) immutableList

filterElementsLesser :: Int -> [Int]
filterElementsLesser threshold = filter (< threshold) immutableList
