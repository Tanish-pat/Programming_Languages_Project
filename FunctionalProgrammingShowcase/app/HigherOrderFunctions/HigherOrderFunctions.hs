{-# LANGUAGE TemplateHaskell #-}

module HigherOrderFunctions where

import Data.Dynamic
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)

-- 1. Dynamic Function Composition
composeDynamic :: Dynamic -> Dynamic -> Dynamic -> Maybe Dynamic
composeDynamic f g x = do
    f' <- fromDynamic f :: Maybe (Int -> Int)
    g' <- fromDynamic g :: Maybe (Int -> Int)
    x' <- fromDynamic x :: Maybe Int
    return . toDyn $ f' (g' x')

-- 2. Map Function with Dynamic Input
dynamicMap :: Dynamic -> Dynamic -> Maybe Dynamic
dynamicMap f xs = do
    f'  <- fromDynamic f  :: Maybe (Int -> Int)
    xs' <- fromDynamic xs :: Maybe [Int]
    return . toDyn $ map f' xs'

-- 3. Template Haskell Code Generation
generateAdd :: Int -> Q Exp
generateAdd x = [| \y -> y + $(lift x) |]

-- 4. Runtime Function Factory
functionFactory :: Dynamic -> Dynamic -> Maybe Dynamic
functionFactory x y = do
    x' <- fromDynamic x :: Maybe Int
    y' <- fromDynamic y :: Maybe Int
    return . toDyn $ (\z -> z * (x' + y'))

-- 5. Creating Functions at Runtime (Partial Application)
createAdder :: Dynamic -> Maybe (Dynamic -> Maybe Dynamic)
createAdder x = do
    x' <- fromDynamic x :: Maybe Int
    return $ \y -> do
        y' <- fromDynamic y :: Maybe Int
        return . toDyn $ x' + y'











-- module HigherOrderFunctions where

-- import Data.Char (toUpper, toLower)

-- -- Existing integer operations:
-- applyToAll :: (a -> b) -> [a] -> [b]
-- applyToAll = map

-- filterWith :: (a -> Bool) -> [a] -> [a]
-- filterWith = filter

-- foldList :: (a -> b -> b) -> b -> [a] -> b
-- foldList _ acc []     = acc
-- foldList f acc (x:xs) = foldList f (f x acc) xs

-- multiplier :: Int -> (Int -> Int)
-- multiplier x = (* x)

-- add :: Int -> (Int -> Int)
-- add x y = x + y

-- dynamicOperation :: String -> (Int -> Int)
-- dynamicOperation op = case op of
--   "double" -> (* 2)
--   "square" -> (^ 2)
--   _        -> id

-- applyDynamicOp :: String -> [Int] -> [Int]
-- applyDynamicOp op = map (dynamicOperation op)

-- applyPipeline :: [a -> a] -> a -> a
-- applyPipeline = foldl (flip (.)) id

-- -- New: string‐based dynamic operations
-- dynamicStringOp :: String -> String -> String
-- dynamicStringOp op = case op of
--   "reverse" -> reverse
--   "upper"   -> map toUpper
--   "lower"   -> map toLower
--   _         -> id

-- applyDynamicStringOp :: String -> [String] -> [String]
-- applyDynamicStringOp op = map (dynamicStringOp op)
