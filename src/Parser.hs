{-# LANGUAGE DeriveFunctor #-}
module Logic2021Khalansky01 where
import qualified Data.Map as M
import Control.Monad (replicateM)

infixr 3 :->
infixl 4 :|
infixl 5 :&

data Preterm a = Var a
               | Preterm a :& Preterm a
               | Preterm a :| Preterm a
               | Preterm a :-> Preterm a
               | Not (Preterm a)
               deriving (Show, Eq, Functor)

{- |Вернуть либо оценку данной формулы на данных значениях переменных, либо,
если переменных на входе не хватает, сообщить хотя бы одну переменную, которой
нет.

> tm = Var "a" :& Var "b" :| Var "c"
> evalTerm tm (M.fromList [("a", True), ("b", False), ("c", True)])
Right True
> evalTerm tm (M.fromList [("a", True), ("b", False)])
Left "c"
-}
evalTerm :: Ord a => Preterm a -> M.Map a Bool -> Either a Bool
evalTerm (a :& b) m = case evalTerm a m of
    Right True -> case evalTerm b m of
        Right True -> Right True
        Right False -> Right False
        Left a -> Left a
    Right False -> Right False
    Left a -> Left a

evalTerm (a :| b) m = case evalTerm a m of
    Right True -> Right True
    Right False -> case evalTerm b m of
        Right True -> Right True
        Right False -> Right False
        Left a -> Left a
    Left a -> Left a

evalTerm (Var a) m = case M.lookup a m of
    Just v -> Right v
    Nothing -> Left a

evalTerm (Not a) m = case evalTerm a m of
    Right True -> Right False
    Right False -> Right True
    Left a -> Left a

evalTerm (a :-> b) m = case evalTerm a m of
    Right True -> case evalTerm b m of
        Right True -> Right True
        Right False -> Right False
        Left a -> Left a
    Right False -> Right True
    Left a -> Left a

tm = Var "a" :& Var "b" :| Var "c"
--evalTerm tm (M.fromList [("a", True), ("b", False), ("c", True)])
-- evalTerm tm (M.fromList [("a", True), ("b", False)])

type Term = Preterm Int

findMin :: Ord a => Preterm a -> a
findMax :: Ord a => Preterm a -> a
findMin (Var a) = a
findMin (a1 :& a2) = min (findMin a1) (findMin a2)
findMax (Var a) = a
findMax (a1 :& a2) = max (findMax a1) (findMax a2)

propsEquiv :: Term -> Term -> Bool
propsEquiv = undefined

{-
Согласно теореме про условия эквивалентности, есть два возможных варианта, как
решать это задание: либо убедиться, что эквивалентность этих формул является
тавтологией, либо проверить, что на всех входах данные формулы имеют равную
оценку.
-}
