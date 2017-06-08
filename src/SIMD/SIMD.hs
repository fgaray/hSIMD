{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
module SIMD.SIMD where

import GHC.Generics
import Test.QuickCheck hiding (property)
import qualified Test.QuickCheck as QC
import qualified Data.SBV as SBV
import Control.Monad.Free
import Control.Monad.Free.TH
import Data.Proxy
import Control.Monad.State
import Data.Graph



data Register128 a = 
      Register128 a a a a
    | Register128Name Int
    deriving (Eq, Show, Generic)

instance Functor Register128 where
    fmap f (Register128 x1 x2 x3 x4) = Register128 (f x1) (f x2) (f x3) (f x4)

instance Applicative Register128 where
    pure x = Register128 x x x x
    (Register128 f1 f2 f3 f4) <*> (Register128 x1 x2 x3 x4) = Register128 (f1 x1) (f2 x2) (f3 x3) (f4 x4)

instance (Arbitrary a) => Arbitrary (Register128 a) where
    arbitrary = Register128 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary





data SIMD a next where
    NewRegister128 :: (a, a, a, a) -> (Register128 a -> next) -> SIMD a next
    Shuffle_ps     :: Register128 a -> Register128 a -> (Int, Int, Int, Int) -> (Register128 a -> next) -> SIMD a next
    Add_ps         :: Register128 a -> Register128 a -> (Register128 a -> next) -> SIMD a next
    Moveldup_ps    :: Register128 a -> (Register128 a -> next) -> SIMD a next
    Movehdup_ps    :: Register128 a -> (Register128 a -> next) -> SIMD a next
    Addsubp_ps     :: Register128 a -> Register128 a -> (Register128 a -> next) -> SIMD a next
    Mul_ps         :: Register128 a -> Register128 a -> (Register128 a -> next) -> SIMD a next
    Min_ps         :: Register128 a -> Register128 a -> (Register128 a -> next) -> SIMD a next
    Max_ps         :: Register128 a -> Register128 a -> (Register128 a -> next) -> SIMD a next
    Function2      :: (Register128 a -> Register128 a -> Register128 a) -> Register128 a -> Register128 a -> (Register128 a -> next) -> SIMD a next
    End            :: SIMD a next
    deriving (Generic, Functor)



instance (Show next, Show a) => Show (SIMD a next) where
    show (NewRegister128 t f) = "NewRegister128 \n" ++ show t ++ (show . f . Register128Name $ 1)
    show (Shuffle_ps _ _ _ f) = "Shuffle\n" ++ (show . f . Register128Name $ 1)
    show (Add_ps _ _ f) = "Add_ps\n" ++ (show . f . Register128Name $ 1)
    show (Moveldup_ps _ f) = "Moveldup_ps\n" ++ (show . f . Register128Name $ 1)
    show (Movehdup_ps _ f) = "Movehdup_ps\n" ++ (show . f . Register128Name $ 1)
    show (Mul_ps _ _ f) = "Mul_ps\n" ++ (show . f . Register128Name $ 1)
    show (Min_ps _ _ f) = "Min_ps\n" ++ (show . f . Register128Name $ 1)
    show (Max_ps _ _ f) = "Max_ps\n" ++ (show . f . Register128Name $ 1)
    show (Function2 _ _ _ f) = "Function2\n" ++ (show . f . Register128Name $ 1)


$(makeFree ''SIMD)


runSIMD :: (Ord a, Num a, Show a, Show next) => (Free (SIMD a)) next -> next
runSIMD (Free program) = runSIMD $ lineInterpreter program
runSIMD (Pure x) = x

makeProgram :: (Free (SIMD a)) next -> [String]
makeProgram = undefined




-- | Interpret one line only without recursion
lineInterpreter :: (Ord a, Num a, Show next, Show a) => (SIMD a) next -> next
lineInterpreter (Add_ps r1 r2 f) = f $ (+) <$> r1 <*> r2
lineInterpreter (Moveldup_ps (Register128 a _ c _) f) = f $ Register128 a a c c
lineInterpreter (Movehdup_ps (Register128 _ b _ d) f) = f $ Register128 b b d d
lineInterpreter (Mul_ps r1 r2 f) = f $ (*) <$> r1 <*> r2
lineInterpreter (Shuffle_ps r1 r2 (x, y, z, w) f) = f $ Register128 (take w r1) (take w r1) (take y r2) (take x r2)
    where
        take :: Int -> Register128 a -> a
        take 0 (Register128 x _ _ _) = x
        take 1 (Register128 _ x _ _) = x
        take 2 (Register128 _ _ x _) = x
        take 3 (Register128 _ _ _ x) = x
lineInterpreter (Addsubp_ps (Register128 a b c d) (Register128 a' b' c' d') f) = f $ Register128 (a - a') (b + b') (c - c') (d + d')
lineInterpreter (NewRegister128 (x1, x2, x3, x4) f) = f $ Register128 x1 x2 x3 x4
lineInterpreter (Min_ps (Register128 a b c d) (Register128 a' b' c' d') f) = f $ Register128 (min a a') (min b b') (min c c') (min d d')
lineInterpreter (Max_ps (Register128 a b c d) (Register128 a' b' c' d') f) = f $ Register128 (max a a') (max b b') (max c c') (max d d')
lineInterpreter (Function2 g r1 r2 f) = f $ (g r1 r2)
lineInterpreter x = error $ show x

