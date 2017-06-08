{-# LANGUAGE ScopedTypeVariables #-}
module Example where

import SIMD.SIMD
import Control.Monad.Free
import Test.QuickCheck hiding (property)
import qualified Test.QuickCheck as QC


sort4MinMax' :: (Num a, Show a) => Register128 a -> (Free (SIMD a)) (Register128 a, Register128 a)
sort4MinMax' v = do
    v2 <- shuffle_ps v v (1, 0, 3, 2)
    v3_min <- min_ps v2 v
    v3_max <- max_ps v2 v
    v3 <- shuffle_ps v3_max v3_min (3, 2, 1, 0)
    v3' <- shuffle_ps v3 v3 (2, 3, 0, 1)
    v4_min <- min_ps v3' v3
    v4_max <- max_ps v3' v3
    v4_max_min <- shuffle_ps v4_max v4_min (3, 1, 2, 0)
    v5 <- shuffle_ps v4_max_min v4_max_min (3, 1, 2, 0)
    v5' <- shuffle_ps v5 v5 (3, 1, 2, 3)
    v6_min <- min_ps v5' v5
    v6_max <- max_ps v5' v5
    return (v6_min, v6_max)



sort4MinMax :: (Show a, Num a) => Register128 a -> (Free (SIMD a)) (Register128 a)
sort4MinMax v = do
    (v6_min, v6_max) <- sort4MinMax' v
    v6 <- shuffle_ps v6_min v6_max (0, 1, 2, 3)
    return v6


sort4MinMaxReverse :: (Show a, Num a) => Register128 a -> (Free (SIMD a)) (Register128 a)
sort4MinMaxReverse v = do
    (v6_min, v6_max) <- sort4MinMax' v
    v6 <- shuffle_ps v6_max v6_min (3, 2, 1, 0)
    return v6


bitonicSort :: (Num a) => Register128 a -> Register128 a -> (Free (SIMD a)) (Register128 a, Register128 a)
bitonicSort v1 v2 = do
    v1'        <- shuffle_ps v1 v1 (3, 1, 2, 0)
    v2'        <- shuffle_ps v2 v2 (3, 1, 2, 0)
    v_e1_min   <- min_ps v1' v2'
    v_e1_max   <- max_ps v1' v2'
    v_e1_min'  <- shuffle_ps v_e1_min v_e1_max (0, 2, 2, 0)
    v_e1_max'  <- shuffle_ps v_e1_min v_e1_max (3, 1, 3, 1)
    v_e1_min'' <- shuffle_ps v_e1_min' v_e1_min' (2, 1, 3, 0)
    v_e1_max'' <- shuffle_ps v_e1_max' v_e1_max' (3, 1, 2, 0)
    v_e2_min   <- min_ps v_e1_min'' v_e1_max''
    v_e2_max   <- max_ps v_e1_min'' v_e1_max''
    v_e2_min'  <- shuffle_ps v_e2_min v_e2_max (1, 0, 1, 0)
    v_e2_max'  <- shuffle_ps v_e2_min v_e2_max (3, 2, 3, 2)
    v_e2_min'' <- shuffle_ps v_e2_min' v_e2_min' (3, 1, 2, 0)
    v_e2_max'' <- shuffle_ps v_e2_max' v_e2_max' (3, 1, 2, 0)
    v_e3_min   <- min_ps v_e2_min'' v_e2_max''
    v_e3_max   <- max_ps v_e2_min'' v_e2_max''

    b2_min'  <- shuffle_ps v_e3_min v_e3_max (1, 0, 1, 0)
    b2_max'  <- shuffle_ps v_e3_min v_e3_max (3, 2, 3, 2)
    b2_min'' <- shuffle_ps b2_min' b2_min' (3, 1, 2, 0)
    b2_max'' <- shuffle_ps b2_max' b2_max' (3, 1, 2, 0)

    return (b2_min'', b2_max'')






-- QuickCheck properties
check = do
    quickCheck (\(a :: Int, b, c, d) -> (\(Register128 a' b' c' d') -> a' <= b' && b' <= c' && c' <= d' && a' <= d') . runSIMD $ sort4MinMax (Register128 a b c d))
    quickCheck (\(a :: Int, b, c, d, e, f, g, h) ->
        (\((Register128 a' b' c' d'), (Register128 e' f' g' h')) -> a' <= b'  && b' <= c' && c' <= d' && d' <= e' && f' <= g' && g' <= h')
            . runSIMD $ bitonicSort (Register128 a b c d) (Register128 e f g h))
