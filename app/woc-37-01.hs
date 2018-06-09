{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}

import           Control.Monad
import           Data.List
import           Data.Monoid
import           Text.Printf

-- | Re-implement Foldl library
data Fold a b = forall x. Fold x                -- initial value
                               (a -> x -> x)    -- folding function
                               (x -> b)         -- "extract"

deriving instance Functor (Fold a)

runFold :: Fold a b -> [a] -> b
runFold (Fold z f e) = e . foldl' (flip f) z

instance Applicative (Fold a) where
    pure x = Fold () (\_ _ -> ()) (\_ -> x)
    Fold fz ff fe <*> Fold xz xf xe = Fold
      (fz, xz)
      (\y (fa, xa) -> (ff y fa, xf y xa))
      (\(fa, xa) -> fe fa (xe xa))

prefilter :: (a -> Bool) -> Fold a b -> Fold a b
prefilter p (Fold z f e) = Fold z (\x a -> if p x then f x a else a) e

foldSum :: Num a => Fold a a
foldSum = Fold 0 (+) id

foldLength :: Fold a Int
foldLength = Fold 0 (\_ x -> x + 1) id


-- Goal: Hackerrank Week of Code 17 #1
--
-- - get the number of items
-- - get a list of ints
-- - output average of items >= 90
--
-- Overcomplicated this using the foldl library

-- | Does it all in one traversal/pass
--
-- Build up a complicated fold, using simpler folds (and fold transformers)
theFold :: Fold Int Double
theFold = prefilter (>= 90) $ do
    tot <- fromIntegral <$> foldSum           -- F.Fold Int Double
    n   <- fromIntegral <$> foldLength        -- F.Fold Int Double
    pure (tot / n)

main :: IO ()
main = do
    n    <- readLn @Int
    emps <- replicateM n readLn
    let aveBonus = runFold theFold emps
    printf "%.2f\n" (fromIntegral @Int @Double (truncate (aveBonus * 100 + 0.5)) / 100)
