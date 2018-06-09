{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Monad
import           Data.Monoid
import           Text.Printf
import qualified Control.Foldl   as F

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
theFold :: F.Fold Int Double
theFold = F.prefilter (>= 90) $ do
    tot <- fromIntegral <$> F.sum           -- F.Fold Int Double
    n   <- fromIntegral <$> F.length        -- F.Fold Int Double
    pure (tot / n)

main :: IO ()
main = do
    n    <- readLn @Int
    emps <- replicateM n readLn
    let aveBonus = F.fold theFold emps
    printf "%.2f\n" aveBonus
