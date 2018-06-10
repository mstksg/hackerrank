{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE ViewPatterns              #-}

import           Control.Monad
import           Data.List
import           Data.Monoid
import           Text.Printf

data Command = CSet Int
             | CAdd Int

theFold :: Int -> Command -> Int
theFold n = \case
    CSet y -> y
    CAdd y -> n + y

parseCommand  :: String -> Command
parseCommand (words->"add":(read->y):_) = CAdd y
parseCommand (words->"set":(read->y):_) = CSet y

main :: IO ()
main = do
    n        <- readLn @Int
    commands <- replicateM n getLine
    let history = scanl theFold 0
                . map parseCommand
                $ commands
    print $ maximum history
