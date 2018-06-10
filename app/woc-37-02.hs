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

runCommand :: Command -> Endo Int
runCommand = \case
    CSet y -> Endo $ const y
    CAdd y -> Endo $ (+ y)

maxing :: Endo Int -> Endo Int
maxing (Endo f) = Endo $ \x -> max x (f x)

parseCommand  :: String -> Command
parseCommand (words->("add":(read->y):_)) = CAdd y
parseCommand (words->("set":(read->y):_)) = CSet y

main :: IO ()
main = do
    n        <- readLn @Int
    commands <- replicateM n getLine
    let res = foldMap (maxing . runCommand . parseCommand) commands
    print $ appEndo res 0
