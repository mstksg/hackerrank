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

-- | Not a monoid
data Command = CSet Int
             | CAdd Int

-- | Turn it into a monoid
runCommand
    :: Command
    -> Endo Int
runCommand = \case
    CSet y -> Endo (const y)
    CAdd y -> Endo (+ y)

-- | Monoid transformer
maxing :: Endo Int -> Endo Int
maxing (Endo f) = Endo $ \x -> max x (f x)

parseCommand  :: String -> Command
parseCommand (words->("add":(read->y):_)) = CAdd y
parseCommand (words->("set":(read->y):_)) = CSet y

main :: IO ()
main = do
    n        <- readLn @Int
    commands <- replicateM n getLine
    let res = flip foldMap commands $ Dual          -- monoid transformer
                                    . maxing        -- monoid transformer
                                    . runCommand    -- turn into a monoid
                                    . parseCommand
    print $ appEndo (getDual res) 0
                    -- ^ need Dual because Endo's default <> is backwards
                    -- Dual is a newtype wrapper that reverses <>
