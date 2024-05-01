module TransitionSystem (parseTransitionSystem) where

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)

--parse :: Parsec.Parsec String -> String -> String
--parse rule text = Parsec.parse rule "(source)" text

parseTransitionSystem :: String -> String
parseTransitionSystem input = 
    case result of
        Right v -> "success!"
        Left err -> "error!"
    where
        result = helper input

helper :: String -> Either Parsec.ParseError Char
helper input = do
    letters <- Parsec.parse Parsec.anyChar "(source)" input
    return letters