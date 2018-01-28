module Parser where

import           Control.Applicative
import           Control.Monad (MonadPlus, mplus, mzero)

-- The type of parsers
newtype Parser a = Parser (String -> [(a, String)])

-- Apply a parser
apply :: Parser a -> String -> [(a, String)]
apply (Parser f) = f

-- Return parsed value, assuming at least one successful parse
parse :: Parser a -> String -> a
parse m s = one [ x | (x,t) <- apply m s, t == "" ]
  where
    one [] = error "no parse"
    one [x] = x
    one xs | length xs > 1 = error "ambiguous parse"

instance Monad Parser where
  return x = Parser (\s -> [(x,s)])
  m >>= k = Parser (\s ->
            [ (y, u) |
              (x, t) <- apply m s,
              (y, u) <- apply (k x) t ])

instance Applicative Parser where
  pure        = return
  (<*>) m1 m2 = do { f <- m1; x2 <- m2; return (f x2) }

instance Functor Parser where
  fmap f m = pure f <*> m

instance MonadPlus Parser where
  mzero     = Parser (const [])
  mplus m n = Parser (\s -> apply m s ++ apply n s)

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero
