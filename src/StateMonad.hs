module StateMonad (fetch, write, Env, Environment) where

import          Parser
import          Datatypes
import          Extended_Parser


import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy (State, StateT(..), get, put)
import Data.Map as Map
import Data.Maybe

type Environment = Map.Map Words Value

type Env a = StateT Environment IO a

fetch :: String -> Env Value
fetch k = do {  envir <- get;
                return $ fromJust $ Map.lookup k envir}

write :: Words -> Value -> Env()
write n v   = do {  env <- get;
                    put $ Map.insert n v env;
                    return ()}
