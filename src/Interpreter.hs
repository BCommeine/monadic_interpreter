import StateMonad
import Extended_Parser
import Parser
import Evaluator

import Data.Map (empty)

import System.Environment (getArgs)
import Control.Monad.State.Lazy (runState)
import Control.Monad.Trans.State.Lazy (State, StateT(..), get, put, modify)

main :: IO ()
main = do   filePath <- getArgs;
            content <- readFile $ head filePath;
            let stmts = parse multipleStatementsParser (prepare content);
            let e = Data.Map.empty;
            runStateT (execute stmts) e;
            return ()

prepare:: String -> String
prepare = filter (`notElem` "\t\n\r ")
