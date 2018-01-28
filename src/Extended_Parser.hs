module Extended_Parser (multipleStatementsParser) where

  import Parser
  import Datatypes
  import Control.Applicative (many, some)
  import Control.Monad (MonadPlus, mplus, mzero, guard, when, unless)
  import Data.Char (isDigit, isAlpha)

  -- Parse one character
  char :: Parser Char
  char = Parser f
    where
      f [] = []
      f (c:s) = [(c,s)]

  -- match one or more occurrences
  plus :: Parser a -> Parser [a]
  plus p = do
            x <- p
            xs <- some p
            return (x:xs)

  -- Parse a character satisfying a predicate (e.g., isDigit)
  spot :: (Char -> Bool) -> Parser Char
  spot p = do { c <- char; guard (p c); return c }

  -- Match a given character
  token :: Char -> Parser Char
  token c = spot (== c)

  -- match a natural number
  parseNat :: Parser Int
  parseNat = do s <- some (spot isDigit);
                return (read s)

  parseDouble :: Parser Float
  parseDouble = do  s <- parseInt;
                    t <- token '.';
                    d <- parseNat;
                    return  (read (show s ++ [t] ++ show d))


  -- match an identifier
  parseWords :: Parser Words
  parseWords = plus (spot isAlpha)

  -- match a Direction
  parseDirection :: Parser Direction
  parseDirection = parseLeft `mplus` parseRight
              where
                parseLeft   = do {match "leftski";
                                  return Datatypes.Left}
                parseRight  = do {match "rightski";
                                  return Datatypes.Right}

  -- match a negative number
  parseNeg :: Parser Int
  parseNeg = do
              token '-'
              n <- parseNat
              return (-n)

  -- match an integer
  parseInt :: Parser Int
  parseInt = parseNat `mplus` parseNeg

  match :: String -> Parser String
  match = mapM token

  multipleStatementsParser :: Parser Statement
  multipleStatementsParser = do { stmts <- many parseStatement;
                                  return $ Seq stmts}

  parseStatement :: Parser Statement
  parseStatement  = parsePrint `mplus` parseIf `mplus` parseWhile `mplus` parseAssignement `mplus` parseDrive `mplus` parseLight `mplus` parseTurn `mplus` parseComment
            where
              parsePrint        = do {  match "printski(";
                                        body <- parseExpression;
                                        match ");";
                                        return (Print body) }
              parseIf           = do {  match "ifski";
                                        prop <- parseExpression;
                                        match "thenski";
                                        token '{';
                                        thenSt <- multipleStatementsParser;
                                        token '}';
                                        match "elski";
                                        token '{';
                                        elseSt <- multipleStatementsParser;
                                        token '}';
                                        return (If prop thenSt elseSt) }
              parseWhile        = do {  match "whileski";
                                        prop <- parseExpression;
                                        match "doski{";
                                        body <- multipleStatementsParser;
                                        token '}';
                                        return (While prop body) }
              parseAssignement  = do {  name <- parseWords;
                                        match ":=";
                                        exp <- parseExpression;
                                        token ';';
                                        return (Assignement name exp) }
              parseDrive        = do {  match "driveski";
                                        sp <- parseExpression;
                                        token ';';
                                        return (Drive sp)
                                        }
              parseTurn         = do {  match "turnski";
                                        dir <- parseDirection;
                                        expr <- parseExpression;
                                        token ';';
                                        return (Turn dir expr)}
              parseLight        = do {  match "ledski";
                                        token '(';
                                        d1 <- parseExpression;
                                        token ',';
                                        d2 <- parseExpression;
                                        token ',';
                                        d3 <- parseExpression;
                                        token ',';
                                        d4 <- parseExpression;
                                        match ");";
                                        return (Lights d1 d2 d3 d4)}
              parseComment      = do {  match "--";
                                        com <- parseExpression;
                                        match "--";
                                        return Comment
                                        }
  parseExpression :: Parser Expression
  parseExpression = parseDec `mplus` parseIdent `mplus` parseFalse `mplus` parseTrue `mplus` parseLit `mplus` parseAdd `mplus` parseMul `mplus` parseDiv `mplus` parseSub `mplus` parseEq `mplus` parseGreater `mplus` parseLesser `mplus` parseAnd `mplus` parseNot `mplus` parseOr `mplus` parseReadSensor `mplus` parseLightSensor
            where
              parseDec   = do { d <- parseDouble;
                                return (Decimal d)}
              parseIdent = do { n <- parseWords;
                                return (Var n)}
              parseLit   = do { n <- parseInt;
                                return (Con n) }
              parseTrue  = do{  match "#true";
                                return (BoolVal True)}
              parseFalse = do { match "#false";
                                return (BoolVal False)}
              parseAdd   = do { token '(';
                                d <- parseExpression;
                                token '+';
                                e <- parseExpression;
                                token ')';
                                return (Add d e) }
              parseMul   = do { token '(';
                                d <- parseExpression;
                                token '*';
                                e <- parseExpression;
                                token ')';
                                return (Mul d e) }
              parseDiv   = do { token '(';
                                d <- parseExpression;
                                token '/';
                                e <- parseExpression;
                                token ')';
                                return (Div d e) }
              parseSub   = do { token '(';
                                d <- parseExpression;
                                token '-';
                                e <- parseExpression;
                                token ')';
                                return (Sub d e) }
              parseEq    = do { token '(';
                                left  <- parseExpression;
                                match "==";
                                right <- parseExpression;
                                token ')';
                                return (Eq left right) }
              parseGreater = do {   token '(';
                                    left  <- parseExpression;
                                    token '>';
                                    right <- parseExpression;
                                    token ')';
                                    return (Greater left right) }
              parseLesser  = do {   token '(';
                                    left  <- parseExpression;
                                    token '<';
                                    right <- parseExpression;
                                    token ')';
                                    return (Lesser left right) }
              parseAnd = do {       token '(';
                                    left  <- parseExpression;
                                    match "&&";
                                    right <- parseExpression;
                                    token ')';
                                    return (And left right) }
              parseOr = do {        token '(';
                                    left  <- parseExpression;
                                    match "||";
                                    right <- parseExpression;
                                    token ')';
                                    return (Or left right) }
              parseNot = do {       match "~";
                                    prop <- parseExpression;
                                    return (Not prop) }
              parseReadSensor = do {match "#sensorski";
                                    return ReadSensor}
              parseLightSensor = do { match "#lightski";
                                      dir <- parseDirection;
                                      return (LightSensor dir)}
