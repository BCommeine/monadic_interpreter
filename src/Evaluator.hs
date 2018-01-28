module Evaluator (eval, execute) where
  import  Datatypes
  import  StateMonad
  import  MBot

  import  Control.Monad.IO.Class
  import  Control.Monad (MonadPlus, mplus, mzero, guard, when, unless, void)

  add :: Value -> Value -> Value
  add (Num x) (Num y) = Num (x + y)
  add (Dec x) (Dec y) = Dec (x + y)
  add (Dec x) (Num y) = Dec (x + fromIntegral y)
  add (Num x) (Dec y) = Dec (fromIntegral x + y)
  add _       _       = Wrong

  mul :: Value -> Value -> Value
  mul (Num x) (Num y) = Num (x * y)
  mul (Dec x) (Dec y) = Dec (x * y)
  mul (Dec x) (Num y) = Dec (x * fromIntegral y)
  mul (Num x) (Dec y) = Dec (fromIntegral x * y)
  mul _        _      = Wrong

  sub :: Value -> Value -> Value
  sub (Num x) (Num y) = Num (x - y)
  sub (Dec x) (Dec y) = Dec (x - y)
  sub (Dec x) (Num y) = Dec (x - fromIntegral y)
  sub (Num x) (Dec y) = Dec (fromIntegral x - y)
  sub _       _       = Wrong

  divide :: Value -> Value -> Value
  divide (Num x) (Num y) = Num (x `div` y)
  divide (Dec x) (Dec y) = Dec (x / y)
  divide (Dec x) (Num y) = Dec (x / fromIntegral y)
  divide (Num x) (Dec y) = Dec (fromIntegral x / y)
  divide _        _      = Wrong

  equal :: Value -> Value -> Value
  equal (Num x)   (Num y)   = Bool (x == y)
  equal (Bool x)  (Bool y)  = Bool (x == y)
  equal (Dec x)   (Dec y)   = Bool (x == y)
  equal (Dec x)   (Num y)   = Bool (x == fromIntegral y)
  equal (Num x)   (Dec y)   = Bool (fromIntegral x == y)
  equal _       _           = Wrong

  greater :: Value -> Value -> Value
  greater (Num x) (Num y)   = Bool (x > y)
  greater (Dec x) (Dec y)   = Bool (x > y)
  greater (Dec x) (Num y)   = Bool (x > fromIntegral y)
  greater (Num x) (Dec y)   = Bool (fromIntegral x > y)
  greater _       _         = Wrong

  lesser :: Value -> Value -> Value
  lesser (Num x) (Num y)   = Bool (x < y)
  lesser (Dec x) (Dec y)   = Bool (x < y)
  lesser (Dec x) (Num y)   = Bool (x < fromIntegral y)
  lesser (Num x) (Dec y)   = Bool (fromIntegral x < y)
  lesser _       _         = Wrong

  andBool :: Value -> Value -> Value
  andBool (Bool x)  (Bool y)  = Bool ( x && y)
  andBool _         _         = Wrong

  orBool :: Value -> Value -> Value
  orBool (Bool x)  (Bool y)  = Bool ( x || y)
  orBool _         _         = Wrong

  notBool :: Value -> Value
  notBool (Bool False)  = Bool True
  notBool (Bool True )  = Bool False
  notBool _             = Wrong

  drive :: Value -> Env()
  drive (Num v) = do {    d <- liftIO openMBot;
                          liftIO $ sendCommand d $ setMotor v v;
                          liftIO $ closeMBot d; }
  drive _       = return ()

  turn :: Direction -> Value -> Env()
  turn Datatypes.Right (Num v) = do {    d <- liftIO openMBot;
                                liftIO $ sendCommand d $ setMotor v 0;
                                liftIO $ closeMBot d; }
  turn Datatypes.Left (Num v) = do {    d <- liftIO openMBot;
                                liftIO $ sendCommand d $ setMotor 0 v;
                                liftIO $ closeMBot d; }
  turn _     _      = return ()


  lights :: Value -> Value -> Value -> Value -> Env()
  lights (Num e1) (Num e2) (Num e3) (Num e4) = do {   d <- liftIO openMBot;
                                                       liftIO $ sendCommand d $ setRGB e1 e2 e3 e4;
                                                       liftIO $ closeMBot d; }
  lights _        _         _        _       = return ()

  processLine :: Line -> Direction -> Value
  processLine LEFTB Datatypes.Left    = Bool False
  processLine LEFTB Datatypes.Right   = Bool True
  processLine RIGHTB Datatypes.Left   = Bool True
  processLine RIGHTB Datatypes.Right  = Bool False
  processLine BOTHB   _               = Bool False
  processLine BOTHW   _               = Bool True



  eval :: Expression -> Env Value
  eval (Var x)              = fetch x
  eval (Con x)              = return (Num x)
  eval (BoolVal x)          = return (Bool x)
  eval (Decimal x)          = return (Dec x)
  eval (Add t1 t2)          = do {  a <- eval t1;
                                    b <- eval t2;
                                    return (add a b) }
  eval (Mul t1 t2)          = do {  a <- eval t1;
                                    b <- eval t2;
                                    return (mul a b) }
  eval (Div t1 t2)          = do {  a <- eval t1;
                                    b <- eval t2;
                                    return (divide a b) }
  eval (Sub t1 t2)          = do {  a <- eval t1;
                                    b <- eval t2;
                                    return (sub a b) }
  eval (Eq t1 t2)           = do {  a <- eval t1;
                                    b <- eval t2;
                                    return (equal a b) }
  eval (Greater t1 t2)      = do {  a <- eval t1;
                                    b <- eval t2;
                                    return (greater a b) }
  eval (Lesser t1 t2)       = do {  a <- eval t1;
                                    b <- eval t2;
                                    return (lesser a b) }
  eval (And t1 t2)          = do {  a <- eval t1;
                                    b <- eval t2;
                                    return (andBool a b) }
  eval (Or t1 t2)           = do {  a <- eval t1;
                                    b <- eval t2;
                                    return (orBool a b) }
  eval (Not t1)             = do {  a <- eval t1;
                                    return (notBool a) }
  eval ReadSensor         = do {    d <- liftIO openMBot;
                                      v <- liftIO $ readUltraSonic d;
                                      liftIO $ closeMBot d;
                                      return (Dec v)}
  eval (LightSensor dir)  = do {    d <- liftIO openMBot;
                                    line <- liftIO $ readLineFollower d;
                                    return (processLine line dir)}
  execute :: Statement -> Env()
  execute Comment                 =  return ()
  execute (Assignement name expr) =  do { v <- eval expr;
                                          write name v}
  execute (If expr1 stmt1 stmt2)   = do { c <- eval expr1;
                                          if c == Bool True
                                          then execute stmt1
                                          else  when( c == Bool False ) $ execute stmt2}

  execute (While expr stmt) = let loop() =  do {
                                          c <- eval expr;
                                          unless ( c == Bool False)
                                          $ do {execute stmt; loop()} }
                              in loop()
  execute (Print expr) = liftIO . void . print =<< eval expr
  execute (Seq stmts) =  foldr ((>>) . execute) (return ()) stmts
  execute (Drive sp) = do {  v <- eval sp;
                             drive v}
  execute (Turn dir sp) = do {  v <- eval sp;
                                turn dir v}
  execute (Lights d1 d2 d3 d4) = do { e1 <- eval d1;
                                      e2 <- eval d2;
                                      e3 <- eval d3;
                                      e4 <- eval d4;
                                      lights e1 e2 e3 e4 }
