type Ptr = Int
type UniqueId = Int

data Expr
  = Appplication [Ptr]
  | Op String
  | Magic UniqueId
  | Num Integer
  deriving (Show, Eq)

data GlobalState = GlobalState
  { gsProg    :: Map String Expr
  , gsMemory  :: Map Ptr Expr
  , gsCounter :: UniqueId
  }
  deriving (Show)

data Error = ETooFewArgs

mkMagic :: State GlobalState Expr
mkMagic = do
  i <- gsCounter <$> get
  modify (\s -> s { gsCounter = i + 1 })
  return $ Magic i

step :: Expr -> State GlobalState (Eiter Expr Error)
step (Application [a]) = return $ Left a
step (Application [(Application a) b]) = return $ Left (Application (a ++ b))
step (Application (Op opname: rest)) = undefined -- TODO: check arg count and call opname
step (Op opname) | opname in prog = return $ Left (prog Map.! opname)
step (Op opname) = return $ Left (Op opname)
step (Num num) = return $ Left (Num num)

step' :: Ptr -> State GlobalState (Either Expr Error)
step' ptr = do
  old <- (Map.! ptr) <$> get
  new <- step old
  case new of
    Left val -> do
      modify (\s -> s { gsMemory = Map.insert ptr val gsMemory })
      return new
    Right _ -> return new

eval :: Expr -> State GlobalState (Either Expr Error)
eval (Application ab@(a:b)) | not (isMagic a) = do
  a <- step' ab
  undefined
