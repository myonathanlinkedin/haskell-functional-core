module Core (Instr(..), runVM) where

data Instr
    = Push Int
    | Add
    | Sub
    | Mul
    | Div
    | Pop
    | Dup
    | Swap
    deriving (Show, Eq)

type Stack = [Int]

runVM :: [Instr] -> Either String Stack
runVM = go []
  where
    go :: Stack -> [Instr] -> Either String Stack
    go st [] = Right st
    go st (i:is) = case i of
        Push n -> go (n:st) is
        Add    -> binOp (+) st is
        Sub    -> binOp (-) st is
        Mul    -> binOp (*) st is
        Div    -> case st of
                    (0:_:_) -> Left "division by zero"
                    _       -> binOp div st is
        Pop    -> case st of
                    _:rest -> go rest is
                    []     -> Left "pop from empty stack"
        Dup    -> case st of
                    x:xs -> go (x:x:xs) is
                    []   -> Left "dup on empty stack"
        Swap   -> case st of
                    x:y:xs -> go (y:x:xs) is
                    _      -> Left "swap needs two elements"

    binOp :: (Int -> Int -> Int) -> Stack -> [Instr] -> Either String Stack
    binOp op (x:y:xs) rest = go (op y x : xs) rest
    binOp _ _ _            = Left "binary operation on insufficient stack"
