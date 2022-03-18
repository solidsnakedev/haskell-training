import Control.Applicative.Lift (Lift(Pure))
data ExprT
    = Lit Integer
    | Add ExprT ExprT
    | Mul ExprT ExprT
    deriving (Show, Eq)

data ExprT2
    = Lit' Integer
    | Add' ExprT2 ExprT2
    | Mul' ExprT2 ExprT2

--instance Num ExprT where
--    Lit x + Lit y = Lit (x + y)
  --(+) = _
  --(*) = _
  --abs = _
  --signum = _
  --fromInteger = _
  --negate = _

class EvalT a where
    evalT :: a -> Maybe Integer

instance EvalT ExprT where
    evalT (Lit x) = Just x
    evalT (Add x y) = (+) <$> evalT x <*> evalT y
    evalT (Mul x y) = (*) <$> evalT x <*> evalT y

instance EvalT ExprT2 where
    evalT (Lit' x) = Just x
    evalT (Add' x y) = (+) <$> evalT x <*> evalT y
    evalT (Mul' x y) = (*) <$> evalT x <*> evalT y

instance EvalT Int where
    evalT x = pure (fromIntegral x)

   -- evalT (Add' x y) = evalT x + evalT y
   -- evalT (Mul' x y) =  evalT x * evalT y

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y ) = eval x + eval y
eval (Mul x y ) = eval x * eval y

test =  Mul (Add (Lit 2) (Lit 3)) (Lit 4)
    --Mul (Add (Lit 1) (Lit 8)) (Lit 5)

-- parseExps Lit Add Mul "(2+3)*4"
-- Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

main:: IO ()
main = do
    print $ eval test
    print $ evalT (Mul (Add (Lit 1) (Lit 3)) (Lit 7))
    print $ evalT (Mul' (Add' (Lit' 1) (Lit' 3)) (Lit' 7))
    print $ evalT (2::Int)
   -- print $ evalT (3::Integer)

