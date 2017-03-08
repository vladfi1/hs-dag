module DAGHOAS where

import Control.Monad

data DAG a b
  = Node [DAG a b]
  | Let (DAG a b) (a -> DAG a b)
  | Var b

instance Functor (DAG a) where
  fmap f = g where
    g (Node bs) = Node $ map g bs
    g (Let b ab) = Let (g b) (g . ab)
    g (Var b) = Var (f b)

instance Applicative (DAG a) where
  pure = return
  (<*>) = undefined

instance Monad (DAG a) where
  return = Var
  
  Node bs >>= f = Node $ map (>>= f) bs
  Let b ab >>= f = Let (b >>= f) (ab >=> f)
  Var b >>= f = f b

--reverse 

showDAG' :: DAG (b -> String) ([String] -> String) -> [String] -> String
showDAG' (Let b ab) (v:vs) = v ++ " <- " ++ showDAG' b vs ++ "\n" ++ showDAG' (ab (const v)) vs
showDAG' (Node bs) (v:vs) = show $ map (($ vs) . showDAG') bs
showDAG' (Var b) (_:vs) = b vs

vars = [[i] | i <- ['a'..'z']] ++ [i:show j | j <- [1..], i <- ['a'..'z']]

showDAG dag = showDAG' dag vars

{--
return = Var
(>>=) = Let
node = Node . map Var
fail = error "failure is not an option"
--}
