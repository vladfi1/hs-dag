module DAGPHOAS where

import PHOAS hiding (let_)

import Data.Profunctor
import Data.Foldable
import Data.Traversable
import Control.Monad (join)

--data SimpleDAGF a b
-- = Let [b] (a -> b)

data DAGF a b
  = Node [b]
  | Let b (a->b)

instance Functor (DAGF a) where
  fmap f (Node bs) = Node $ map f bs
  fmap f (Let b ab) = Let (f b) (f . ab)

instance Profunctor DAGF where
  dimap a2b c2d (Node cs) = Node $ map c2d cs
  dimap a2b c2d (Let c b2c) = Let (c2d c) (c2d . b2c . a2b)

showDAGF :: DAGF (b -> String) ([String] -> String) -> [String] -> String
showDAGF (Let b ab) (v:vs) = v ++ " <- " ++ b vs ++ "\n" ++ ab (const v) vs
showDAGF (Node bs) vs = "[" ++ concatMap ((++ ", ") . ($ vs)) bs ++ "]"
--showDAGF (Node bs) vs = show $ map ($ vs) bs

type DAG = Rec DAGF

showDAG :: DAG (b -> String) ([String] -> String) -> String
showDAG dag = cata showDAGF dag vars

let_ a f = Roll (Let a f)
node = Roll . Node . map var

doubleF :: DAGF a b -> DAGF a b
doubleF (Node bs) = Node (bs ++ bs)
doubleF dag = dag

double = bottomUp doubleF

--paths 
nodes (Roll (Node dags)) = 1 + sum (map nodes dags)
nodes (Roll (Let dag f)) = nodes dag + nodes (f ())
nodes (Place _) = 0

edges (Roll (Node dags)) = sum (map edges dags)
edges (Roll (Let dag f)) = edges dag + edges (f ())
edges (Place _) = 1

--wrap 
wrapF b = Node [b]

--canonicalF :: DAGF a (DAG a b) ->
canonicalF (Node dags) = fmap Node (sequenceA dags)
--canonicalF (Let dag f) = 
--canonicalF (Roll (Let dag f)) = do
--  b <- canonicalF dag
--  Roll $ Let b (canonicalF . f)

liftLet :: DAG a b -> DAG a (DAG a b)
liftLet (Roll (Node dags)) = Roll . Node <$> traverse liftLet dags
liftLet (Roll (Let dag f)) = Roll $ Let (Place dag) (Place . f)
--liftLet (Let dag f) = Let <$> (liftLet dag) <*> f
liftLet dag = Place dag

canonicalForm :: DAG a b -> DAG a b
canonicalForm = join . liftLet

empty_dag = Roll $ Node []

let_dag = Roll $ Let empty_dag Place

bad_dag = Roll $ Node [let_dag]
good_dag = Roll $ Let empty_dag (\a -> Roll $ Node [Place a])

lift_empty_dag = Place (Roll $ Node [])
lift_let_dag = Roll $ Let (Place empty_dag) (liftLet . Place)

lift_bad_dag = Roll . Node <$> (sequenceA [lift_let_dag])

--canon (Roll (Node [Roll (Let dag f)])) = Roll (Let dag (\a -> Roll (Node [f a])))
--canon (Roll (Node [Roll (Let d1 f1), Roll (Let d2 f2)])) = Roll $ Let d1 (\a1 -> Roll $ Let d2 (\a2 -> Roll $ Node [f1 a1, f2 a2]))

--DAG a b -> 

canon (Roll (Let dag f)) = pushLet (\a -> Roll $ Let a f) (canon dag)
canon (Roll (Node [dag])) = pushLet (\a -> Roll $ Node [a]) (canon dag)
canon (Roll (Node [dag1, dag2])) = pushLet (\a -> pushLet (\b -> Roll $ Node [a, b]) (canon dag2)) (canon dag1)
--canon (Roll (Node dag:dags)) = pushLet (\a -> Roll $ Node [a]) (canon dag)
--canon (Place a) = Place a
canon dag = dag

bad_dag2 = Roll $ Let let_dag Place
good_dag2 = Roll $ Let empty_dag (\b -> Roll $ Let (Place b) Place)

pushLet g (Roll (Let dag f)) = Roll $ Let dag (pushLet g . f)
pushLet g dag = g dag

--reverse (Roll (Let dag Place)) = Roll $ Let empty_dag 
--reverse (Roll (Let dag f)) = Roll $ Let (reverse . f) (reverse dag)
reverse dag = dag

outgoing f = count (f 1) where
  count (Roll (Let dag g)) = count dag + count (g 0)
  count (Roll (Node dags)) = sum $ map count dags
  count (Place b) = b

--eliminate :: (a -> DAG a b) -> Maybe (DAG a b)
--eliminate f = sequenceA (f Nothing)

--eliminate f = helper (f Nothing) where
--  helper (Roll (Let dag g)) = _ (\a -> helper (g a))

