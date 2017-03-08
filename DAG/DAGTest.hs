{-# LANGUAGE RebindableSyntax #-}

module DAGTest where

import PHOAS hiding (let_)
import DAGPHOAS

import Prelude (($), show, (.), (++), const, map, fromInteger, String, error)

return = Place
(>>=) = let_
fail = error "failure is not an option"

empty = node []

dag = do
  a <- empty
  b <- empty
  c <- node [a, b]
  d <- node [a, b]
  node [c, d]

dag2 = do
  a <- dag
  node [a]

dag3 = Roll $ Node [dag, dag]

