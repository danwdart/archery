{-# LANGUAGE Safe #-}

module Control.Category.LambdaCalculus.BCKW where

class SKI where
    b :: cat (cat (cat p2 p3) (cat p1 p2)) (cat p1 p3)
    c :: cat (cat p1 (cat p2 p3)) (cat p2 (cat p1 p3))
    k :: cat (cat p1 p2) p1
    w :: cat (cat (cat (cat p1 p1) p2) p1) p2
