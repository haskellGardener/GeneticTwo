
module Test
where

data Interval = I Double Double

instance Eq Interval
  where
    (==) (I l u) (I l' u') = al <= bl && au > bu
      where ((I al au), (I bl bu)) =
                if l == l'
                then if u == u'
                     then ((I l u), (I l' u'))
                     else if u > u'
                          then ((I l u), (I l' u'))
                          else ((I l' u'), (I l u))
                else if l < l'
                     then ((I l u), (I l' u'))
                     else ((I l' u'), (I l u))

instance Ord Interval
  where
    compare i@(I l u) i'@(I l' u') = if i == i'
                                     then EQ
                                     else if l < l'
                                          then LT
                                          else GT
       

