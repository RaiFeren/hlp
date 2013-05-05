module JpnRepr where

import JpnSyn

toEngString :: Exp -> String
toEngString (JSent np vp) = (toEngNp np) ++ " " ++ (toEngVp vp)
toEngString (JClause vp) = "I " ++ (toEngVp vp)

toEngVp :: VerbPhrase -> String
toEngVp (Transitive np vb) = (toEngVb vb) ++ " " ++ (toEngNp np)
toEngVp (Intransitive vb) = toEngVb vb

toEngNp :: NounPhrase -> String
toEngNp (BasicNoun a) = a
toEngNp (AdjNoun adj n) = (toEngAp adj) ++ " " ++ n

toEngAp :: AdjPhrase -> String
toEngAp (SingleAdj a) = toEngAdj a

toEngVb :: Verb -> String
toEngVb (Present a) = a
toEngVb (Neg a) = "will not " ++ a
toEngVb (Past a) = "did " ++ a
toEngVb (NegPast a) = "did not " ++ a

toEngAdj :: Adjective -> String
toEngAdj (BasicAdj a) = a