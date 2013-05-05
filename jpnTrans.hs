module JpnTrans where

import JpnSyn

transSent :: Exp -> Exp
transSent (JSent np vp) = JSent (transNp np) (transVp vp)
transSent (JClause vp) = JClause (transVp vp)

transVp :: VerbPhrase -> VerbPhrase
transVp (Transitive np vb) = Transitive (transNp np) (transVb vb)
transVp (Intransitive vb) = Intransitive (transVb vb)

transNp :: NounPhrase -> NounPhrase
transNp (BasicNoun a) = BasicNoun (tWord a)
transNp (AdjNoun adj n) = AdjNoun (transAp adj) (tWord n)

transAp :: AdjPhrase -> AdjPhrase
transAp (SingleAdj a) = SingleAdj (transAdj a)

transVb :: Verb -> Verb
transVb (Present a) = Present (tWord a)
transVb (Neg a) = Neg (tWord a)
transVb (Past a) = Past (tWord a)
transVb (NegPast a) = NegPast (tWord a)

transAdj :: Adjective -> Adjective
transAdj (BasicAdj a) = BasicAdj (tWord a)


tWord :: String -> String
tWord "tabe" = "eat"
tWord "asobi" = "play"
tWord "boku" = "I"
tWord "watashi" = "I"
tWord "neko" = "cat"
tWord "akai" = "red"
tWord "ookii" = "big"