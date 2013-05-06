module JpnTrans where

import JpnSyn

transSent :: Exp -> Exp
transSent (JSent np vp) = JSent (transNp np) (transVp vp)
transSent (JClause vp) = JClause (transVp vp)

transVp :: VerbPhrase -> VerbPhrase
transVp (Transitive np vb) = Transitive (transNp np) (transVb tVt vb)
transVp (Intransitive vb) = Intransitive (transVb tVi vb)

transNp :: NounPhrase -> NounPhrase
transNp (BasicNoun a) = BasicNoun (tN a)
transNp (AdjNoun adj n) = AdjNoun (transAp adj) (tN n)

transAp :: AdjPhrase -> AdjPhrase
transAp (SingleAdj a) = SingleAdj (transAdj a)

transVb :: (String -> String) -> Verb -> Verb
transVb f (Present a) = Present (f a)
transVb f (Neg a) = Neg (f a)
transVb f (Past a) = Past (f a)
transVb f (NegPast a) = NegPast (f a)

transAdj :: Adjective -> Adjective
transAdj (BasicAdj a) = BasicAdj (tA a)


tN :: String -> String
tN "boku" = "I"
tN "watashi" = "I"
tN "neko" = "cat"
tN "gakusei" = "student"
tN "sensei" = "teacher"
tN "kodomo" = "child"
tN "denwa" = "phone"
tN "tomodachi" = "friend"
tN "nihon" = "Japan"
tN "amerika" = "America"
tN "igirisu" = "England"
tN "kankoku" = "Korea"
tN "dare" = "who"
tN "doko" = "where"
tN "sakana" = "fish"
tN "jitensha" = "bicycle"
tN "yasai" = "vegetable"
tN "niku" = "meat"
tN "hon" = "book"
tN "ki" = "tree"
tN "otera" = "temple"
tN "kouen" = "park"
tN "basutei" = "bus stop"
tN "tabemono" = "food"
tN other = other

tVt :: String -> String
tVt "tabe" = "eat"
tVt "nomi" = "drink"
tVt "yomi" = "read"
tVt "kai" = "buy"
tVt "shi" = "do"
tVt "tsukai" = "use"
tVt "tsukuri" = "make"
tVt other = other

tVi :: String -> String
tVi "asobi" = "play"
tVi "oyogi" = "swim"
tVi "shini" = "die"
tVi "tachi" = "stand"
tVi other = other


tVd :: String -> String
tVd "iki" = "go"
tVd "ki" = "come"
tVd "ai" = "meet"
tVd "hairi" = "enter in"
tVd other = other

tA :: String -> String
tA "ookii" = "big"
tA "akai" = "red"
tA "takai" = "expensive"
tA "yasui" = "cheap"
tA "hayai" = "early"
tA "yasashii" = "kind"
tA "kireina" = "beautiful" -- NA
tA "shizukana" = "quiet" -- NA
tA "chiisai" = "small"
tA "omoshiroi" = "interesting"
tA "kowai" = "scary"
tA "tanoshii" = "fun"
tA "nagai" = "long"
tA "mijikai" = "short"
tA "wakai" = "young"
tA other = other
