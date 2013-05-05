module JpnSyn where

-- Japanese sentences either have a Noun Phrase followed by a Verb Phrase,
-- or drop the noun and assume that the speaker is referring to themselves.
-- After all, its rude to talk explicitly about yourself over and over.
data Exp = JSent NounPhrase VerbPhrase
         | JClause VerbPhrase
           deriving (Show)

data NounPhrase = BasicNoun String
                | AdjNoun AdjPhrase String
                  deriving (Show)

data VerbPhrase = Transitive NounPhrase Verb
                | Intransitive Verb
                  deriving (Show)

data Verb = Present String
          | Neg String
          | Past String
          | NegPast String
          deriving (Show)

-- Need to do work to support negations and the like.
-- Also what about ookikute akai inu?
data AdjPhrase = SingleAdj Adjective
               deriving (Show)

data Adjective = BasicAdj String
               deriving (Show)


data Token = TokenWa
           | TokenWo
           | TokenNoun String
           | TokenStem String
           | VerbPresent
           | VerbNeg
           | VerbPast
           | VerbNegPast
           deriving (Show)

