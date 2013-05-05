{
module JpnParse where

import Data.Char
import Data.List

import JpnSyn

}

%name readJpn
%tokentype {Token}
%error {parseError}
%token
    wa {TokenWa}
    wo {TokenWo}
    noun {TokenNoun $$}
    stem {TokenStem $$}
    masu {VerbPresent}
    masen {VerbNeg}
    mashita {VerbPast}
    masendeshita {VerbNegPast}
%%

Exp :: {Exp}
    : NounPhrase wa VerbPhrase {JSent $1 $3}
    | VerbPhrase {JClause $1}

NounPhrase :: {NounPhrase}
           : noun {BasicNoun $1}
           | AdjPhrase noun {AdjNoun $1 $2}

VerbPhrase :: {VerbPhrase}
           : NounPhrase wo Verb {Transitive $1 $3}
           | Verb {Intransitive $1}


Verb :: {Verb}
     : stem masu {Present $1}
     | stem masen {Neg $1}
     | stem mashita {Past $1}
     | stem masendeshita {NegPast $1}


AdjPhrase :: {AdjPhrase}
           : Adjective {SingleAdj $1}
          
Adjective :: {Adjective}
           : stem {BasicAdj $1}


{
parseError :: [Token] -> a
parseError _ = error "Parse Error"

lexer :: String -> [Token]
lexer [] = []
lexer cs
    | isSuffixOf "wa" firstWord = lexNoun fWordBase ++ [TokenWa] ++ lexer (unwords rest)
    | isSuffixOf "wo" firstWord = lexNoun fWordBase ++ [TokenWo] ++ lexer (unwords rest)
    | isSuffixOf "mas" firstWord = lexVerb firstWord ++ lexer (unwords rest)
    | otherwise = lexVerb firstWord ++ lexer (unwords rest)
    where firstWord:rest = words cs
          fWordBase = take ((length firstWord) - 2) firstWord
--lexer (word++"wa "++rest) = lexNoun word : TokenWa : lexer rest
--lexer (word++"wo "++rest) = lexNoun word : TokenWo : lexVerb rest

lexNoun :: String -> [Token]
lexNoun cs = TokenNoun word : lexer rest
             where (word, rest) = span isAlpha cs

lexVerb :: String -> [Token]
lexVerb cs
    | isSuffixOf "masu" firstWord 
        = TokenStem (fstem 4) : VerbPresent : next
    | isSuffixOf "mashita" firstWord 
        = TokenStem (fstem 7) : VerbPast : next
    | isSuffixOf "masen" firstWord
        = TokenStem (fstem 5) : VerbNeg : next
    | isSuffixOf "masendeshita" firstWord
        = TokenStem (fstem 12) : VerbNegPast : next
    | otherwise = TokenStem firstWord : next
    where firstWord:rest = words cs
          fstem x = take ((length firstWord) - x) firstWord
          next = lexer (unwords rest)


main = getContents >>= print . readJpn . lexer
    
}
