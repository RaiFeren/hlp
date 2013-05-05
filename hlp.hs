-- HLP Main Driver
-- Rai Feren

module Main where

-- Should import an Abstract Syntax thing
-- Should import a parsing thing
-- Should import an eval thing

import JpnParse
import JpnTrans
import JpnRepr


main :: IO ()
main =
    getContents >>= print . toEngString . transSent . readJpn . lexer