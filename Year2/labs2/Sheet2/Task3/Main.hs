{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
import MDLTokens
import System.Environment
import Control.Exception
import System.IO


main :: IO ()
main = catch main' noParse

main' :: IO ()
main' = 
    do 
        (fileName : _ ) <- getArgs 
        sourceText <- readFile fileName
        putStrLn ("Parsing : " ++ sourceText)
        let parsedProg = parseCalc (alexScanTokens sourceText)
        putStrLn ("Parsed as " ++ (show parsedProg))

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()