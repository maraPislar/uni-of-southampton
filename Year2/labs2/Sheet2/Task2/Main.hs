import Tokens
import System.Environment
import Control.Exception
import System.IO

main :: IO ()
-- write an exception handler
main = catch main' noLex

main' :: IO ()
main' =
    do
        -- getArgs is an expression
        -- looks at the command line to get arguments
        -- returns a list of strings
        -- only gets the first argument (in this case) 
        -- which is called fileName
        (fileName : _) <- getArgs
        sourceText <- readFile fileName
        putStrLn ("Lexing : " ++ sourceText)
        -- this is a pure function
        -- a list of tokens
        let lexedProg = alexScanTokens sourceText
        putStrLn ("Lexed as : " ++ show lexedProg)

-- error handler
noLex :: ErrorCall -> IO ()
noLex e = 
    do
        let err = show e
        -- the standard error handler
        hPutStr stderr ("Problem with lexing: " ++ err)
        return ()