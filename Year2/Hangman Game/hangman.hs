import GHC.IO.Handle ( hSetEcho )
import GHC.IO.Handle.FD (stdin)

main :: IO ()
main = hangman

hangman :: IO ()
hangman = 
    do 
        putStrLn "Think of a word:"
        word <- secretGetLine

        -- create a string of '-'s the same length as the 
        -- inputted word for the characters guessed so far
        let ds = replicate (length word) '-'

        putStrLn ds
        putStrLn "Try to guess it:"

        -- pass in the secret word and the letters guessed so far
        play word ds

-- this method returns a computation of the input string
-- it doesn'y echo the '-' character itserlf
-- that is done by the call to "putSTRLn ds" in the hangman function
-- a variation would be to echo each character as '-' as it is typed
secretGetLine :: IO String
secretGetLine = 
    do
        -- use this input method with no echoing
        hSetEcho stdin False
        xs <- getLine

        -- switch echo off/on
        hSetEcho stdin True
        return xs

play :: String -> String -> IO ()
play word answerSoFar 
    | answerSoFar == word = putStrLn "Correct!!"
    | otherwise =
        do 
            putStrLn "Enter a character:"
            guess <- getChar
            updatedAnswer <- putUpdate (updateMatch word answerSoFar guess)
            play word updatedAnswer

putUpdate :: String -> IO String
putUpdate s = 
    do 
        putStr "Your answer so far is : "
        putStrLn s
        return s

updateMatch :: String -> String -> Char -> String
updateMatch [] [] _ = []

-- If this char has previously been guessed then copy it to the updated answer
updateMatch (x:xs) (y:ys) c | x==y = x : updateMatch xs ys c

-- If this char is currently being guessed then copy it to the updated answer
updateMatch (x:xs) (y:ys) c | x==c = x : updateMatch xs ys c

-- Otherwise the current guess does not match this character so 
-- this character is still unknown in the updated answer.
updateMatch (x:xs) (y:ys) c | otherwise = '-' : updateMatch xs ys c