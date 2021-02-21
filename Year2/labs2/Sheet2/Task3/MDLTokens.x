{
    module MDLTokens where
}

--
-- it allows for a digit and an integer as well
-- E := Forward n | Rotate D | Check [1..9] | if E then E else E | E ; E | ( E )
-- D := L | R
--

%wrapper "posn"
$digit = 0-9

tokens :-
    $white+             ;
    "--".*              ;
    Forward             { \p s -> TokenForward p }
    Rotate              { \p s -> TokenRotate p }
    Check               { \p s -> TokenCheck p }
    if                  { \p s -> TokenIf p }
    then                { \p s -> TokenThen p }
    else                { \p s -> TokenElse p }
    L                   { \p s -> TokenLeft p }
    R                   { \p s -> TokenRight p }
    \;                  { \p s -> TokenSeq p }
    \(                  { \p s -> TokenLParen p }
    \)                  { \p s -> TokenRParen p }
    -- distinction between a digit and an integer
    [1..9]              { \p s -> TokenDigit p (read s) }
    $digit $digit+      { \p s -> TokenInt p (read s) }

{
    -- Each action has type :: AlexPosn -> String -> MDLToken 

    -- The token type
data MDLTokens = 
    TokenForward AlexPosn        | 
    TokenRotate  AlexPosn        | 
    TokenDigit AlexPosn Int      |
    TokenInt AlexPosn Int        | 
    TokenCheck AlexPosn          |
    TokenIf AlexPosn             |
    TokenThen AlexPosn           |
    TokenElse AlexPosn           |
    TokenLeft AlexPosn           |
    TokenRight AlexPosn          |
    TokenSeq AlexPosn            |
    TokenLParen AlexPosn         |
    TokenRParen AlexPosn      
    deriving (Eq,Show)

tokenPosn :: MDLTokens -> String
tokenPosn (TokenForward (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRotate  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDigit  (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt  (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCheck  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenThen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLeft (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRight (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSeq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

}