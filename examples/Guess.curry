------------------------------------------------------------------------------
-- Example for HTML programming in Curry:
--
-- A recursive form for a number guessing game
-- which also counts the number of guesses
------------------------------------------------------------------------------

import HTML.Base

main :: IO HtmlForm
main = return $ form "Number Guessing" (guessInput 1)

guessInput :: Int -> [HtmlExp]
guessInput n =
  [htxt "Guess a natural number: ", textfield nref "",
   button "Check" (guessHandler n nref)]   where nref free

guessHandler :: Int -> CgiRef -> (CgiRef -> String) -> IO HtmlForm
guessHandler n nref env = do
  let nr = read (env nref) :: Int
  return $ form "Answer" $
             if nr==42
               then [h1 [htxt $ "Right! You needed "++show n++" guesses!"]]
               else [h1 [htxt $ if nr<42 then "Too small!"
                                         else "Too large!"],
                     hrule] ++ guessInput (n+1)

-- Install the CGI script in user homepage by:
-- > curry-makecgi -o ~/public_html/guess.cgi Guess
