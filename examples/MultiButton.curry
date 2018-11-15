--- A stress test for the HTML/CGI library.
--- Each page produces 500 buttons, where each event handler for each button
--- generates a new page with 500 buttons.

import HTML.Base

main :: IO HtmlForm
main = return $ form "Multi-Button" $
         map (flip button (const main) . show) [1..500]

-- > curry-makecgi -o ~/public_html/multibut.cgi MultiButton.curry
