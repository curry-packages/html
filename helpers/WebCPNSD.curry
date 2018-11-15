------------------------------------------------------------------------------
--- A web script to access the CPNS demon.
---
--- If this web script is installed with name `cpnsd.cgi` (see below),
--- one can get information about the status of CGI registry via
--- the web page or in a terminal via `curl`.
---
--- For instance, to see the status of CPNSD, execute
---
---     curl http://localhost/.../cpnsd.cgi?status
---
--- To see the log file of CPNSD, execute
---
---     curl http://localhost/.../cpnsd.cgi?log
---
------------------------------------------------------------------------------

import IOExts ( evalCmd )
import System ( getEnviron )

import HTML.Base
import HTML.CGI

-- Remark: maybe one has to modify the check for non-local accesses
main :: IO HtmlForm
main = do
  param <- getUrlParameter
  remote <- getEnviron "REMOTE_ADDR"
  if remote `elem` ["127.0.0.1","::1"]
    then do result <- execCommand param
            return $ answerText result
    else return $ answerText $
           "Access denied from non-local host (REMOTE_ADDR=" ++ remote ++ ")!\n"

execCommand :: String -> IO String
execCommand param = case param of
  "status" -> cpnsCmd "status"
  "log"    -> cpnsCmd "log"
  "start"  -> cpnsCmd "start"
  _        -> return $ unlines $
                ["ILLEGAL URL PARAMETER: " ++ param, "", cpnsCommands]
 where
  cpnsCmd cmd = do
    (_,out,err) <- evalCmd "curry-cpnsd" [cmd] ""
    return $ out ++ if null err then "" else "ERROR OUTPUT:\n" ++ err

  cpnsCommands = "Allowed arguments: status | log | start"


-- Install the CGI script in user homepage by:
-- > curry-makecgi -o ~/public_html/cpnsd.cgi WebCPNSD
