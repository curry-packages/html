------------------------------------------------------------------------------
--- A web script to access the CGI registry manager.
---
--- If this web script is installed with name `registry.cgi` (see below),
--- one can get information about the status of CGI registry via
--- the web page or in a terminal via `curl`.
---
--- For instance, to see a list of active CGI server processes, execute
---
---     curl http://localhost/.../registry.cgi?show
---
--- To clean the registry from old CGI server processes, execute
---
---     curl http://localhost/.../registry.cgi?clean
---
------------------------------------------------------------------------------

import List   ( isPrefixOf )
import System ( getEnviron )

import HTML.Base
import HTML.CGI
import HTML.CGI.Config
import HTML.CGI.Registry

-- Remark: maybe one has to modify the check for non-local accesses
main :: IO HtmlForm
main = do
  param <- getUrlParameter
  remote <- getEnviron "REMOTE_ADDR"
  if remote == "127.0.0.1"
    then do result <- execCommand param
            return $ answerText result
    else return $ answerText $
           "Access denied from non-local host (REMOTE_ADDR=" ++ remote ++ ")!\n"

execCommand :: String -> IO String
execCommand param = case param of
  "show"     -> showAllActiveServers
  "load"     -> cmdForAllServers "Status of "        GetLoad
  "status"   -> cmdForAllServers "Status of "        SketchStatus
  "sketch"   -> cmdForAllServers "Sketch status of " SketchHandlers
  "showall"  -> cmdForAllServers "Status of "        ShowStatus
  "clean"    -> do out <- cmdForAllServers "Clean status of " CleanServer
                   getAndCleanRegistry
                   return out
  "stop"     -> do out <- cmdForAllServers "Stop cgi server " StopCgiServer
                   getAndCleanRegistry
                   return out
  "kill" -> killAllActiveServers
  _ -> let stopscript = "stopscript&"
       in if stopscript `isPrefixOf` param
            then stopActiveScriptServers (drop (length stopscript) param)
            else return $ unlines $
                  ["ILLEGAL URL PARAMETER: " ++ param, ""] ++ registryCommands

-- Install the CGI script in user homepage by:
-- > curry-makecgi -o ~/public_html/registry.cgi WebRegistry
