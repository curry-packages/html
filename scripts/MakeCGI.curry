------------------------------------------------------------------------------
--- Script to compile a Curry program implementing a web script
--- using the package `html` and the library `HTML.Base`
--- into a cgi script to be placed in a server-accessible directory
--- for executing cgi scripts.
---
--- Actually, it just calls the shell script `makecgi.sh`
--- which does all the work.
---
--- @author Michael Hanus
--- @version November 2018
------------------------------------------------------------------------------

module MakeCGI
 where

import Distribution ( installDir )
import FilePath ( (</>) )
import System

import HTML.CGI.PackageConfig ( packagePath )

main :: IO ()
main = do
  args <- getArgs
  rc <- system (unwords ((packagePath </> "scripts" </> "makecgi.sh") :
                         map (\s -> '"' : s ++ "\"") (installDir : args)))
  exitWith rc
