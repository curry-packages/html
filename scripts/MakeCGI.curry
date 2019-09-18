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
--- @version September 2019
------------------------------------------------------------------------------

module MakeCGI
 where

import Directory    ( doesFileExist )
import Distribution ( installDir )
import FilePath     ( (</>) )
import List         ( isPrefixOf )
import System

import HTML.CGI.PackageConfig ( packagePath )

main :: IO ()
main = do
  args0 <- getArgs
  -- check whether option `--system=xxx` is provided:
  let (args1,args2) = break ("--system=" `isPrefixOf`) args0
      (root,args)  = if null args2
                        then (installDir, args1)
                        else (drop 9 (head args2), args1 ++ tail args2)
  checkCurrySystem root
  rc <- system (unwords ((packagePath </> "scripts" </> "makecgi.sh") :
                         map (\s -> '"' : s ++ "\"") (root : args)))
  exitWith rc

checkCurrySystem :: String -> IO ()
checkCurrySystem currydir = do
  let currybin = currydir </> "bin" </> "curry"
  isexec <- doesFileExist currybin
  unless isexec $
    error $ "Curry system executable '" ++ currybin ++ "' does not exist!"
