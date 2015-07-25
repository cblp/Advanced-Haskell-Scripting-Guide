#!/usr/bin/env runhaskell
{-# LANGUAGE ViewPatterns #-}
-- Cleanup, version 3

-- Warning:
----------
-- This script uses quite a number of features that will be explained
-- later on.
-- By the time you've finished the first half of the book,
-- there should be nothing mysterious about it.

import Control.Monad
import Safe
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Posix

logDir = "/var/log"
rootUid = 0                   -- ^ Only users with UID 0 have root privileges.
defaultLines = 50             -- ^ Default number of lines saved.
errorXcd = ExitFailure 86     -- ^ Can't change directory?
errorNotRoot = ExitFailure 87 -- ^ Non-root exit error.

main = do
    -- Run as root, of course.
    uid <- getRealUserID
    when (uid /= rootUid) $ do
        hPutStrLn stderr "Must be root to run this script."
        -- @cblp: here was just `echo` in the original guide, but I think
        -- it is better to write errors to stderr
        exitWith errorNotRoot

    args <- getArgs
    -- Test whether command-line argument is present.
    let lines = if length args >= 2
        then read (args !! 1)
        else defaultLines -- Default, if not specified on command-line.

    -- Stephane Chazelas suggests the following,
    -- as a better way of checking command-line arguments,
    -- but this is still a bit advanced for this stage of the tutorial.
    --
    -- let errorWrongArgs = ExitFailure 85
    -- -- ^ Non-numerical argument (bad argument format).
    --
    -- lines <- case args !! 1 of
    --     "" ->
    --         return defaultLines
    --     (readMay -> Just n) ->
    --         return n
    --     _ -> do
    --         hPutStrLn stderr $
    --             "Usage: " ++ takeFileName (head args) ++ " lines-to-cleanup"
    --         exitWith errorWrongArgs
    --
    -- Skip ahead to "Loops" (TODO check interlinks) chapter to decipher all this.

    setCurrentDirectory logDir

    pwd <- getCurrentDirectory
    -- Doublecheck if in right directory before messing with log file.
    when (pwd /= logDir) $ do -- Not in /var/log?
        hPutStrLn stderr $ "Can't change to " ++ logDir ++ "."
        exitWith errorXcd

    -- Far more efficient is:
    --
    -- TODO haskellize this
    -- cd "/var/log" || {
    --     hPutStrLn stderr "Cannot change to necessary directory."
    --     exitWith errorXcd
    -- }

    -- TODO haskellize this
    -- tail -n $lines messages > mesg.temp  -- Save last section of message log file.
    renameFile "mesg.temp" "messages"       -- Rename it as system log file.

    -- writeFile "messages" ""
    -- No longer needed, as the above method is safer.

    writeFile "wtmp" ""
    putStrLn "Log files cleaned up."
    -- Note that there are other log files in /var/log not affected
    -- by this script.

    -- A zero return value from the script upon exit indicates success
    -- to the shell.
