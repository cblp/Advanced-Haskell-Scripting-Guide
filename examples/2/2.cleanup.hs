#!/usr/bin/env runhaskell
-- Proper header for a Haskell script.

-- Cleanup, version 2

import System.Directory

main = do
    -- Run as root, of course.
    -- Insert code here to print error message and exit if not root.
    let logDir = "/var/log" -- Variables are better than hard-coded values.
    setCurrentDirectory logDir

    writeFile "messages" ""
    writeFile "wtmp" ""

    putStrLn "Log files cleaned up."

    -- The right and proper method of "exiting" from a script is
    -- just end of the `main` function.
