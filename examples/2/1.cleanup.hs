-- Cleanup
-- Run as root, of course.

import System.Directory

main = do
    setCurrentDirectory "/var/log"
    writeFile "messages" ""
    writeFile "wtmp" ""
    putStrLn "Log files cleaned up."
