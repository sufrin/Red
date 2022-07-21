--
--
-- AppleRed application script -- uses its own built-in server to open files for editing
-- 
--
on run
        set container to (POSIX path of (path to me)) & "Contents/"
        -- do shell script ("echo " & container & "Java/AppleRed-server " & container & " >> ~/REDAPP")
        do shell script ((container & "Java/AppleRed-server " & container & " > /dev/null 2>&1 &")) 
        -- do shell script ((container & "Java/AppleRed-server " & container & " >> ~/REDLOG 2>&1 &"))
        -- do shell script ("~/bin/redserver  > /dev/null 2>&1 &")
end run
on open dropped
        repeat with p in dropped
                -- display dialog ("~/bin/redserver " & POSIX path of p & " > /dev/null 2>&1 &")
                do shell script ((container & "Java/AppleRed-server " & container & POSIX path of p & " > /dev/null 2>&1 &"))
                -- do shell script ("~/bin/redserver " & POSIX path of p & " > /dev/null 2>&1 &")
        end repeat
end open
--
-- TODO: it's inelegant to have to run the server for each argument
-- BUT:  there are almost no circumstances where there is more than one
--
-- TODO: it might be helpful if the app stayed alive after starting
-- BUT:  right now the server's dock image shows which it is, because
--       the app is (mostly) not alive
--
-- BS (July 2022)


