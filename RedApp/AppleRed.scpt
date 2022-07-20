--
--
-- AppleRed application script -- uses redserver to open files for editing
-- 
--
on run
        do shell script ("~/bin/redserver  > /dev/null 2>&1 &")
end run
on open dropped
        repeat with p in dropped
                -- display dialog ("~/bin/redserver " & POSIX path of p & " > /dev/null 2>&1 &")
                do shell script ("~/bin/redserver " & POSIX path of p & " > /dev/null 2>&1 &")
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


