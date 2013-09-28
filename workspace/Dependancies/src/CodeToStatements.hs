

hasBreak :: String -> Bool
hasBreak code@(c:_) = hasAny [c] statementBreaks || endOfInclude code
hasBreak _          = False


endOfInclude :: String -> Bool
endOfInclude statement = startswith "#include " statement && (length statement) > 11


toStatements :: String -> String -> [String]
toStatements statement code 
             | hasBreak code      = newStatement:(toStatements [] (tail code))
                                    where newStatement = reverse $ (head code):statement
toStatements statement ('#':code) = statement:(toStatements "#" code)
toStatements statement (c:code)   = toStatements (c:statement) code
toStatements statement []         = []
