import qualified Data.Map as M

import SExp
import Read
import Parse
import Syntax
import Inference


main = do
    txt <- getContents
    sexp <- readSexpIO txt
    putStrLn "++++ sexp:" 
    print sexp

    parsed <- either fail return $ parseModule sexp
    putStrLn "++++ parsed:"
    print parsed

    checked <- runTypecheck $ typecheckModule M.empty parsed
    putStrLn "++++ type-checked:"
    print checked
