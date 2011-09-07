module Read where

import SExp

import Data.Char
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Data.Maybe
import Data.List
import Debug.Trace

data ReaderMacro = RM
  { rmPattern :: String
  , rmReader :: Reader -> Reader
  }

data ParenType
  = Paren
  | Block
  | Input
  | Item
  deriving (Show, Eq)

data ReadResult
  = ReadDone SExp String
  | ReadError String
  | ReadUnexpected ParenType String
  deriving (Show, Eq)

type ReadProc a = StateT ReaderState IO a
type Reader = String -> ReadProc ReadResult

data MacroMap = MM (Map.Map Char (Either ReaderMacro MacroMap))

emptyM :: MacroMap
emptyM = MM Map.empty

insM :: ReaderMacro -> MacroMap -> Maybe MacroMap
insM rm mm = ins (rmPattern rm) mm
  where
    ins :: String -> MacroMap -> Maybe MacroMap
    ins "" _ = Nothing
    ins (c:cs) (MM mm) = case Map.lookup c mm of
      Nothing -> Just $ MM $ Map.insert c (create cs) mm
      Just (Left _) -> Nothing
      Just (Right mm') -> do
        mm'' <- ins cs mm'
        return $ MM $ Map.insert c (Right mm'') mm

    create name = foldr build (Left rm) name
      where
        build c ct = Right $ MM $ Map.insert c ct $ Map.empty

data ReaderState = RS
  { rsMacroMap :: !MacroMap
  , rsUserState :: !(Map.Map Symbol SExp)
  }

doTrace = False

xtrace :: (Show a) => String -> ReadProc a -> ReadProc a
xtrace name action
  | doTrace = do
    rsus <- gets rsUserState
    liftIO $ putStrLn $ "+" ++ name ++ "; " ++ show rsus
    v <- action
    rsus1 <- gets rsUserState
    liftIO $ putStrLn $ "-" ++ name ++ "; " ++ show rsus1
    liftIO $ putStrLn $ "  => " ++ show v
    return v
  | otherwise = action

initRS :: ReaderState
initRS = RS { rsMacroMap = defaultMM, rsUserState = Map.empty }

constituent :: Char -> Bool
constituent c = isAlpha c || isDigit c || elem c "+-*/%=<>_?!\\|&'"

monitor :: (Show a) => a -> a
monitor x = trace (show x) x

isReadingBlock :: ReadProc Bool
isReadingBlock = do
  ctxs <- getlist "context_stack"
  return $ fromMaybe False $ msum $ map f ctxs
  where
    f x
      | x == sym "block" = Just True
      | x == sym "rparen" = Just False
      | otherwise = Nothing

defaultReader :: Reader
defaultReader [] = xtrace "defaultR[]" $ do
  exitBlock
  return $ ReadUnexpected Input []
defaultReader input = xtrace "defaultR" $ do
  care <- isReadingBlock
  beg <- getv "line_beginning?"
  if beg /= NilS && care
    then readFromLine defaultReader input
    else do
      putv "line_beginning?" NilS
      readExp input

exitBlock :: ReadProc ()
exitBlock = do
  stk <- getv "context_stack"
  when (car stk == sym "block") $ do
    modifyv "context_stack" cdr
    modifyv "indent_stack" cdr

readExp :: Reader
readExp input@(c:_) = xtrace ("readExp " ++ show input) $
  if isDigit c then numReader next input else
  if constituent c then symReader next input else do
  rs <- get
  loop (rsMacroMap rs) input []
  where
    loop _ [] cur = return $ ReadError $ "eoi reached within macro sequence: " ++ reverse cur
    loop (MM partial) (c:cs) cur = case Map.lookup c partial of
      Nothing -> return $ ReadError $ "unhandled macro sequence: " ++ show (reverse (c:cur))
      Just (Left rm) -> rmReader rm next cs
      Just (Right mm) -> loop mm cs (c:cur)

    next = defaultReader

readFromLine :: Reader -> Reader
readFromLine sub input = xtrace ("readFromLine " ++ show input) $ do
  stk <- getv "context_stack"
  if car stk == sym "block"
    then do
      idts <- getlist "indent_stack"
      let cur = asStr $ head idts
      indent <- getstr "current_indent"
      case leIndent cur indent of
        True -> readItem sub input
        False -> do
          modifyv "context_stack" cdr
          modifyv "indent_stack" cdr
          sub input
    else return $ ReadUnexpected Item input

symReader :: Reader -> Reader
symReader = genReader constituent SymS

numReader :: Reader -> Reader
numReader = genReader isDigit (LitS . IntL . read)

genReader :: (Char -> Bool) -> (String -> SExp) -> Reader -> Reader
genReader pred conv _ input = case span pred input of
  (cs, rest) -> return $ ReadDone (conv cs) rest

readSeq :: Reader -> ([SExp] -> ParenType -> String -> ReadProc ReadResult) -> Reader
readSeq sub cont input = xtrace "read-seq" $ do
  r <- loop input []
  case r of
    Left c -> return $ ReadError c
    Right (es, c, rest) -> cont es c rest
  where
    loop input acc = do
      r <- sub input
      case r of
        ReadDone v rest -> loop rest (v:acc)
        ReadError c -> return $ Left c
        ReadUnexpected c rest -> return $ Right (reverse acc, c, rest)

listReader :: Reader -> ParenType -> Reader
listReader sub close input = xtrace ("listReader " ++ show close) $ withv "newline_sensitive?" NilS $ readSeq sub cont input
  where
    cont es c rest
      | c == close = return $ ReadDone (ListS es) $ tail rest 
      | otherwise = do
        s <- get
        trace (show $ rsUserState s) $ return $ ReadError $ "unexpected closing " ++ show c ++ ", expecting " ++ show close ++ "\n current input=" ++ rest


readItem :: Reader -> Reader
readItem sub input = xtrace ("readItem " ++ show input) $ do
  putv "line_beginning?" NilS
  r <- withv "newline_sensitive?" (sym "t") $
    pushToContextStackAnd (sym "item") $
    readSeq sub cont input
  return r
  where
    cont es _ rest = case es of
        [elm] -> return $ ReadDone elm rest
        _ -> return $ ReadDone (ListS es) rest

defaultMM :: MacroMap
defaultMM = case  foldM (flip insM) emptyM tbl of
  Nothing -> error "overlapping macro mapping"
  Just mm -> mm
  where
    tbl = 
      [ entry " " nop
      , entry "\t" nop
      , entry "\n" handleNewline
      , entry "(" popen
      , entry ")" pclose
      , entry ":\n" handleColonNewline
      , entry "." handleDot
      , entry "\"" handleStringLit
      , entry ";" lineComment
      ]

    nop = id
    popen next input = pushToContextStackAnd (sym "rparen") $
        listReader next Paren input
    pclose _ input = exitBlock >> return (ReadUnexpected Paren (')':input))

    entry prefix op = RM{ rmPattern = prefix, rmReader = op }

pushToContextStackAnd :: SExp -> ReadProc a -> ReadProc a
pushToContextStackAnd s action = do
  stk <- getlist "context_stack"
  withv "context_stack" (ListS $ s : stk) action

handleNewline :: Reader -> Reader
handleNewline next input = do
  ctxs <- getv "newline_sensitive?"
  if ctxs == NilS
    then next input
    else do
      indent_stack <- getv "indent_stack"
      let cur = asStr $ car indent_stack
      case (leIndent indent cur, leIndent cur indent) of
        (True, True) -> end Item
        (True, False) -> do
          end Item
        (False, True) -> next rest
        (False, False) -> return $ ReadError "incompatible indent"
  where
    (indent, rest) = splitIndent input

    end typ = do
      putv "line_beginning?" $ sym "t"
      putv "current_indent" $ str indent
      return $ ReadUnexpected typ rest

lineComment :: Reader -> Reader
lineComment next input = next $ snd $ break (=='n') input

handleDot :: Reader -> Reader
handleDot sub input = xtrace "dot" $ pushToContextStackAnd (sym "dot") $
  readSeq sub cont input
  where
    cont es _ rest = return $ ReadDone (ListS es) rest

splitIndent :: String -> (String, String)
splitIndent str = case span isIndentChar str of
  (_, '\n':next) -> splitIndent next
  a -> a
  where
    isIndentChar c = c == ' ' || c == '\t'

leIndent :: String -> String -> Bool
leIndent x y = isPrefixOf x y

handleColonNewline :: Reader -> Reader
handleColonNewline def rest = case splitIndent rest of
  (indent, code) -> do
    indent_stack <- getv "indent_stack"
    let cur = asStr $ car indent_stack
    case leIndent cur indent of
      True -> do
        putv "indent_stack" $ cons (str indent) indent_stack 
        putv "current_indent" (str indent)
        modifyv "context_stack" (cons $ sym "block")
        putv "line_beginning?" $ sym "t"
        readItem def code
      False -> endIndent def indent code

handleStringLit :: Reader -> Reader
handleStringLit def rest = case break (=='"') rest of
  (_, []) -> return $ ReadUnexpected Input rest
  (body, _:z) -> return $ ReadDone (LitS $ StringL body) z

endIndent :: Reader -> String -> Reader
endIndent _ indent input = do
  putv "line_beginning?" $ sym "t"
  putv "current_indent" $ str indent
  return $ ReadUnexpected Item input

getstr :: String -> ReadProc String
getstr key = do
  v <- getv key
  case v of
    LitS (StringL str) -> return str
    _ -> return ""

getlist :: String -> ReadProc [SExp]
getlist key = do
  v <- getv key
  case v of
    ListS v -> return v
    NilS -> return []
    _ -> error $ "getlist: not a list: " ++ show v

getv :: String -> ReadProc SExp
getv key = gets $ fromMaybe NilS . Map.lookup key . rsUserState

putv :: String -> SExp -> ReadProc ()
putv key val = do
  --when doTrace $ liftIO $ putStrLn $ "putv " ++ key ++ " <- " ++ show val
  modify $ \rs -> rs{ rsUserState = Map.insert key val $ rsUserState rs }

modifyv :: String -> (SExp -> SExp) -> ReadProc ()
modifyv key f = getv key >>= putv key . f

withv :: String -> SExp -> ReadProc a -> ReadProc a
withv key val action = do
  old <- getv key
  putv key val
  r <- action
  new <- getv key
  when (new /= val) $ fail $ "withv: temporary binding " ++ key ++ "=" ++ show val ++ " not preserved, resulting in " ++ show new
  putv key old
  return r

{-
readSexp :: String -> SExp
readSexp input = case evalState (defaultReader input) initRS of
  ReadError s -> error s
  ReadDone sexp _ -> sexp
  ReadUnexpected c _ -> error $ "unexpected ending " ++ show c
-}

readSexpIO :: String -> IO SExp
readSexpIO input = do
  r <- evalStateT (defaultReader $ removeComment input) initRS
  case r of
    ReadError s -> fail s
    ReadDone sexp _ -> return sexp
    ReadUnexpected c _ -> fail $ "unexpected ending " ++ show c

removeComment :: String -> String
removeComment = unlines . filter ((/="#") . take 1) . lines

readTest :: IO ()
readTest = do
  input <- readFile "test_input.txt"
  let source = "(defmodule nil:\n" ++ removeComment input ++ ")"
  putStrLn . showSexp =<< readSexpIO source

-- vim: sw=2 ts=2 sts=2
