
{-# LANGUAGE OverloadedStrings #-}

module Haskintex (haskintex) where

-- System
import System.Process (readProcess)
import System.FilePath
import System.Directory
import System.IO (hFlush,stdout)
-- Text
import Data.Text (pack,unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- Parser
import Text.Parsec hiding (many,(<|>))
import Text.Parsec.Text ()
-- Transformers
import Control.Monad (when,unless)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
-- LaTeX
import Text.LaTeX hiding (version)
import Text.LaTeX.Base.Syntax
-- Utils
import Control.Applicative
import Data.Foldable (foldMap)
-- Paths
import Paths_haskintex
import Data.Version (showVersion)
-- Lists
import Data.List (intersperse)
-- GHC
import Language.Haskell.Interpreter

-- Syntax

-- | The 'Syntax' datatype describes how haskintex see a LaTeX
--   file. When haskintex processes an input file, it parsers
--   to this structure. It differentiates between these parts:
--
-- * writehaskell environments (WriteHaskell), either marked
--   visible or not, located either in the header (for pragmas)
--   or in the body (for regular code).
--
-- * Haskell expression of type 'LaTeX' (InsertHaTeX).
--   See the HaTeX package for details about this type.
--
-- * Haskell expression of tyep 'IO LaTeX' (InsertHaTeXIO).
--   Exactly like InsertHaTeX, but within the IO monad.
--
-- * evalhaskell commands and environments (EvalHaskell).
--
-- * Anything else (WriteLaTeX).
--
data Syntax =
    WriteLaTeX   Text
  | WriteHaskell Bool -- Visibility: False for Hidden, True for Visible
                 Bool -- Location: True for Header, False for Body
                 Text
  | InsertHaTeX  Bool -- Memorized expression?
                 Text
  | InsertHaTeXIO Bool -- Memorized expression?
                  Text
  | EvalHaskell  Bool -- Type: False for Command, True for Environment
                 Bool -- Memorized expression?
                 Text
  | Sequence     [Syntax]
    deriving Show -- Show instance for debugging.

-- Configuration

data Conf = Conf
  { keepFlag      :: Bool
  , visibleFlag   :: Bool
  , verboseFlag   :: Bool
  , manualFlag    :: Bool
  , helpFlag      :: Bool
  , lhs2texFlag   :: Bool
  , stdoutFlag    :: Bool
  , overwriteFlag :: Bool
  , debugFlag     :: Bool
  , memoFlag      :: Bool
  , unknownFlags  :: [String]
  , inputs        :: [FilePath]
    }

supportedFlags :: [(String,Conf -> Bool)]
supportedFlags =
  [ ("keep"      , keepFlag)
  , ("visible"   , visibleFlag)
  , ("verbose"   , verboseFlag)
  , ("manual"    , manualFlag)
  , ("help"      , helpFlag)
  , ("lhs2tex"   , lhs2texFlag)
  , ("stdout"    , stdoutFlag)
  , ("overwrite" , overwriteFlag)
  , ("debug"     , debugFlag)
  , ("memo"      , memoFlag)
    ]

readConf :: [String] -> Conf
readConf = go $ Conf False False False False False False False False False False [] []
  where
    go c [] = c
    go c (x:xs) =
       case x of
        -- Arguments starting with '-' are considered a flag.
        ('-':flag) ->
           case flag of
             "keep"      -> go (c {keepFlag      = True}) xs
             "visible"   -> go (c {visibleFlag   = True}) xs
             "verbose"   -> go (c {verboseFlag   = True}) xs
             "manual"    -> go (c {manualFlag    = True}) xs
             "help"      -> go (c {helpFlag      = True}) xs
             "lhs2tex"   -> go (c {lhs2texFlag   = True}) xs
             "stdout"    -> go (c {stdoutFlag    = True}) xs
             "overwrite" -> go (c {overwriteFlag = True}) xs
             "debug"     -> go (c {debugFlag     = True}) xs
             "memo"      -> go (c {memoFlag      = True}) xs
             _           -> go (c {unknownFlags = unknownFlags c ++ [flag]}) xs
        -- Otherwise, an input file.
        _ -> go (c {inputs = inputs c ++ [x]}) xs

-- Haskintex monad

type Haskintex = ReaderT Conf IO

outputStr :: String -> Haskintex ()
outputStr str = do
  b <- verboseFlag <$> ask
  when b $ lift $ putStrLn str

-- PARSING

type Parser = ParsecT Text () Haskintex

parseSyntax :: Parser Syntax
parseSyntax = do
  s <- fmap Sequence $ many $ choice [ p_writehaskell, p_inserthatex False , p_inserthatex True , p_evalhaskell, p_writelatex ]
  eof
  return s

p_writehaskell :: Parser Syntax
p_writehaskell = do
  isH <- (try $ string "\\begin{writehaskell}" >> return False)
           <|> (try $ string "\\begin{haskellpragmas}" >> return True)
  b <- choice $ fmap try [ string "[hidden]"  >> return False
                         , string "[visible]" >> return True
                         , lift $ visibleFlag <$> ask ] -- When no option is given, take the default.
  h <- manyTill anyChar $ try $ string $ if isH then "\\end{haskellpragmas}" else "\\end{writehaskell}"
  return $ WriteHaskell b isH $ pack h

readMemo :: Parser Bool
readMemo = (char '[' *> choice xs <* char ']') <|> lift (memoFlag <$> ask)
  where
    xs = [ string "memo" >> return True
         , string "notmemo" >> return False ]

p_inserthatex :: Bool -- False for pure, True for IO
              -> Parser Syntax
p_inserthatex isIO = do
  --
  let iden = if isIO then "iohatex" else "hatex"
      cons = if isIO then InsertHaTeXIO else InsertHaTeX
  --
  _ <- try $ string $ '\\' : iden
  b <- readMemo
  char '{'
  h <- p_haskell 0
  return $ cons b $ pack h

p_evalhaskell :: Parser Syntax
p_evalhaskell = choice [ p_evalhaskellenv, p_evalhaskellcomm ]

p_evalhaskellenv :: Parser Syntax
p_evalhaskellenv = do
  _ <- try $ string "\\begin{evalhaskell}"
  b <- readMemo
  h <- manyTill anyChar $ try $ string "\\end{evalhaskell}"
  return $ EvalHaskell True b $ pack h

p_evalhaskellcomm :: Parser Syntax
p_evalhaskellcomm = do
  _  <- try $ string "\\evalhaskell"
  b <- readMemo
  char '{'
  h  <- p_haskell 0
  return $ EvalHaskell False b $ pack h

p_haskell :: Int -> Parser String
p_haskell n = choice [
    do _ <- char '{'
       ('{':) <$> p_haskell (n+1)
  , do _ <- char '}'
       if n == 0
          then return []
          else ('}':) <$> p_haskell (n-1)
  , do _ <- char '\"'
       liftA2 (++) (('\"':) <$> p_string) (p_haskell n)
  , try (string "'{'") >> return "'{'"
  , try (string "'}'") >> return "'}'"
  , liftA2 (:) anyChar (p_haskell n)
    ]

p_string :: Parser String
p_string = choice [
    try $ liftA2 (++) (char '\\' >> char '\"' >> return "\\\"") p_string
  , liftA2 (:) (char '\"') (return [])
  , liftA2 (:) anyChar p_string
    ]

p_writelatex :: Parser Syntax
p_writelatex = (WriteLaTeX . pack) <$>
  many1 (p_other >>= \b -> if b then anyChar else fail "stop write latex")
  where
    p_other =
      choice $ fmap (try . lookAhead)
             [ string "\\begin{writehaskell}"   >> return False -- starts p_writehaskell (for body)
             , string "\\begin{haskellpragmas}" >> return False -- starts p_writehaskell (for header)
             , string "\\hatex"                 >> return False -- starts p_inserthatex
             , string "\\iohatex"               >> return False -- starts p_inserthatexio
             , string "\\begin{evalhaskell}"    >> return False -- starts p_evalhaskellenv
             , string "\\evalhaskell"           >> return False -- starts p_evalhaskellcomm
             , return True
             ]

-- PASS 1: Extract code from processed Syntax.

extractCode :: Syntax -> (Text,Text)
extractCode (WriteHaskell _ isH t) = if isH then (t,mempty) else (mempty,t)
extractCode (Sequence xs) = foldMap extractCode xs
extractCode _ = mempty

-- PASS 2: Evaluate Haskell expressions from processed Syntax.

evalCode :: String -- ^ Auxiliary module name
         -> Bool   -- ^ Is manual flag on?
         -> Bool   -- ^ Is lhs2tex flag on?
         -> Syntax -> Haskintex Text
evalCode modName mFlag lhsFlag = go
  where
    go (WriteLaTeX t) = return t
    go (WriteHaskell b _ t) =
         let f :: Text -> LaTeX
             f x | not b = mempty
                 | mFlag = raw x
                 | lhsFlag = TeXEnv "code" [] $ raw x
                 | otherwise = verbatim x
         in return $ render $ f t
    go (InsertHaTeX isMemo t) = do
         let e = unpack $ T.strip t
             int = do
               loadModules [modName]
               setTopLevelModules [modName]
               setImports ["Prelude"]
               interpret e (as :: LaTeX)
         outputStr $ "Evaluation (LaTeX): " ++ e
         r <- runInterpreter int
         case r of
           Left err -> do
             outputStr $ "Warning: Error while evaluating the expression.\n"
               ++ errorString err
             return mempty
           Right l -> return $ render l
    go (InsertHaTeXIO isMemo t) = do
         let e = unpack $ T.strip t
             int = do
               loadModules [modName]
               setTopLevelModules [modName]
               setImports ["Prelude"]
               interpret e (as :: IO LaTeX)
         outputStr $ "Evaluation (IO LaTeX): " ++ e
         r <- runInterpreter int
         case r of
           Left err -> do
             outputStr $ "Warning: Error while evaluating the expression.\n"
               ++ errorString err
             return mempty
           Right l -> liftIO $ render <$> l
    go (EvalHaskell env isMemo t) =
         let f :: Text -> LaTeX
             f x | mFlag = raw x -- Manual flag overrides lhs2tex flag behavior
                 | env && lhsFlag = TeXEnv "code" [] $ raw x
                 | lhsFlag = raw $ "|" <> x <> "|"
                 | env = verbatim $ layout x
                 | otherwise = verb x
         in (render . f . pack) <$> ghc modName t
    go (Sequence xs) = mconcat <$> mapM go xs

ghc :: String -> Text -> Haskintex String
ghc modName e = do
  let e' = unpack $ T.strip e
  outputStr $ "Evaluation: " ++ e'
  lift $ init <$> readProcess "ghc" 
     -- Disable reading of .ghci files.
     [ "-ignore-dot-ghci"
     -- Evaluation loading the temporal module.
     -- The expression is stripped.
     , "-e", e', modName ++ ".hs"
       ] []

maxLineLength :: Int
maxLineLength = 60

-- | Break lines longer than 'maxLineLenght'.
layout :: Text -> Text
layout = T.unlines . go . T.lines
  where
    go [] = []
    go (t:ts) =
      if T.length t > maxLineLength
         then let (l,r) = T.splitAt maxLineLength t
              in  l : go (r:ts)
         else t : go ts

-- Errors

errorString :: InterpreterError -> String
errorString (UnknownError e) = "Unknown error: " ++ e
errorString (WontCompile es) = "Compiler error:\n" ++ init (unlines $ fmap errMsg es)
errorString (NotAllowed e) = "Not allowed:" ++ e
errorString (GhcException e) = "GHC exception: " ++ e

-- Haskintex main function

-- | Run haskintex with the given arguments. For example:
--
-- > haskintex ["-visible","-overwrite","foo.htex"]
--
--   Useful if you want to call /haskintex/ from another program.
--   This function does /not/ do any system call.
haskintex :: [String] -> IO ()
haskintex = runReaderT haskintexmain . readConf

haskintexmain :: Haskintex ()
haskintexmain = do
  flags <- ask
  if -- If the help flag is passed, ignore everything else
     -- and just print the help.
     helpFlag flags
     then lift $ putStr help
     else let xs = inputs flags
          in  if null xs
                 then lift $ putStr noFiles
                 else mapM_ haskintexFile xs

commas :: [String] -> String
commas = concat . intersperse ", "

showEnabledFlags :: Haskintex ()
showEnabledFlags = do
  c <- ask
  outputStr $ "Enabled flags: "
           ++ commas (foldr (\(str,f) xs -> if f c then str : xs else xs) [] supportedFlags)
           ++ "."

reportWarnings :: Haskintex ()
reportWarnings = do
  -- Combination of manual and lhs2tex flags.
  manFlag <- manualFlag  <$> ask
  lhsFlag <- lhs2texFlag <$> ask
  when (manFlag && lhsFlag) $
    outputStr "Warning: lhs2tex flag is useless in presence of manual flag."

haskintexFile :: FilePath -> Haskintex ()
haskintexFile fp_ = do
  -- If the given file does not exist, try adding '.htex'.
  b <- lift $ doesFileExist fp_
  let fp = if b then fp_ else fp_ ++ ".htex"
  -- Report enabled flags
  showEnabledFlags
  -- Warnings
  reportWarnings
  -- Other unknown flags passed.
  uFlags <- unknownFlags <$> ask
  unless (null uFlags) $
    outputStr $ "Unsupported flags: " ++ commas uFlags ++ "."
  -- File parsing.
  outputStr $ "Reading " ++ fp ++ "..."
  vFlag <- visibleFlag <$> ask
  t <- lift $ T.readFile fp
  pres <- runParserT parseSyntax () fp t
  case pres of
    Left err -> outputStr $ "Reading of " ++ fp ++ " failed:\n" ++ show err
    Right s -> do
      -- Zero pass: In case of debugging, write down the parsed AST.
      dbugFlag <- debugFlag <$> ask
      when dbugFlag $ do
        let debugfp = dropExtension (takeFileName fp) ++ ".debughtex"
        outputStr $ "Writing file " ++ debugfp ++ " with debugging output..."
        lift $ writeFile debugfp $ show s
      -- First pass: Create haskell source from the code obtained with 'extractCode'.
      let modName = ("Haskintex_" ++) $ dropExtension $ takeFileName fp
      outputStr $ "Creating Haskell source file " ++ modName ++ ".hs..."
      let (hsH,hs) = extractCode s
          moduleHeader = pack $ "\nmodule " ++ modName ++ " where\n\n"
      lift $ T.writeFile (modName ++ ".hs") $ hsH <> moduleHeader <> hs
      -- Second pass: Evaluate expressions using 'evalCode'.
      outputStr $ "Evaluating expressions in " ++ fp ++ "..."
      mFlag <- manualFlag <$> ask
      lhsFlag <- lhs2texFlag <$> ask
      l <- evalCode modName mFlag lhsFlag s
      -- Write final output.
      let fp' = dropExtension (takeFileName fp) ++ ".tex"
          writeit = do outputStr $ "Writing final file at " ++ fp' ++ "..."
                       lift $ T.writeFile fp' l
      outFlag <- stdoutFlag <$> ask
      overFlag <- overwriteFlag <$> ask
      nonew <- lift $ doesFileExist fp'
      let finalOutput
           | outFlag = do outputStr "Sending final output to stdout..."
                          lift $ T.putStr l
           | overFlag = writeit
           | nonew = do lift $ putStr $ "File " ++ fp' ++ " already exists. Overwrite?"
                                     ++ " (use -overwrite to overwrite by default) "
                        lift $ hFlush stdout -- To immediately show the text on Windows systems.
                        resp <- lift getLine
                        if resp `elem` ["","y","yes"]
                           then writeit
                           else outputStr "No file was written."
           | otherwise = writeit
      finalOutput
      -- If the keep flag is not set, remove the haskell source file.
      kFlag <- keepFlag <$> ask
      unless kFlag $ do
        outputStr $ "Removing Haskell source file " ++ modName ++ ".hs "
                  ++ "(use -keep to avoid this)..."
        lift $ removeFile $ modName ++ ".hs"
      -- End.
      outputStr $ "End of processing of file " ++ fp ++ "."

-- MESSAGES

help :: String
help = unlines [
    "You are using haskintex version " ++ showVersion version ++ "."
  , "http://daniel-diaz.github.io/projects/haskintex"
  , ""
  , "Usage and flags:"
  , "Any argument passed to haskintex that starts with '-' will be considered"
  , "a flag. Otherwise, it will be considered an input file. Every input file"
  , "will be processed with the same set of flags, which will include all the"
  , "flags passed in the call. This is the list of flags supported by haskintex:"
  , ""
  , "  -keep       haskintex creates an intermmediate Haskell file before"
  , "              evaluating any expressions. By default, this file is "
  , "              eliminated after processing the file. Pass this flag to"
  , "              keep the file."
  , ""
  , "  -visible    By default, code written inside a writehaskell environment"
  , "              is not shown in the LaTeX output. This flag changes the"
  , "              default."
  , ""
  , "  -verbose    If this flag is enabled, haskintex will print information"
  , "              about its own execution while running."
  , ""
  , "  -manual     By default, Haskell expressions, either from writehaskell "
  , "              or evalhaskell, appear in the LaTeX output inside verb or"
  , "              verbatim declarations. If this flag is passed, neither verb"
  , "              nor verbatim will be used. The code will be written as text "
  , "              as it is. The user will decide how to handle it."
  , ""
  , "  -help       This flags cancels any other flag or input file and makes"
  , "              the program simply show this help message."
  , ""
  , "  -stdout     Instead of writing the output to a file, send it to the"
  , "              standard output stream (stdout)."
  , ""
  , "  -lhs2tex    Instead of using verb or verbatim declarations, format the"
  , "              output using the syntax accepted by lhs2TeX."
  , ""
  , "  -overwrite  Overwrite the output file if it already exists. If this flag"
  , "              is not set, the program will ask before overwriting."
  , ""
  , "  -debug      Only for debugging purposes. It writes a file with extension"
  , "              .debughtex with the AST of the internal representation of the"
  , "              input file haskintex uses."
  , ""
  , "Any unsupported flag will be ignored."
  ]

noFiles :: String
noFiles = "No input file given.\n"
