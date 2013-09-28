
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- System
import System.Process (readProcess)
import System.Environment (getArgs)
import System.FilePath
import System.Directory
import System.IO (hFlush,stdout)
-- Text
import Data.Text (pack,unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- Parser
import Data.Attoparsec.Text
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
--   visible or not.
--
-- * Haskell expression of type 'LaTeX' (InsertHaTeX).
--   See the HaTeX package for details about this type.
--
-- * evalhaskell commands and environments (EvalHaskell).
--
-- * Anything else (WriteLaTeX).
--
data Syntax =
    WriteLaTeX   Text
  | WriteHaskell Bool Text -- False for Hidden, True for Visible
  | InsertHaTeX  Text
  | EvalHaskell  Bool Text -- False for Command, True for Environment
  | Sequence     [Syntax]
    deriving Show -- Show instance for debugging.

-- Parsing

parseSyntax :: Bool -> Parser Syntax
parseSyntax v = fmap Sequence $ many $ choice [ p_writehaskell v, p_inserthatex, p_evalhaskell, p_writelatex ]

p_writehaskell :: Bool -> Parser Syntax
p_writehaskell v = do
  _ <- string "\\begin{writehaskell}"
  b <- choice [ string "[hidden]"  >> return False
              , string "[visible]" >> return True
              , return v ] -- When no option is given, take the default.
  h <- manyTill anyChar $ string "\\end{writehaskell}"
  return $ WriteHaskell b $ pack h

p_inserthatex :: Parser Syntax
p_inserthatex = do
  _ <- string "\\hatex{"
  h <- p_haskell 0
  return $ InsertHaTeX $ pack h

p_evalhaskell :: Parser Syntax
p_evalhaskell = choice [ p_evalhaskellenv, p_evalhaskellcomm ]

p_evalhaskellenv :: Parser Syntax
p_evalhaskellenv = do
  _ <- string "\\begin{evalhaskell}"
  h <- manyTill anyChar $ string "\\end{evalhaskell}"
  return $ EvalHaskell True $ pack h

p_evalhaskellcomm :: Parser Syntax
p_evalhaskellcomm = do
  _  <- string "\\evalhaskell{"
  h  <- p_haskell 0
  return $ EvalHaskell False $ pack h

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
  , liftA2 (:) anyChar (p_haskell n)
    ]

p_string :: Parser String
p_string = choice [
    liftA2 (++) (char '\\' >> char '\"' >> return "\\\"") p_string
  , liftA2 (:) (char '\"') (return [])
  , liftA2 (:) anyChar p_string
    ]  

p_writelatex :: Parser Syntax
p_writelatex = (WriteLaTeX . pack) <$>
  many1 (p_other >>= \b -> if b then anyChar else fail "stop write latex")
  where
    p_other =
      choice [ string "\\begin{writehaskell}" >> return False -- starts p_writehaskell
             , string "\\hatex"               >> return False -- starts p_inserthatex
             , string "\\begin{evalhaskell}"  >> return False -- starts p_evalhaskellenv
             , string "\\evalhaskell"         >> return False -- starts p_evalhaskellcomm
             , return True
             ]

-- PASS 1: Extract code from processed Syntax.

extractCode :: Syntax -> Text
extractCode (WriteHaskell _ t) = t
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
    go (WriteHaskell b t) =
         let f :: Text -> LaTeX
             f x | not b = mempty
                 | mFlag = raw x
                 | lhsFlag = TeXEnv "code" [] $ raw x
                 | otherwise = verbatim x
         in return $ render $ f t
    go (InsertHaTeX t) = do
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
             outputStr $ "Warning: Expression '" ++ e ++ "' thrown the following error:\n"
               ++ show err ++ "\n"
             return mempty
           Right l -> return $ render l
    go (EvalHaskell env t) =
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
  lift $ init <$> readProcess "ghc" [ "-e", e', modName ++ ".hs" ] []

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
    ]

readConf :: [String] -> Conf
readConf = go $ Conf False False False False False False False False [] []
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
             _           -> go (c {unknownFlags = unknownFlags c ++ [flag]}) xs
        -- Otherwise, an input file.
        _ -> go (c {inputs = inputs c ++ [x]}) xs

-- Haskintex

type Haskintex = ReaderT Conf IO

outputStr :: String -> Haskintex ()
outputStr str = do
  b <- verboseFlag <$> ask
  when b $ lift $ putStrLn str

-- MAIN

main :: IO ()
main = (readConf <$> getArgs) >>= runReaderT haskintex

haskintex :: Haskintex ()
haskintex = do
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
  case parseOnly (parseSyntax vFlag) t of
    Left err -> outputStr $ "Reading of " ++ fp ++ " failed: " ++ err
    Right s -> do
      -- First pass: Create haskell source from the code obtained with 'extractCode'.
      let modName = ("Haskintex_" ++) $ dropExtension $ takeFileName fp
      outputStr $ "Creating Haskell source file " ++ modName ++ ".hs..."
      let hs = extractCode s
          moduleHeader = pack $ "module " ++ modName ++ " where\n\n"
      lift $ T.writeFile (modName ++ ".hs") $ moduleHeader <> hs
      -- Second pass: Evaluate expressions using 'evalCode'.
      outputStr $ "Evaluating expressions in " ++ fp ++ "..."
      mFlag <- manualFlag <$> ask
      lhsFlag <- lhs2texFlag <$> ask
      l <- evalCode modName mFlag lhsFlag s
      -- Write final output.
      outFlag <- stdoutFlag <$> ask
      if outFlag
         then do outputStr "Sending final output to stdout..."
                 lift $ T.putStr l
         else do let fp' = dropExtension (takeFileName fp) ++ ".tex"
                     writeIt = do outputStr $ "Writing final file at " ++ fp' ++ "..."
                                  lift $ T.writeFile fp' l
                 overFlag <- overwriteFlag <$> ask
                 if overFlag
                    then writeIt
                    else do nonew <- lift $ doesFileExist fp' 
                            if nonew
                               then do lift $ putStr $ "File " ++ fp' ++ " already exists. Overwrite? (use -overwrite to overwrite by default) "
                                       lift $ hFlush stdout
                                       resp <- lift getLine
                                       if resp `elem` ["","y","yes"]
                                          then writeIt
                                          else outputStr "No file was written."
                               else writeIt
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
  , "  -keep     haskintex creates an intermmediate Haskell file before"
  , "            evaluating any expressions. By default, this file is "
  , "            eliminated after processing the file. Pass this flag to"
  , "            keep the file."
  , ""
  , "  -visible  By default, code written inside a writehaskell environment"
  , "            is not shown in the LaTeX output. This flag changes the"
  , "            default."
  , ""
  , "  -verbose  If this flag is enabled, haskintex will print information"
  , "            about its own execution while running."
  , ""
  , "  -manual   By default, Haskell expressions, either from writehaskell "
  , "            or evalhaskell, appear in the LaTeX output inside verb or"
  , "            verbatim declarations. If this flag is passed, neither verb"
  , "            nor verbatim will be used. The code will be written as text "
  , "            as it is. The user will decide how to handle it."
  , ""
  , "  -help     This flags cancels any other flag or input file and makes"
  , "            the program simply show this help message."
  , ""
  , "  -stdout   Instead of writing the output to a file, send it to the"
  , "            standard output stream (stdout)."
  , ""
  , "  -lhs2tex  Instead of using verb or verbatim declarations, format the"
  , "            output using the syntax accepted by lhs2TeX."
  , ""
  , "Any unsupported flag will be ignored."
  ]

noFiles :: String
noFiles = "No input file given.\n"
