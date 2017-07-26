
{-# LANGUAGE OverloadedStrings, CPP #-}

module Haskintex (haskintex) where

-- System
import System.Process (readProcess, readCreateProcess, shell)
import System.FilePath
import System.Directory
import System.IO (hFlush,stdout)
-- Text
import Data.Text (pack,unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding
-- Parser
import Text.Parsec hiding (many,(<|>))
import Text.Parsec.Text ()
-- Transformers
import Control.Monad (when,unless,replicateM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
-- LaTeX
import Text.LaTeX hiding (version)
import qualified Text.LaTeX as Hatex
import Text.LaTeX.Base.Syntax
-- Utils
import Control.Applicative
#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (foldMap)
#endif
import Numeric (showFFloat)
-- Paths
import Paths_haskintex (version)
import Data.Version (showVersion)
-- Lists
import Data.List (intersperse, isSuffixOf)
-- GHC
import Language.Haskell.Interpreter hiding (get)
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import Data.Typeable
import qualified Language.Haskell.Exts.Pretty as H
import qualified Language.Haskell.Exts.Parser as H
import qualified Language.Haskell.Exts.Syntax as H
-- Map
import qualified Data.Map as M
-- Binary
import Data.Binary.Put
import Data.Binary.Get hiding (lookAhead)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB

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

-- | Possible sources of package DBs. The value of the type is constructed from
-- CLI argument and tells haskintex which strategy to use for package DB selection.
--
-- If no CLI argument is presented, haskintex tries to guess from local environment
-- which package DB to use.
data PackageDB =
    CabalSandboxDB -- ^ Pick package-db from `.cabal-sandbox` folder
  | StackDB -- ^ Pick package-db from `stack path`
  deriving Show

-- | True if the input value is cabal sandbox package-db
isCabalSandboxDB :: PackageDB -> Bool
isCabalSandboxDB v = case v of
  CabalSandboxDB -> True
  _ -> False

-- | True if the input value is stack package-db
isStackDB :: PackageDB -> Bool
isStackDB v = case v of
  StackDB -> True
  _ -> False

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
  , memocleanFlag :: Bool
  , autotexyFlag  :: Bool
  , nosandboxFlag :: Bool
  , packageDb     :: Maybe PackageDB
  , werrorFlag    :: Bool
  , unknownFlags  :: [String]
  , inputs        :: [FilePath]
  , memoTree      :: MemoTree
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
  , ("memoclean" , memocleanFlag)
  , ("autotexy"  , autotexyFlag)
  , ("nosandbox" , nosandboxFlag)
  , ("cabaldb"   , maybe False isCabalSandboxDB . packageDb)
  , ("stackdb"   , maybe False isStackDB . packageDb)
  , ("werror"    , werrorFlag)
    ]

readConf :: [String] -> Conf
readConf = go $ Conf False False False False False False False False False False False False False Nothing False [] [] M.empty
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
             "memoclean" -> go (c {memocleanFlag = True}) xs
             "autotexy"  -> go (c {autotexyFlag  = True}) xs
             "nosandbox" -> go (c {nosandboxFlag = True}) xs
             "cabaldb"   -> go (c {packageDb     = Just CabalSandboxDB}) xs
             "stackdb"   -> go (c {packageDb     = Just StackDB}) xs
             "werror"    -> go (c {werrorFlag    = True}) xs
             _           -> go (c {unknownFlags = unknownFlags c ++ [flag]}) xs
        -- Otherwise, an input file.
        _ -> go (c {inputs = inputs c ++ [x]}) xs

-- Haskintex monad

type Haskintex = StateT Conf IO

outputStr :: String -> Haskintex ()
outputStr str = do
  b <- verboseFlag <$> get
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
                         , lift $ visibleFlag <$> get ] -- When no option is given, take the default.
  h <- manyTill anyChar $ try $ string $ if isH then "\\end{haskellpragmas}" else "\\end{writehaskell}"
  return $ WriteHaskell b isH $ pack h

readMemo :: Parser Bool
readMemo = (char '[' *> choice xs <* char ']') <|> lift (memoFlag <$> get)
  where
    xs = [ string "memo" >> return True
         , string "notmemo" >> return False ]

processExp :: (H.Exp () -> H.Exp ()) -- ^ Transformation to apply to Haskell Expression
           -> Text -- ^ Haskell expression
           -> Parser Text
processExp f t = do
  return $ case H.parseExp (unpack t) of
    H.ParseOk e -> pack $ H.prettyPrint $ f $ const () <$> e
    _ -> t

p_inserthatex :: Bool -- False for pure, True for IO
              -> Parser Syntax
p_inserthatex isIO = do
  --
  let iden = if isIO then "iohatex" else "hatex"
      cons = if isIO then InsertHaTeXIO else InsertHaTeX
  --
  _ <- try $ string $ '\\' : iden
  b <- readMemo
  _ <- char '{'
  h <- p_haskell 0
  auto <- lift $ autotexyFlag <$> get
  let v = H.Var () . H.UnQual () . H.Ident ()
      f = if auto then H.App () $ if isIO then H.App () (v "fmap") (v "texy")
                                          else v "texy"
                  else id
  cons b <$> processExp f (pack h)

p_evalhaskell :: Parser Syntax
p_evalhaskell = choice [ p_evalhaskellenv, p_evalhaskellcomm ]

p_evalhaskellenv :: Parser Syntax
p_evalhaskellenv = do
  _ <- try $ string "\\begin{evalhaskell}"
  b <- readMemo
  h <- manyTill anyChar $ try $ string "\\end{evalhaskell}"
  EvalHaskell True b <$> processExp id (pack h)

p_evalhaskellcomm :: Parser Syntax
p_evalhaskellcomm = do
  _  <- try $ string "\\evalhaskell"
  b <- readMemo
  _ <- char '{'
  h  <- p_haskell 0
  EvalHaskell False b <$> processExp id (pack h)

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

----------------------------------------------------------
----------------------------------------------------------
-- MEMO TREE

-- | A 'MemoTree' maps each expression to its reduced form.
type MemoTree = M.Map Text Text

-- | Search in current directory and all parents dirs for given file.
-- Return 'True' if found such file, 'False' either way. Search is performed
-- until root folder is not hit.
doesFileExistsUp :: FilePath -> IO Bool
doesFileExistsUp fname = do
  parents <- getAllParents <$> getCurrentDirectory
  checks <- mapM (doesFileExist . (</> fname)) parents
  pure $ or checks

-- | Generate list of all parents of the given path including the path itself.
getAllParents :: FilePath -> [FilePath]
getAllParents = go . reverse . splitDirectories
  where
    go [] = []
    go segs = let
      parent = joinPath $ reverse segs
      in parent : go (drop 1 segs)

-- | Try to detect cabal sandbox or stack project and get pathes to package DBs.
--
-- If ambigous situation is presented (both stack and cabal sandbox is found),
-- then fail with descriptive message.
autoDetectSandbox :: Haskintex (Maybe [String])
autoDetectSandbox = do
  noSandbox <- nosandboxFlag <$> get
  if noSandbox
     then do
       outputStr "Ignoring sandbox."
       pure Nothing
     else do inSandbox <- lift $ doesDirectoryExist ".cabal-sandbox"
             hasStackFile <- lift $ doesFileExistsUp "stack.yaml"
             case (inSandbox, hasStackFile) of
               (True, False) -> do
                 outputStr "Detected cabal sandbox."
                 loadCabalSandboxDBPaths
               (False, True) -> do
                 outputStr "Detected stack sandbox."
                 loadStackDBPaths
               (True, True) -> fail $ "Found both cabal sandbox and stack project. Please, specify which package DB to use with either "
                 ++ " '-cabaldb' or '-stackdb' flags."
               (False, False) -> do
                 outputStr "No sandbox or stack project detected."
                 pure Nothing

-- | Generate CLI arguments for GHC for package DB using cabal sandbox
loadCabalSandboxDBPaths :: Haskintex (Maybe [String])
loadCabalSandboxDBPaths = do
  outputStr "Using cabal sandbox for package db"
  sand <- lift $ getDirectoryContents ".cabal-sandbox"
  let pkgdbs = filter (isSuffixOf "packages.conf.d") sand
  case pkgdbs of
    pkgdb : _ -> do
      outputStr $ "Using sandbox package db: " ++ pkgdb
      pure . Just $ [".cabal-sandbox/" ++ pkgdb]
    _ -> do
      outputStr "Don't use sandbox. Empty .cabal-sandbox"
      pure Nothing

-- | Generate CLI arguments for GHC for package DB using stack environment
loadStackDBPaths :: Haskintex (Maybe [String])
loadStackDBPaths = do
  outputStr "Using stack environment for package db"
  let getDBPath s = fmap (filter (/= '\n')) . lift $ readCreateProcess (shell $ "stack path --" ++ s) ""
  pkgdbSnapshot <- getDBPath "snapshot-pkg-db"
  pkgdbGlobal <- getDBPath "global-pkg-db"
  pkgdbLocal <- getDBPath "local-pkg-db"
  outputStr $ "Using sandbox package db: \n" ++ unlines [pkgdbSnapshot, pkgdbGlobal, pkgdbLocal]
  pure . Just $ [pkgdbSnapshot, pkgdbGlobal, pkgdbLocal]

-- | Try to detect cabal sandbox and use stack's ones if user specifies the 'stackdb' flag.
getSandbox :: Haskintex (Maybe [String])
getSandbox = do
  pkgDbConf <- packageDb <$> get
  case pkgDbConf of
    Nothing -> autoDetectSandbox
    Just CabalSandboxDB -> loadCabalSandboxDBPaths
    Just StackDB -> loadStackDBPaths

memoreduce :: Typeable t
           => String -- ^ Auxiliar module name
           -> Bool -- ^ Is this expression memorized?
           -> Text -- ^ Input
           -> t -- ^ Type
           -> (t -> Haskintex Text) -- ^ Rendering function
           -> Haskintex Text
memoreduce modName isMemo t ty f = do
  let e = unpack t
  outputStr $ "Evaluation (" ++ showsTypeRep (typeRep $ Just ty) "" ++ "): " ++ e
  memt <- memoTree <$> get
  let p = if isMemo then M.lookup t memt else Nothing
  case p of
    Nothing -> do
      let int = do
             loadModules [modName]
             setTopLevelModules [modName]
             setImports ["Prelude"]
             interpret e ty
      -- Sandbox recognition and executing interpreter
      r <- maybe (runInterpreter int) (\pkgdbs -> unsafeRunInterpreterWithArgs (("-package-db " ++) <$> pkgdbs) int) =<< getSandbox
      case r of
        Left err -> do
          shouldFail <- werrorFlag <$> get
          if shouldFail
            then fail $ "Error: failed while evaluating the expression: \n"
                   ++ errorString err
            else outputStr $ "Warning: Error while evaluating the expression.\n"
                   ++ errorString err
          return mempty
        Right x -> do
          -- Render result
          t' <- f x
          -- If the expression is marked to be memorized, store it in the 'MemoTree'.
          when isMemo $ do
            modify $ \st -> st { memoTree = M.insert t t' $ memoTree st }
            outputStr $ "-> Result has been memorized."
          -- Return result
          return t'
    Just o -> do
      outputStr "-> Result of the evaluation recovered from memo tree."
      return o

{- Memo Tree Format

A memo tree is stored as a list of (key,value) in key ascending order.
Keys and values are encoded in UTF-8.

| offset |   description        | size (in bytes) |
---------------------------------------------------
| 00     | Number of blocks     | 2               |
| 02     | Zero or more blocks  | variable        |

Each block has the following structure:

| offset |   description        | size (in bytes) |
---------------------------------------------------
| 00     | Length of key (k)    | 2               |
| 02     | Length of value (v)  | 2               |
| 04     | Key                  | k               |
| 04+k   | Value                | v               |

-}

memoTreeToBinary :: MemoTree -> ByteString
memoTreeToBinary memt = runPut $ do
  putWord16le $ fromIntegral $ M.size memt
  mapM_ (\(t,t') -> do
    let b  = encodeUtf8 t
        b' = encodeUtf8 t'
    putWord16le $ fromIntegral $ SB.length b
    putWord16le $ fromIntegral $ SB.length b'
    putByteString b
    putByteString b'
    ) $ M.toAscList memt

memoTreeFromBinary :: ByteString -> Either String MemoTree
memoTreeFromBinary b =
  case runGetOrFail getMemoTree b of
    Left (_,_,err) -> Left err
    Right (_,_,memt) -> Right memt

getMemoTree :: Get MemoTree
getMemoTree = do
  n <- fromIntegral <$> getWord16le
  fmap M.fromAscList $ replicateM n $ do
    l  <- getWord16le
    l' <- getWord16le
    b  <- getByteString $ fromIntegral l
    b' <- getByteString $ fromIntegral l'
    return (decodeUtf8 b, decodeUtf8 b')

memoTreeOpen :: Haskintex ()
memoTreeOpen = do
  d <- liftIO $ getAppUserDataDirectory "haskintex"
  let fp = d </> "memotree"
  b <- liftIO $ doesFileExist fp
  if b then do t <- liftIO $ LB.readFile fp
               case memoTreeFromBinary t of
                 Left err -> do
                   outputStr $ "Error: memotree failed to read: " ++ err
                   outputStr "-> Using empty memotree."
                   modify $ \st -> st { memoTree = M.empty }
                 Right memt -> do
                   modify $ \st -> st { memoTree = memt }
                   let n = LB.length t
                       kbs :: Double
                       kbs = fromIntegral n / 1024
                       s = if kbs < 1 then show n ++ " Bs"
                                      else showFFloat (Just 2) kbs " KBs"
                   outputStr $ "Info: memotree loaded (" ++ s ++ ")."
       else do outputStr "Info: memotree does not exist."
               outputStr "-> Using empty memotree."
               modify $ \st -> st { memoTree = M.empty }

memoTreeSave :: Haskintex ()
memoTreeSave = do
  memt <- memoTree <$> get
  unless (M.null memt) $ do
    outputStr "Saving memotree..."
    liftIO $ do
      d <- getAppUserDataDirectory "haskintex"
      createDirectoryIfMissing True d
      let fp = d </> "memotree"
      LB.writeFile fp $ memoTreeToBinary memt
    outputStr "Info: memotree saved."

memoTreeClean :: Haskintex ()
memoTreeClean = do
  d <- liftIO $ getAppUserDataDirectory "haskintex"
  let fp = d </> "memotree"
  b <- liftIO $ doesFileExist fp
  when b $ do
    liftIO $ removeFile fp
    outputStr "Info: memotree removed."

----------------------------------------------------------

-- PASS 1: Extract code from processed Syntax.

extractCode :: Syntax -> (Text,Text)
extractCode (WriteHaskell _ isH t) = if isH then (t,mempty) else (mempty,t)
extractCode (Sequence xs) = foldMap extractCode xs
extractCode _ = mempty

-- PASS 2: Evaluate Haskell expressions from processed Syntax.

evalCode :: String -- ^ Auxiliary module name
         -> Syntax -> Haskintex Text
evalCode modName = go
  where
    go (WriteLaTeX t) = return t
    go (WriteHaskell b _ t) = do
         mFlag <- manualFlag <$> get
         lhsFlag <- lhs2texFlag <$> get
         let f :: Text -> LaTeX
             f x | not b = mempty
                 | mFlag = raw x
                 | lhsFlag = TeXEnv "code" [] $ raw x
                 | otherwise = verbatim x
         return $ render $ f t
    go (InsertHaTeX   isMemo t) = memoreduce modName isMemo t (as ::    LaTeX) (return .      render)
    go (InsertHaTeXIO isMemo t) = memoreduce modName isMemo t (as :: IO LaTeX) (liftIO . fmap render)
    go (EvalHaskell env isMemo t) = do
         mFlag <- manualFlag <$> get
         lhsFlag <- lhs2texFlag <$> get
         let f :: Text -> LaTeX
             f x | mFlag = raw x -- Manual flag overrides lhs2tex flag behavior
                 | env && lhsFlag = TeXEnv "code" [] $ raw x
                 | lhsFlag = raw $ "|" <> x <> "|"
                 | env = verbatim $ layout x
                 | otherwise = verb x
         (render . f) <$> ghc modName isMemo t
    go (Sequence xs) = mconcat <$> mapM go xs

ghc :: String -> Bool -> Text -> Haskintex Text
ghc modName isMemo t = do
  let e = unpack t
  outputStr $ "Evaluation: " ++ e
  memt <- memoTree <$> get
  let p = if isMemo then M.lookup t memt else Nothing
  case p of
    Nothing -> do
      -- Run GHC externally and read the result.
      r <- lift $ pack . init <$> readProcess "ghc"
                -- Disable reading of .ghci files.
                [ "-ignore-dot-ghci"
                -- Evaluation loading the temporal module.
                , "-e", e, modName ++ ".hs"
                  ] []
      -- If the expression is marked to be memorized, we do so.
      when isMemo $ do
         modify $ \st -> st { memoTree = M.insert t r $ memoTree st }
         outputStr "-> Result has been memorized."
      -- Return result
      return r
    Just o -> do
      outputStr "-> Result of the evaluation recovered from memo tree."
      return o

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
--
haskintex :: [String] -> IO ()
haskintex = evalStateT haskintexmain . readConf

haskintexmain :: Haskintex ()
haskintexmain = do
  flags <- get
  if -- If the help flag is passed, ignore everything else
     -- and just print the help.
     helpFlag flags
     then lift $ putStr help
     else let xs = inputs flags
          in  if null xs
                 then lift $ putStr noFiles
                 else do memoTreeOpen
                         mapM_ haskintexFile xs
                         willClean <- memocleanFlag <$> get
                         if willClean then memoTreeClean else memoTreeSave

commas :: [String] -> String
commas = concat . intersperse ", "

showEnabledFlags :: Haskintex ()
showEnabledFlags = do
  c <- get
  outputStr $ "Enabled flags: "
           ++ commas (foldr (\(str,f) xs -> if f c then str : xs else xs) [] supportedFlags)
           ++ "."

reportWarnings :: Haskintex ()
reportWarnings = do
  -- Combination of manual and lhs2tex flags.
  manFlag <- manualFlag  <$> get
  lhsFlag <- lhs2texFlag <$> get
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
  uFlags <- unknownFlags <$> get
  unless (null uFlags) $
    outputStr $ "Unsupported flags: " ++ commas uFlags ++ "."
  -- File parsing.
  outputStr $ "Reading " ++ fp ++ "..."
  t <- lift $ T.readFile fp
  pres <- runParserT parseSyntax () fp t
  case pres of
    Left err -> outputStr $ "Reading of " ++ fp ++ " failed:\n" ++ show err
    Right s -> do
      -- Zero pass: In case of debugging, write down the parsed AST.
      dbugFlag <- debugFlag <$> get
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
      l <- evalCode modName s
      -- Write final output.
      let fp' = dropExtension (takeFileName fp) ++ ".tex"
          writeit = do outputStr $ "Writing final file at " ++ fp' ++ "..."
                       lift $ T.writeFile fp' l
      outFlag <- stdoutFlag <$> get
      overFlag <- overwriteFlag <$> get
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
      kFlag <- keepFlag <$> get
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
  , "The underlying HaTeX version is " ++ showVersion Hatex.version ++ "."
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
  , "  -memo       Unless otherwise specified, every evalhaskell, hatex or iohatex"
  , "              command (or environment) will be called with the memo option."
  , ""
  , "  -memoclean  Cleans the memo tree after the execution of haskintex. If "
  , "              several files are processed, the memo tree will be cleaned"
  , "              after processing all of them."
  , ""
  , "  -autotexy   Apply the function texy from HaTeX to every expression in a hatex"
  , "              or iohatex command. This effectively allows the user to write"
  , "              expressions in types other than LaTeX and have haskintex to perform"
  , "              the required transformation."
  , ""
  , "  -nosandbox  Do not use the sandbox package db even in the presence of one."
  , ""
  , "Any unsupported flag will be ignored."
  ]

noFiles :: String
noFiles = "No input file given.\n"
