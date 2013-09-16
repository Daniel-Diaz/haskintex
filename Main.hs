
{-# LANGUAGE OverloadedStrings #-}

-- LaTeX
import Text.LaTeX
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Parser
-- System
import System.Process
import System.Environment (getArgs)
import System.FilePath
import System.Directory
-- Text
import Data.Text (Text,pack,unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- Transformers
import Control.Applicative
import Control.Monad (when)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

isCommand :: String -> LaTeX -> Bool
isCommand str (TeXComm n _) = str == n
isCommand _ _ = False

isEnv :: String -> LaTeX -> Bool
isEnv str (TeXEnv n _ _) = str == n
isEnv _ _ = False

extractCode :: Bool -> LaTeX -> Writer Text LaTeX
extractCode v = texmapM (isEnv "writehaskell") f
  where
    f (TeXEnv _ as b) = do
      let rb = render b
      tell rb
      return $
        if v then if as == [OptArg "hidden"]
                     then mempty
                     else verbatim rb
             else if as == [OptArg "visible"]
                     then verbatim rb
                     else mempty
    f l = return l

moduleHeader :: String -> Text -> Text
moduleHeader str t = pack ("module " ++ str ++ " where\n\n") <> t

-- Evaluation

evaluate :: String -> LaTeX -> Haskintex LaTeX
evaluate modName = texmapM (\l -> isCommand "evalhaskell" l || isEnv "evalhaskell" l) f
  where
    i = "import " ++ modName
    ghc e = do
       outputStr $ "Evaluation: " ++ e
       lift $ init <$> readProcess "ghc" [ "-e", e, modName ++ ".hs" ] []
    f (TeXEnv  _ _ e)        = (verbatim . layout . pack) <$> ghc (unpack $ render e)
    f (TeXComm _ [FixArg e]) = (verb     . pack         ) <$> ghc (unpack $ render e)

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
  { keepFlag :: Bool
  , visibleFlag :: Bool
  , verboseFlag :: Bool
  , inputs :: [FilePath]
    }

isArg :: String -> Bool
isArg [] = False
isArg (x:_) = x == '-'

readConf :: [String] -> Conf
readConf xs =
  Conf ("-keep" `elem` xs)
       ("-visible" `elem` xs)
       ("-verbose" `elem` xs)
       (filter (not . isArg) xs)

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
haskintex = ask >>= mapM_ haskintexFile . inputs

haskintexFile :: FilePath -> Haskintex ()
haskintexFile fp = do
  outputStr $ "Reading " ++ fp ++ "..."
  t <- lift $ T.readFile fp
  case latexAtOnce t of
    Left err -> outputStr $ "Reading of " ++ fp ++ " failed: " ++ err
    Right l -> do
      let modName = ("Haskintex_" ++) $ dropExtension $ takeFileName fp
      outputStr $ "Creating " ++ modName ++ " module..."
      v <- visibleFlag <$> ask
      let (l',hs) = runWriter $ extractCode v l
      lift $ T.writeFile (modName ++ ".hs") $ moduleHeader modName hs
      outputStr $ "Evaluating expressions in " ++ fp ++ "..."
      l'' <- evaluate modName l'
      let fp' = "haskintex_" ++ fp
      outputStr $ "Writing final file at " ++ fp' ++ "..."
      lift $ T.writeFile fp' $ render l''
      k <- keepFlag <$> ask
      when (not k) $ do
        outputStr $ "Removing Haskell Source file " ++ modName ++ ".hs..."
        lift $ removeFile $ modName ++ ".hs"
      outputStr $ "End of processing of file " ++ fp ++ "."
