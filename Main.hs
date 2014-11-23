{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Monad (when)
import Options.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import Control.Exception (try, IOException)
import Data.Either (either)
import System.FilePath (replaceExtension)
import qualified Litany as L

data Args = Args { infile :: String
                 , markdown :: Bool
                 , extract :: Bool } deriving (Eq, Show)

args :: Parser Args
args = Args <$> ( argument str (metavar "<infile>") )
            <*> switch
                ( long "markdown"
                <> short 'm'
                <> help "emit a markdown file basename.md" )
            <*> switch
                ( long "extract"
                <> short 'e'
                <> help "extract files from basename.lit" )

argsInfo :: ParserInfo Args
argsInfo = info ( helper <*> args )
                ( fullDesc <> progDesc "process a .lit file" <> header "litany" )

readErr = "readFile error"

runMain :: Args -> IO ()
runMain (Args i m e) = do
    res <- (try $ B.readFile i >>= (return . U.toString) :: IO (Either IOException String))
    let s = case res of
            Left err -> readErr
            Right s0 -> s0
    if not m && not e
        then putStrLn "error: must choose at least one of {-m,-e}"
        else either
                (mapM_ putStrLn)
                (\s' -> sequence_ [when m (L.writeMarkdown (replaceExtension i "md") s'),
                                   when e (L.writeEmbeddedFiles s')])
                (if s == readErr then Left [s] else L.check s)

main :: IO ()
main = execParser argsInfo >>= runMain
