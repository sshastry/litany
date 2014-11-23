{-# LANGUAGE ScopedTypeVariables #-}

module Litany where

import Types
import Fibers
import FenceParser

import Control.Applicative
import Control.Monad (liftM)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Either
import Data.Maybe (fromJust, fromMaybe)
import Data.List ((\\))
import System.Directory (createDirectoryIfMissing)
import Text.ParserCombinators.Parsec hiding ((<|>),many,optional,State)
import System.FilePath (splitFileName)
import Text.Printf (printf)

----------------------------------------------------------------------
-- util functions

runsOf :: Integral t => t -> [a] -> [[a]]
runsOf _ [] = []
runsOf n0 xs = (take n xs) : runsOf n0 (drop n xs)
                    where n = fromIntegral n0

gatherListAsPairs :: [a] -> [(a,a)]
gatherListAsPairs xs = map (\[x,y] -> (x,y)) xs' where
    k = length xs
    n = if odd k then k-1 else k
    xs' = runsOf 2 $ take n xs

----------------------------------------------------------------------
-- main algorithms

-- invariant: check s == Right s is the identity on the right
check :: String -> Either [String] String
check s = if Set.fromList allFenceLines == Set.fromList validFenceLines
          then Right s
          else Left (map show invalidFences)
  where
    fs = getFenceLines s
    allFenceLines = map lineno fs
    validFenceLines = map lineno $ concat $ getValidSubSeqs fs
    invalidLineNos = allFenceLines \\ validFenceLines
    invalidFences = filter (\x -> lineno x `elem` invalidLineNos) fs

getFenceLines :: String -> [Fence]
getFenceLines s = [toFence (n, x) | (n, Right x) <- ls]
  where
    ls = zip [1..] $ map (\l -> parse fenceP "" l) (lines s)
    toFence (n, (d, l, f)) = Fence {lineno=n, delim=d, lang=l, fname=f}

getValidSubSeqs :: [Fence] -> [[Fence]]
getValidSubSeqs xs = xss where
    fs = fibers (\x -> (lang x, fname x)) xs -- (*)
    xss0 = map snd (Map.toList fs)
    xss = filter validFenceList xss0

{- explanation of (*): fs is a map s.t. the key is a pair such as (Haskell,
 - "foo.hs") and the value over that pair is the collection of all fences such
 - as Fence { lineno = 20, delim = Pause, lang = Haskell, fname = "foo.hs" }
 - with the lineno and delim varying but the lang and fname fixed.  -}

-- $ ($) <$> :)
validFenceList :: [Fence] -> Bool
validFenceList fs = and $ ($) <$> predicates <*> pure fs
  where
    predicates = [(even . length), checkConsistentPairs, globalDelimCheck]

checkConsistentPairs xs = and [langCheck, fnameCheck, localDelimCheck]
  where
    ps = gatherListAsPairs xs
    checkOnePair pred (a, b) = pred a == pred b
    andMap pred = \pairs -> and $ map (checkOnePair pred) pairs
    langCheck = andMap lang ps
    fnameCheck = andMap fname ps
    validDelimPairs = [(Begin, End), (Begin, Pause), (Continue, Pause), (Continue, End)]
    delimPred (x, y) = (delim x, delim y) `elem` validDelimPairs
    localDelimCheck = and $ map delimPred ps

globalDelimCheck xs = and [validBookends, validMidsection]
  where
    ds = map delim xs
    validBookends = head ds == Begin && last ds == End
    middle = length ds - 2
    midsection = take middle (drop 1 ds)
    n = length midsection `div` 2
    pauseContSeq = (concat (replicate n [Pause, Continue]))
    endBeginSeq = (concat (replicate n [End, Begin]))
    validMidsection = midsection `elem` [pauseContSeq, endBeginSeq]

----------------------------------------------------------------------
-- File IO

type EmbeddedFile = (FilePath, [Int])

writeEmbeddedFiles :: String -> IO ()
writeEmbeddedFiles s = mapM_ writeOneFile (getEmbeddedFiles s)
  where
    ls = lines s
    writeOneFile :: EmbeddedFile -> IO ()
    writeOneFile (filePath, lineNos) = do
        let (dir, outfile) = splitFileName filePath
            txt = unlines [ls !! (n-1) | n <- lineNos] -- !! is 0 indexed
            lineCount = printf "%4d" $ length $ lines txt
        createDirectoryIfMissing True dir
        B.writeFile filePath (U.fromString txt)
        putStrLn ("✓ " ++ lineCount ++ " " ++ filePath)

getEmbeddedFiles :: String -> [EmbeddedFile]
getEmbeddedFiles s = nonEmpties
  where
    fenceSeqs = getValidSubSeqs (getFenceLines s)
    ys :: [(FilePath, [Int])] = map (\fenceSeq -> (fname (head fenceSeq), map lineno fenceSeq)) fenceSeqs
    ys' :: [EmbeddedFile] = [(filePath, fleshOutInterval (gatherListAsPairs ns)) | (filePath, ns) <- ys]
    -- an empty filename means just highlight the block in markdown output but don't extract the file
    nonEmpties = filter (\x -> fst x /= "") ys'

fleshOutInterval :: [(Int,Int)] -> [Int]
fleshOutInterval ps = ps >>= f
  where
    f (x,y) = [(x+1)..(y-1)]

writeMarkdown :: FilePath -> String -> IO ()
writeMarkdown path s = do
    B.writeFile path $ U.fromString $ toMarkdown s
    putStrLn ("✓ " ++ path)

----------------------------------------------------------------------
-- conversion to markdown

commentMap :: [(Language, (String,String))]
commentMap = [ (Handlebars,("<!-- "," -->"))
             , (Html,("<!-- "," -->"))
             , (Haskell,("{- "," -}"))
             , (Css,("/* "," */"))
             , (CoffeeScript,("# ",""))
             , (JavaScript,("// ",""))
             , (Bash,("# ","")) ]

makeHeadline :: Language -> FilePath -> String
makeHeadline l f = "```" ++ downcase (show l) ++ fileNameComment
  where
    (open, close) = fromMaybe ("??? "," ???") (lookup l commentMap)
    fileNameComment = if f == "" then "" else ("\n" ++ open ++ f ++ close)

makeBlock :: Either ParseError (Delimiter, Language, FilePath) -> String -> String
makeBlock (Right (Begin, l, f)) s = makeHeadline l f
makeBlock (Right (Continue, l, f)) s = makeHeadline l f
makeBlock (Right (End, l, f)) s = "```"
makeBlock (Right (Pause, l, f)) s = "```"
makeBlock (Left e) s = s

toMarkdown :: String -> String
toMarkdown s = unlines ls2
  where
    ls0 = lines s
    ls1 = zip [parse fenceP "" l | l <- ls0] ls0
    ls2 = map (\(x, y) -> makeBlock x y) ls1

