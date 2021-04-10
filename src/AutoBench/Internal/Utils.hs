
{-# OPTIONS_GHC -Wall            #-} 
{-# LANGUAGE ScopedTypeVariables #-}

-- |
--
-- Module      : AutoBench.Internal.Utils
-- Description : General-purpose helper functions
-- Copyright   : (c) 2018 Martin Handley
-- License     : BSD-style
-- Maintainer  : martin.handley@nottingham.ac.uk
-- Stability   : Experimental
-- Portability : GHC
--
-- General-purpose, system-wide helper functions.
--

-------------------------------------------------------------------------------
-- <TO-DO>:
-------------------------------------------------------------------------------
-- + 'wrapPPList' and 'bySide' should be removed;
-- + In the future will need a module for parsing (e.g., when parsing 
--   'TestReports' from file);

module AutoBench.Internal.Utils
  (

  -- * Lists 
    notNull                  -- not . null.
  , allEq                    -- Check whether all elements in a list are equal.
  , uniqPairs                -- Unique pairs from a list.
  -- * Tuples
  , (**>)                    -- Convert a list of 2-tuples to a list of 3-tuples by applying a function to the values in each 2-tuple.
  , (==>)                    -- Convert a list of 2-tuples to a list of 3-tuples by placing a constant element at the end of each 2-tuple.
  -- * Formatting
  , subNum                   -- Subscripts for 0-9, then "_n". 
  , superNum                 -- Superscripts for 0-9, then "^n".
  , strip                    -- Strip surrounding whitespace from a string.
  -- * Parsing primitives 
  , Parser                   -- Basic Megaparsec parsing type.
  , integer                  -- Basic 'Int' parser.
  , lexeme                   -- Basic lexeme parser.
  , sc                       -- Basic space consumer.
  , symbol                   -- Basic symbol parser.
  -- ** Command line parsing
  , CLArgs(..)               -- Command line arguments.
  , clArgsParser             -- Options parser for command line arguments.
  -- * Pretty printing
  , (<<+>>)                  -- Put two spaces between two 'PP.Doc's.
  , forceSecs                -- Force a positive number of seconds to be displayed in the given spacing and in the given units.
  , bySide                   -- Put a list of multi-line 'PP.Doc's side by side and output as a 'String'.
  , secs                     -- Convert a number of seconds to a string.
  , wrapPPList               -- Pretty print a list of strings by wrapping it to a maximum width.
  -- * Misc.
  , Padme(..)                -- Padded lists.
  , (.*)                     -- Generalised function composition.
  , deggar                   -- Pad strings with whitespace so they are the same length.
  , filepathToModuleName     -- Convert a filepath to a module name.

  ) where 

import           Data.Char                    (isSpace, toUpper)
import           Data.List                    ( dropWhileEnd, intercalate
                                              , tails, transpose )
import           Data.Void                    (Void)
import           Language.Haskell.Interpreter (ModuleName)
import qualified Options.Applicative          as OPTS
import           System.FilePath.Posix        (isValid, makeValid, takeBaseName) 
import qualified Text.Megaparsec              as MP
import qualified Text.Megaparsec.Char         as MP 
import qualified Text.Megaparsec.Char.Lexer   as L
import           Text.Printf                  (printf)
import qualified Text.PrettyPrint.HughesPJ    as PP

import qualified AutoBench.Internal.Configuration as Config

-- * Lists 

-- | Nobody has time for @not . null@.
--
-- > notNull = not . null
notNull :: [a] -> Bool 
notNull  = not . null

-- | Check whether all elements in a list are equal.
allEq :: Eq a => [a] -> Bool
allEq []       = True
allEq [_]      = True
allEq (x : xs) = all (== x) xs

-- | All unique pairs from a list. 
uniqPairs :: [a] -> [(a, a)]
uniqPairs xs = [ (l, r) | (l : ys) <- tails xs, r <- ys ]

-- * Tuples

-- Convert a list of 2-tuples to a list of 3-tuples by applying a function to 
-- the values in each 2-tuple.
(**>) :: (a -> b -> c) -> [(a, b)] -> [(a, b, c)]
f **> ts = fmap (uncurry (,,) <*> uncurry f) ts

-- Convert a list of 2-tuples to a list of 3-tuples by placing a constant 
-- element at the end of each 2-tuple.
(==>) :: c -> [(a, b)] -> [(a, b, c)]
x ==> ts = (\_ _ -> x) **> ts

-- * Formatting 

-- | Strip surrounding whitespace from a string.
--
-- > strip "  bacon  " = "bacon"
strip :: String -> String
strip  = dropWhileEnd isSpace . dropWhile isSpace

-- | Unicode subscripts for 0-9, then "_n". 
subNum :: (Num a, Eq a, Show a) => a -> String
subNum 0 = "\x2080"
subNum 1 = "\x2081"
subNum 2 = "\x2082"
subNum 3 = "\x2083"
subNum 4 = "\x2084"
subNum 5 = "\x2085"
subNum 6 = "\x2086"
subNum 7 = "\x2087"
subNum 8 = "\x2088"
subNum 9 = "\x2089"
subNum n = "_" ++ show n

-- | Unicode superscripts for 0-9, then "^n".
superNum :: (Num a, Eq a, Show a) => a -> String
superNum 1 = ""
superNum 2 = "\x00B2"
superNum 3 = "\x00B3"
superNum 4 = "\x2074"
superNum 5 = "\x2075"
superNum 6 = "\x2076"
superNum 7 = "\x2077"
superNum 8 = "\x2078"
superNum 9 = "\x2079"
superNum n = "^" ++ show n

-- * Parsing

-- | Basic Megaparser.
type Parser = MP.Parsec Void String

-- | Basic space consumer.
sc :: Parser ()
sc = L.space MP.space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- | Basic symbol parser.
symbol :: String -> Parser String
symbol  = L.symbol sc

-- | Basic lexeme parser.
lexeme :: Parser a -> Parser a
lexeme  = L.lexeme sc

-- | Basic 'Int' parser.
integer :: Parser Int
integer  = lexeme L.decimal

-- * Command line parsing

-- | AutoBench's command line arguments.
data CLArgs = 
  CLArgs
    { _userInputFile :: FilePath  -- ^ User input filepath.
    }

-- | Options parser for AutoBench's command line arguments.
clArgsParser :: OPTS.ParserInfo CLArgs
clArgsParser  = CLArgs <$> OPTS.info 
  userInputFile
  (OPTS.header Config.version) 
  where 
    -- Only argument so far is the user input file.
    userInputFile :: OPTS.Parser FilePath
    userInputFile  = OPTS.argument (OPTS.str >>= readFilepath) 
      (OPTS.metavar "FILEPATH" <> OPTS.help "User input file")

    -- Do some basic checking upfront.
    readFilepath :: String -> OPTS.ReadM FilePath
    readFilepath s 
      | isValid s' = return s'
      | otherwise  = OPTS.readerError ("Invalid filepath: " ++ show s)
      where s' = makeValid s

-- * Pretty printing 

-- | Put a list of multi-line 'PP.Doc's side by side and output as a 'String'.
bySide :: [PP.Doc] -> String -> String
bySide [] _   = ""
bySide xs sep = 
  unlines . foldr1 side . fmap (deggar . lines . PP.render) $ xs
  where 
    side s1 s2   = fmap (intercalate sep) . padded $
                     padLeft s1 (pad s1) <*> padRight s2 (pad s2)
    pad s        = replicate (length $ head s) ' '
    padLeft  s p = (:) <$> s :- p
    padRight s p = (:) <$> s :- p <*> pure []

-- | Convert a number of seconds to a (string, units)-tuple. The string will 
-- consist of four decimal places.
-- 
-- Adapted from Criterion source code:
-- http://hackage.haskell.org/package/criterion-1.4.1.0/docs/src/Criterion.Measurement.html#secs
secs :: Double -> (String, String)
secs k
  | k < 0      = let (s, u) = secs (-k) in ('-' : s, u)
  | k >= 1     = k        `with` "s"
  | k >= 1e-3  = (k*1e3)  `with` "ms"
  | k >= 1e-6  = (k*1e6)  `with` "μs"
  | k >= 1e-9  = (k*1e9)  `with` "ns"
  | k >= 1e-12 = (k*1e12) `with` "ps"
  | k >= 1e-15 = (k*1e15) `with` "fs"
  | k >= 1e-18 = (k*1e18) `with` "as"
  | otherwise  = (printf "%g" k, "s")
   where with (t :: Double) (u :: String)
             | t >= 1e9  = (printf "%.4g" t, u)
             | t >= 1e3  = (printf "%.0f" t, u)
             | t >= 1e2  = (printf "%.1f" t, u)
             | t >= 1e1  = (printf "%.2f" t, u)
             | otherwise = (printf "%.3f" t, u)

-- | Force a /positive/ number of seconds to be displayed in the given spacing 
-- and in the given units. 
--
-- Contains code adapted from Criterion source code:
-- http://hackage.haskell.org/package/criterion-1.4.1.0/docs/src/Criterion.Measurement.html#secs
forceSecs :: Int -> String -> Double -> String 
forceSecs i s k = case s of 
  "s"  -> fmt k 
  "ms" -> fmt (k * 1e3)
  "μs" -> fmt (k * 1e6)
  "ns" -> fmt (k * 1e9)
  "ps" -> fmt (k * 1e12)
  "fs" -> fmt (k * 1e15)
  "as" -> fmt (k * 1e18) 
  _    -> printf ("%-" ++ show i ++ "g") k
  where fmt (t :: Double)
          | t >= 1e9  = printf ("%-" ++ show i ++ ".4g") t
          | t >= 1e3  = printf ("%-" ++ show i ++ ".0f") t
          | t >= 1e2  = printf ("%-" ++ show i ++ ".1f") t
          | t >= 1e1  = printf ("%-" ++ show i ++ ".2f") t
          | otherwise = printf ("%-" ++ show i ++ ".3f") t


-- | Pretty print a list of strings by wrapping it to a maximum width.
-- Arguments are the wrap width and the list delimiter.  
wrapPPList :: Int -> String -> [String] -> PP.Doc 
wrapPPList _ _ [] = PP.empty
wrapPPList width delim ss = PP.vcat sssDelim
  where 

    sss = fmap (fmap PP.text . reverse) $ go 0 [] ss

    sssDelim = case sss of 
      [_] -> fmap (PP.hcat . PP.punctuate (PP.text delim)) sss
      _   -> fmap punctuateEnd (init sss) ++ 
        fmap (PP.hcat . PP.punctuate (PP.text delim)) ([last sss])

    -- All lines except the last have a delimiter at the end too.
    punctuateEnd xs = PP.hcat $ PP.punctuate (PP.text delim) xs ++ [PP.text delim]

    -- If there's a list item that's wider than the width, readjust.
    maxWidth = max width (maximum $ fmap (length . show) ss)   
    sep      = length delim

    -- Split the list into appropriately sized sublists in reverse order.
    go :: Int -> [String] -> [String] -> [[String]]
    go _ acc [] = [acc]
    go currWidth [] (x : xs)
      | currWidth + l > maxWidth = error "maxWidth too small"
      | otherwise = go (currWidth + l) [x] xs 
        where l = length x
    go currWidth acc (x : xs)
      | currWidth + l + sep > maxWidth = acc : go 0 [] (x : xs) 
      | otherwise = go (currWidth + l + sep) (x : acc) xs 
        where l = length x

-- | Put two spaces between 'PP.Doc's.
(<<+>>) :: PP.Doc -> PP.Doc -> PP.Doc
x <<+>> y = x PP.<> PP.text "  " PP.<> y

-- * Misc.

-- | Two argument function composition.
(.*) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.*)  = fmap . fmap 

infixr 8 .*

-- Padding lists. Interesting solution by McBride:
-- https://stackoverflow.com/questions/21349408/zip-with-default-value-instead-of-dropping-values

data Padme m = (:-) { padded :: [m], padder :: m }

instance Functor Padme where 
  fmap = (<*>) . pure

instance Applicative Padme where
  pure                    = ([] :-)
  (fs :- f) <*> (ss :- s) = zapp fs ss :- f s 
    where
      zapp  []         ss'        = fmap f ss'
      zapp  fs'        []         = fmap ($ s) fs'
      zapp  (f' : fs') (s' : ss') = f' s' : zapp fs' ss'

-- | Pad strings with whitespace so they are the same length.
-- Solution by McBridge on Stack Overflow:
-- https://stackoverflow.com/questions/21349408/zip-with-default-value-instead-of-dropping-values
--
-- > deggar ["bacon", "eggs"] = ["bacon", "eggs "]
deggar :: [String] -> [String]
deggar  = transpose . padded . traverse (:- ' ')

-- | Convert a filepath to a module name.
--
-- > filepathToModuleName "foo/bar/foobar/Foo.hs" = "Foo"
filepathToModuleName :: FilePath -> ModuleName
filepathToModuleName fp = let (c : cs) = takeBaseName fp in toUpper c : cs