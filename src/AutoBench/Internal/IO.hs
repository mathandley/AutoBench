
{-# OPTIONS_GHC -Wall            #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE TupleSections       #-}

{-|

  Module      : AutoBench.Internal.IO
  Description : AutoBench's internal IO.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  This module deals with all AutoBench's internal IO, including:

  * System saving to/loading from files;
  * Generating/compiling/executing benchmarking files;
  * Cleaning up temporary files created for/during benchmarking;

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - generateBenchmarks error handling?
   - It would be nice for the generating benchmarking file to be nicely
     formatted;
   - Add more validation to generateBenchmarkingReport;
   - Handle CTRL-C?
-}

module AutoBench.Internal.IO
  (

  -- * IO for benchmarking files
    generateBenchmarkingFile            -- Generate a benchmarking file to benchmark all the test programs in a given test suite
  , generateTestReport                  -- Generate a 'TestReport' that summarises the system's testing phase.
  , compileBenchmarkingFile             -- Compile benchmarking file using zero or more user-specified compiler flags.
  , deleteAllFiles                      -- Delete all files created by the system.
  , deleteBenchmarkingFiles             -- Delete all files created for/during the benchmarking phase.
  , deleteTemporarySystemFiles          -- Delete temporary system files created for/during the benchmarking phase.
  -- * Helper functions
  , anyKeyExit                          -- Press any key to exit.
  , discoverInputFiles                  -- Discover potential input files in the working directory.
  , execute                             -- Execute a file, capturing its output to STDOUT and printing it to the command line.
  , generateBenchmarkingFilename        -- Generate a valid filename for the benchmarking file from the filename of the user input file.
  , nestPutStr                          -- Output documents at a specific level of nesting using 'putStr'.
  , nestPutStrLn                        -- Output documents at a specific level of nesting using 'putStrLn'.
  , spacer                              -- Output @n@ line spaces to the command line.

  ) where

import           Control.Exception         (catch)
import           Control.Exception.Base    (throwIO)
import           Control.Monad             (unless, void)
import           Control.Monad.Catch       (throwM)
import           Criterion.IO              (readJSONReports)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as C
import           Data.List                 ( groupBy, isInfixOf, partition, sort
                                           , sortBy )
import qualified Data.Map                  as Map
import           Data.Ord                  (comparing)
import qualified Data.Vector               as V
import qualified DynFlags                  as GHC
import qualified GHC
import qualified GHC.Paths                 as GHC
import           Statistics.Types          (estPoint)
import           System.Directory          ( doesFileExist, getDirectoryContents
                                           , removeFile )
import           System.FilePath.Posix     ( dropExtension, takeBaseName
                                           , takeDirectory, takeExtension )
import           System.IO                 (Handle, hFlush, stdout)
import           System.IO.Error           (isDoesNotExistError)
import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Text.Megaparsec           as MP

import System.Process
  ( ProcessHandle
  , StdStream(..)
  , createProcess
  , getProcessExitCode
  , proc
  , std_out
  )

import Criterion.Types
 ( Report
 , anMean
 , anOutlierVar
 , anRegress
 , anStdDev
 , ovEffect
 , ovFraction
 , regCoeffs
 , regResponder
 , reportAnalysis
 , reportName
 , reportMeasured
 )

import AutoBench.Internal.AbstractSyntax (Id, ModuleName, prettyPrint, qualIdt)
import AutoBench.Internal.UserIO         (printGoodbyeMessage)
import AutoBench.Internal.Utils          (Parser, allEq, integer, symbol)
import AutoBench.Internal.Types
  ( BenchReport(..)
  , DataOpts(..)
  , DataSize(..)
  , InputError(..)
  , SimpleReport(..)
  , SystemError(..)
  , TestReport(..)
  , TestSuite(..)
  , UserInputs(..)
  )


-- * IO for benchmarking files:

-- | Generate a benchmarking file to benchmark all the test programs in a
-- given test suite. This includes generating/supplying necessary test data.
--
-- Note that the names of test programs and sizes of test inputs are encoded
-- into each benchmark's (i.e., test case's) title to keep track of
-- measurements. This allows for simple but necessary validation checks to be
-- performed on the JSON report file created by Criterion to ensure that test
-- results agree with test inputs. (The checks are done by
-- 'generateBenchmarkingReport'.)
generateBenchmarkingFile
  :: FilePath       -- ^ Filepath to save benchmarking file.
  -> ModuleName     -- ^ User input file's module name.
  -> UserInputs     -- ^ Parsed/categorised user inputs (to cross-reference).
  -> Id             -- ^ The chosen test suite's identifier.
  -> TestSuite      -- ^ The chosen test suite.
  -> IO ()
generateBenchmarkingFile fp mn inps tsIdt ts = do
  -- Generate functional call that will in turn generate the appropriate
  -- benchmarks.
  gFunc <- genFunc gen nf unary
  -- Generate file contents and write to file.
  writeFile fp (PP.render $ contents gFunc)

  where
    ---------------------------------------------------------------------------
    -- ** CHANGING THE CONTENTS WILL BREAK THE SYSTEM **
    ---------------------------------------------------------------------------
    -- Note: imports/definitions are qualified to avoid ambiguity.

    contents gFunc = PP.vcat
      [ PP.text ""
      , PP.text "module Main (main) where"
      , PP.text ""
      , PP.text "import qualified AutoBench.Internal.Benchmarking"       -- Import all generation functions.
      , PP.text "import qualified" PP.<+> PP.text mn                     -- Import user input file.
      , PP.text ""
      , PP.text "main :: IO ()"                                          -- Generate a main function.
      , PP.text "main  = AutoBench.Internal.Benchmarking.runBenchmarks"  -- Run benchmarks.
          PP.<+> PP.char '(' PP.<> gFunc PP.<> PP.char ')'               -- Generate benchmarks.
          PP.<+> PP.text (prettyPrint . qualIdt mn $ tsIdt)              -- Identifier of chosen test suite (for run cfg).
      ]

    ---------------------------------------------------------------------------
    -- ** CHANGING THE NAMES OF THESE FUNCTIONS WILL BREAK THE SYSTEM **
    ---------------------------------------------------------------------------
    -- Important note: each function below encodes the name of test programs
    -- and input sizes into the titles of benchmark test cases.
    ---------------------------------------------------------------------------

    -- Generate benchmarking function call.
    -- genFunc gen? nf? unary?
    genFunc :: Bool -> Bool -> Bool -> IO PP.Doc
    -- genBenchmarksGenNfUn:
    -- Generated test data, results to nf, unary test programs.
    genFunc True True True = return
      (genGenFunc "AutoBench.Internal.Benchmarking.genBenchmarksGenNfUn")
    -- genBenchmarksGenWhnfUn:
    -- Generated test data, results to whnf, unary test programs.
    genFunc True False True = return
      (genGenFunc "AutoBench.Internal.Benchmarking.genBenchmarksGenWhnfUn")
     -- genBenchmarksGenNfBin:
     -- Generated test data, results to nf, binary test programs.
    genFunc True  True  False = return
      (genGenFunc "AutoBench.Internal.Benchmarking.genBenchmarksGenNfBin")
    -- genBenchmarksGenWhnfBin:
    -- Generated test data, results to whnf, binary test programs.
    genFunc True  False False = return
      (genGenFunc "AutoBench.Internal.Benchmarking.genBenchmarksGenWhnfBin")
    -- genBenchmarksManNfUn:
    -- User-specified test data, results to nf, unary test programs.
    genFunc False True True =
      genManFunc "AutoBench.Internal.Benchmarking.genBenchmarksManNfUn"
    -- genBenchmarksManWhnfUn:
    -- User-specified test data, results to whnf, unary test programs.
    genFunc False False True =
      genManFunc "AutoBench.Internal.Benchmarking.genBenchmarksManWhnfUn"
    -- genBenchmarksManNfBin:
    -- User-specified test data, results to nf, binary test programs.
    genFunc False True False =
       genManFunc "AutoBench.Internal.Benchmarking.genBenchmarksManNfBin"
    -- genBenchmarksManWhnfBin:
    -- User-specified test data, results to whnf, binary test programs.
    genFunc False False False =
       genManFunc "AutoBench.Internal.Benchmarking.genBenchmarksManWhnfBin"

    -- Generate function call for benchmarks requiring automatically generated
    -- test data.
    genGenFunc :: Id -> PP.Doc
    genGenFunc func = PP.hsep $
        [ PP.text func
        , ppList $ fmap ppTuple qualProgs
        , PP.text (prettyPrint . qualIdt mn $ tsIdt)
        ]

    -- Generate function call for benchmarks using user-specified test data.
    genManFunc :: Id -> IO PP.Doc
    genManFunc func = do
      dat <- getManualDatIdt (_dataOpts ts)
      return $ PP.hsep
        [ PP.text func
        , ppList $ fmap ppTuple qualProgs
        , PP.text (prettyPrint . qualIdt mn $ tsIdt)
        , PP.text (prettyPrint . qualIdt mn $ dat)
        ]

    -- Pretty print an (identifier, program) tuple.
    ppTuple :: Id -> PP.Doc
    ppTuple idt = PP.char '('
      PP.<> PP.text (show idt)
      PP.<> PP.text ", "
      PP.<> PP.text idt
      PP.<> PP.char ')'

    -- Pretty print a comma-separated list.
    ppList :: [PP.Doc] -> PP.Doc
    ppList docs = PP.hcat $
      PP.char '[' : (PP.punctuate (PP.text ", ") docs) ++ [PP.char ']']

    -- Helpers

    -- Classifiers:
    unary = head (_progs ts) `elem` fmap fst (_unaryFuns inps)  -- Unary test programs?
    nf    = _nf ts                                              -- NF test results?
    gen   = case _dataOpts ts of                                -- Generate test data?
      Manual{} -> False
      Gen{}    -> True

    -- All test programs are qualified with the module name.
    qualProgs = fmap (prettyPrint . qualIdt mn) (_progs ts)

    -- Get the identifier of User-specified test data from 'DataOpts'.
    -- Questionable throwM error handling here? But better than a partial function.
    getManualDatIdt :: DataOpts -> IO Id
    getManualDatIdt (Manual s) = return s
    getManualDatIdt Gen{} =
      throwM (InternalErr $ "generateBenchmarks: unexpected 'Gen' setting.")

-- | Use GHC to compile the benchmarking file. Compile using the flags
-- specified in the 'TestSuite' used to generate the benchmarking file.
-- Includes the location of the user input file as a source directory in case
-- it is not the working directory. Any invalid compiler flags are returned
-- in case they affect test results.
compileBenchmarkingFile
  :: FilePath     -- ^ Benchmarking filepath.
  -> FilePath     -- ^ User input filepath.
  -> [String]     -- ^ GHC compiler flags.
  -> IO [String]  -- ^ Invalid compiler flags.
compileBenchmarkingFile benchFP userFP flags = do
  (success, invalidFlags) <- GHC.runGhc (Just GHC.libdir) $ do
    dflags <- GHC.getSessionDynFlags
    -- Add flags specified in 'TestSuite's '_ghcFlags' list.
    (dflags', invalidFlags, _) <- GHC.parseDynamicFlagsCmdLine dflags (GHC.noLoc <$> flags)
    -- Include location of input file in import paths.
    let dflags'' = dflags' { GHC.importPaths = GHC.importPaths dflags ++ [takeDirectory userFP] }
    void $ GHC.setSessionDynFlags dflags''
    target <- GHC.guessTarget benchFP Nothing
    GHC.setTargets [target]
    success <- GHC.succeeded <$> GHC.load GHC.LoadAllTargets
    return (success, fmap GHC.unLoc invalidFlags)
  -- Throw an error if compilation fails.
  unless success (throwIO $ FileErr "Compilation failed.")
  -- Notify user of any invalid flags in case they affect test results.
  return invalidFlags

-- | Delete all files created for/during the benchmarking phase
-- of testing, including:
--
-- * Benchmarking Haskell module;
-- * Benchmarking binary;
-- * *.o, *.hi files;
-- * Temporary system files e.g., Criterion JSON report file.
deleteAllFiles :: FilePath -> FilePath -> [FilePath] -> IO ()
deleteAllFiles fBench fUser sysTmps =
  mapM_ removeIfExists (fUsers ++ fBenchs ++ sysTmps)
  where
    fUsers   = fmap (dropExtension fUser ++) exts
    fBench'  = dropExtension fBench
    fBenchs  = fBench : fBench' : fmap (fBench' ++ ) exts
    exts     = [".o", ".hi"]

    removeIfExists fp = removeFile fp `catch` handleExists
      where handleExists e | isDoesNotExistError e = return ()
                           | otherwise = throwIO e

-- | Delete files created for/during the benchmarking phase of testing,
-- including:
--
-- * Benchmarking Haskell module;
-- * Benchmarking binary;
-- * *.o, *.hi files;
deleteBenchmarkingFiles :: FilePath -> FilePath -> IO ()
deleteBenchmarkingFiles fBench fUser =
  mapM_ removeIfExists (fUsers ++ fBenchs)
  where
    fUsers   = fmap (dropExtension fUser ++) exts
    fBench'  = dropExtension fBench
    fBenchs  = fBench : fBench' : fmap (fBench' ++ ) exts
    exts     = [".o", ".hi"]

    removeIfExists fp = removeFile fp `catch` handleExists
      where handleExists e | isDoesNotExistError e = return ()
                           | otherwise = throwIO e

-- | Delete temporary files created for/during the benchmarking phase
-- of testing, e.g., Criterion JSON report file.
deleteTemporarySystemFiles :: [FilePath] -> IO ()
deleteTemporarySystemFiles systemTemps =
  mapM_ removeIfExists systemTemps
  where
    removeIfExists fp = removeFile fp `catch` handleExists
      where handleExists e | isDoesNotExistError e = return ()
                           | otherwise = throwIO e

-- | Generate a test report to summarise the system's testing phase. This
-- includes parsing the JSON benchmark report file created by Criterion
-- in order to generate a 'BenchReport': see 'generateBenchmarkingReport'.
generateTestReport
  :: ModuleName -- Module name of user input file.
  -> TestSuite  -- TestSuite used to generate benchmarking file.
  -> FilePath   -- Filepath of Criterion's JSON report.
  -> Bool       -- Whether test programs are semantically equal according to QuickCheck testing.
  -> IO TestReport
generateTestReport mn ts fp eql = do
  -- Generate benchmarking report.
  benchRep <- generateBenchmarkingReport mn ts fp
  return TestReport
           { -- Copy 'TestSuite' settings:
             _tProgs    = sort $ fmap (prettyPrint . qualIdt mn) (_progs ts)
           , _tDataOpts = _dataOpts ts
           , _tNf       = _nf ts
           , _tGhcFlags = _ghcFlags ts
             -- Other test results:
           , _eql       = eql
           , _br        = benchRep
           }

-- | Parse a Criterion JSON report file and use the parsed '[Report]'s to
-- generate a 'BenchReport' that summarises the benchmarking phase of testing.
-- The 'BenchReport' includes a 'SimpleReport' for each test case and baseline
-- measurements, if applicable.
--
-- Some background information:
-- When generating the benchmarking file using 'generateBenchmarkingFile',
-- the names of test programs and input sizes are encoded into the titles of
-- benchmarks. The 'Report' titles are decoded here and checked against the
-- settings of the 'TestSuite used to generate the benchmarks initially.
generateBenchmarkingReport                                                                          -- <TO-DO>: Check input size against the 'UserInputs' data structure.
  :: ModuleName -- Module name of user input file.
  -> TestSuite  -- TestSuite used to generate benchmarking file.                                    -- <TO-DO>: Are these checks sufficient?
  -> FilePath   -- Filepath of Criterion's JSON report.
  -> IO BenchReport
generateBenchmarkingReport mn ts fp = do
  -- Check file exists.
  exists <- doesFileExist fp
  unless exists (throwIO $ FileErr $ "Cannot locate Criterion report: " ++ fp)
  -- Generate 'BenchReport' largely from Criterion's JSON results.
  -- Parse Criterion JSON report.
  readJSONReports fp >>= \case
    -- Parse error.
    Left err -> throwIO (FileErr $ "Invalid Criterion report: " ++ err)
    -- Parsed 'ReportFileContents', only care about 'Report's.
    Right (_, _, reps) ->
      -- Reports are organised differently depending on whether baseline
      -- measurements were taken so separate the baseline measurements.
      let (bls, nonBls) = partition (("Baseline for" `isInfixOf`) . reportName) reps
      in case bls of
        -- If there isn't any baseline measurements, all reports are
        -- test program measurements and have titles such as
        -- Input Size 5/p1    Input Size 5/p2     Input Size 5/p3    for unary.
        -- Input Sizes (5, 5)/p1     Input Sizes (5, 5)/p2           for binary.
        [] -> case noBaselines reps of -- Use 'parseRepName' to parse titles to
                                       -- (Id, DataSize) tuples.
          Nothing -> throwIO $ FileErr $ "Incompatible Criterion report."
          Just xs -> return $ convertReps [] (zip reps xs)
        -- If baseline measurements have been taken, then reports relating
        -- to test program measurements have titles such as:
        -- With Baseline/Input Size 5/p1 etc.
        -- And baseline measurements have titles such as:
        -- With Baseline/Baseline Measurement for Input Size 5.
        _  -> case withBaselines bls nonBls of
           -- Use 'parseRepName' to parse titles to (Id, DataSize) tuples
           -- and 'parseBaseline' to parse baseline measurements to same format.
          Nothing -> throwIO $ FileErr $ "Incompatible Criterion report."
          Just (nBls, nNonBls) ->
            return $ convertReps (zip bls nBls) (zip nonBls nNonBls)

    where
      -- Qualified '_progs' list
      progs = sort $ fmap (prettyPrint . qualIdt mn) (_progs ts)

      -- The overall idea is that the titles of the Criterion reports encode
      -- the names of test programs and the sizes of test inputs. The titles
      -- are decoded and the parsed data is checked against the settings of the
      -- 'TestSuite' used to generate the benchmarks in the first place. This is
      -- perhaps a little over cautious but I think it's worth it.

      -- Parse the report titles of the baseline and test program measurements.
      withBaselines
        :: [Report] -- Baseline measurements.
        -> [Report] -- Test program measurements.
        -> Maybe ([(Id, DataSize)], [(Id, DataSize)])
      withBaselines _ [] = Nothing
      withBaselines bls nonBls = do

        -- Parse titles of baseline measurements. dropWhile (/= 'I') "With
        -- Baseline/Baseline Measurement for Input Size 5" ===> "Input Size 5",
        -- then can use 'parseBaseline'.
        nBls <- sequence $ fmap (MP.parseMaybe parseBaseline .
          dropWhile (/= 'I') . reportName) bls

        -- Parse titles of test program measurements.
        -- dropWhile (/= 'I') "With Baseline/Input Size 5/p1" ===> "Input Size
        -- 5/p1", then can use 'parseRepName'.
        nNonBls <- sequence $ fmap (MP.parseMaybe parseRepName .
          dropWhile (/= 'I') . reportName) nonBls
        -- Group parse results relating to the same test program by grouping
        -- by identifier.

        let nNonBlss = groupBy (\x1 x2 -> fst x1 == fst x2) $
                         sortBy (comparing fst) nNonBls
        -- The size range of test data for each test program.
            sizes = fmap (sort . fmap snd) nNonBlss
        -- Validation checks:
        if | not (allEq $ sort (fmap snd nBls) : sizes) -> Nothing                        -- (1) Make sure same number of measurements for each program
                                                                                          --     and all have same input sizes.
           | not $ (sort $ fmap (fst . head) nNonBlss) == progs -> Nothing                -- (2) Make sure test programs match those in the 'TestSuite's '_progs' list.
                                                                                                    -- (3) <TO-DO>: Some form of input size check against 'UserInputs' data structure.
           | otherwise -> Just (nBls, nNonBls)

      -- Parse the report titles of just the test program measurements.
      noBaselines :: [Report] -> Maybe [(Id, DataSize)]
      noBaselines [] = Nothing
      noBaselines reps = do
        -- Parse titles of test program measurements.
        xs <- sequence $ fmap (MP.parseMaybe parseRepName . reportName) reps
        -- Group parse results relating to the same test program.
        let xss = groupBy (\x1 x2 -> fst x1 == fst x2) $ sortBy (comparing fst) xs
        -- The size range of test data for each test program.
            sizes = fmap (sort . fmap snd) xss
        -- Validation checks:
        if | not (allEq sizes) -> Nothing                                                 -- (1) Make sure same number of measurements for each program
                                                                                          --     and all have same input sizes.
           | not $ (sort $ fmap (fst . head) xss) == progs -> Nothing                     -- (2) Make sure test programs match those in the 'TestSuite's '_progs' list.
                                                                                                    -- (3) <TO-DO>: Some form of input size check against 'UserInputs' data structure.
           | otherwise -> Just xs

      -- Convert a set of Criterion 'Report's from the same test into a
      -- 'BenchReport' by generating a 'SimpleReport' for each test case
      -- and baseline measurement. Copy over some settings from the 'TestSuite'
      -- used to generate the benchmarks initially.
      convertReps
        :: [(Report, (Id, DataSize))] -- Baseline measurements.
        -> [(Report, (Id, DataSize))] -- Test program measurements.
        -> BenchReport
      convertReps bls nonBls =
        BenchReport
          { -- Generate test program 'SimpleReport's.
            _reports = fmap (fmap $ uncurry toSimpleReport) .
              -- Group by test program's identifier.
              groupBy (\(_, (idt1, _)) (_, (idt2, _)) -> idt1 == idt2) .
              -- Sort by test program's identifier.
              sortBy  (\(_, (idt1, _)) (_, (idt2, _)) -> compare idt1 idt2) $ nonBls
          -- Generate 'SimpleReport's for baseline measurements.
          , _baselines = fmap (uncurry toSimpleReport) $ sortBy (comparing snd) bls
          }

        where

         -- Convert a Criterion 'Report' to a 'SimpleReport' for a given
         -- (test program identifier, input size).
         toSimpleReport :: Report -> (Id, DataSize) -> SimpleReport
         toSimpleReport rep (idt, size) =
           SimpleReport
              { _name    = idt
              , _size    = size
              , _runtime = getRegressTime -- Use the runtime predicted by linear regression, /not mean/.
                                          -- ** But fall back on mean if something goes wrong **.
               -- Note: Criterion uses a large number of samples to calculate its statistics.
               -- Each sample itself is a number of iterations, but then the measurements are
               -- standardised, so length here should work(?)
              , _samples    = V.length (reportMeasured rep)
              , _stdDev     = estPoint   . anStdDev     . reportAnalysis $ rep
              , _outVarEff  = ovEffect   . anOutlierVar . reportAnalysis $ rep
              , _outVarFrac = ovFraction . anOutlierVar . reportAnalysis $ rep
              }
           where
             -- Lookup the runtime predicted by linear regression.
             getRegressTime = case filter (\reg -> regResponder reg == "time")
               (anRegress $ reportAnalysis rep) of
                 [x] -> case estPoint <$> Map.lookup "iters" (regCoeffs x) of
                          Just d -> d
                          -- Fall back on mean.
                          Nothing -> mean
                 -- Fall back on mean.
                 _ -> mean

               where mean = estPoint . anMean . reportAnalysis $ rep

      -- Parser helpers to decode report titles: ------------------------------

      -- Parse a report's title into the corresponding test
      -- program's identifier and input size.
      parseRepName :: Parser (Id, DataSize)
      parseRepName  = do
        -- E.g., "Input Sizes (5, 5)/p1"
        -- E.g., "Input Size 5/p2"
        void $ (symbol "Input Sizes") MP.<|> (symbol "Input Size")
        ds <- parseDataSize
        void $ symbol "/"
        idt <- MP.manyTill MP.anySingle MP.eof
        return (idt, ds)

      -- Parse the encoded baseline size from the name of a Criterion report.
      -- E.g., Input Sizes (5, 5) or Input Size 5
      parseBaseline :: Parser (Id, DataSize)
      parseBaseline = do
        void $ (symbol "Input Sizes") MP.<|> (symbol "Input Size")
        ("Baseline measurements",) <$> parseDataSize                                                -- <TO-DO>: This is too important to be a string.

      -- Parse the encoded data size from the name of a Criterion report.
      -- E.g., (5, 5) or 5.
      parseDataSize :: Parser DataSize
      parseDataSize  = (do
        void $ symbol "("
        n1 <- integer
        void $ symbol ","
        n2 <- integer
        void $ symbol ")"
        return (SizeBin n1 n2)) MP.<|> (SizeUn <$> integer)

-- * Helper functions

-- | Execute a file, capturing its output to STDOUT and printing it to the
-- command line.
execute :: FilePath -> IO ()
execute fp = do
  let p = (proc fp []) { std_out = CreatePipe }
  (_, Just out, _, ph) <- createProcess p
  printOutput ph out
  where
    printOutput :: ProcessHandle -> Handle -> IO ()
    printOutput ph h = go
      where
        go = do
          bs <- BS.hGetNonBlocking h (64 * 1024)
          printLine bs
          ec <- getProcessExitCode ph
          maybe go (const $ do
            end <- BS.hGetContents h
            printLine end) ec
        printLine bs = unless (BS.null bs) (C.putStr bs)

-- | Generate a valid filename for the benchmarking file from the filename of
-- the user input file.
generateBenchmarkingFilename :: String -> IO String
generateBenchmarkingFilename s = do
  b1 <- doesFileExist s'
  b2 <- doesFileExist (addSuffix s')
  if b1 || b2
    then go s' 0
    else return (addSuffix s')
  where
    go :: String -> Int -> IO String
    go s_ i = do
      let s_' = s_ ++ show i
      b1 <- doesFileExist s_'
      b2 <- doesFileExist (addSuffix s_')
      if b1 || b2
        then go s_ (i + 1)
        else return (addSuffix s_')

    addSuffix = (++ ".hs")
    s'        = takeDirectory s ++ "/Bench" ++ takeBaseName s

-- | Discover potential input files in the working directory.
discoverInputFiles :: IO [FilePath]
discoverInputFiles  = filter ((== ".hs") . takeExtension) <$> getDirectoryContents "."

-- | Press any key to exit.
anyKeyExit :: IO ()
anyKeyExit  = do 
  putStr "Press any key to exit... "
  hFlush stdout
  -- Want to use Haskeline here but can't.
  -- https://github.com/judah/haskeline/issues/74
  void getChar
  printGoodbyeMessage

-- | Output @n@ line spaces to the command line.
spacer :: Int -> IO ()
spacer n = putStrLn (replicate (n - 1) '\n') >> hFlush stdout

-- | Output documents at a specific level of nesting using 'putStr'.
nestPutStr :: Int -> PP.Doc -> IO ()
nestPutStr n doc = putStr (PP.render $ PP.nest n doc) >> hFlush stdout

-- | Output documents at a specific level of nesting using 'putStrLn'.
nestPutStrLn :: Int -> PP.Doc -> IO ()
nestPutStrLn n doc = putStrLn (PP.render $ PP.nest n doc) >> hFlush stdout
