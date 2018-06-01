
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall            #-} 

{-|

  Module      : AutoBench.Internal.UserIO
  Description : AutoBench's user IO.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  This module deals with all AutoBench's user IO

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - improvementReport is duplicated;
   -
-}

module AutoBench.Internal.UserIO 
  ( 

  -- * User interactions
    selTestSuiteOption                  -- Select a test suite to run from validated 'UserInputs'.
                                        -- Note: in some cases no valid test suites will be available due to
                                        -- input errors, in this case users can review the 'UserInputs'
                                        -- data structure /using this function/.
  -- * User outputs
  , outputAnalysisReport                -- Output the results of statistical analysis.
  , outputQuickAnalysis                 -- Output quick analysis results.

  ) where

import           Control.Exception         (SomeException, catch)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Char                 (toLower)
import           Data.List                 (sort)
import           Data.List.Utils           (replace)

import           System.Console.Haskeline  (InputT, MonadException, getInputLine)
import           System.Directory          (doesFileExist)
import qualified Text.PrettyPrint.HughesPJ as PP


import AutoBench.Internal.Utils          ((<<+>>), deggar, strip, wrapPPList)
import AutoBench.Internal.AbstractSyntax (Id)
import AutoBench.Internal.Types 
  ( AnalOpts(..)
  , AnalysisReport(..)
  , QuickAnalysis(..)
  , QuickResults(..)
  , SimpleResults(..)
  , TestReport(..)
  , TestSuite(..)
  , UserInputs(..)
  , docCoords
  , docQuickResults
  , docSimpleResults
  , docTestSuite
  , docUserInputs
  , showImprovements
  )


-- * User interactions 

-- | Select which test suite to run from the 'UserInputs' data structure:
-- 
-- * If precisely one test suite is valid, then it is automatically selected;
-- * If two or more test suites are valid, then users must pick;
-- * If no test suites are valid, then users can review the 'UserInput's 
--   data structure.
--
-- In all cases, users can also review the 'UserInput's data structure.
selTestSuiteOption 
  :: (MonadIO m, MonadException m) 
  => UserInputs 
  -> InputT m [(Id, TestSuite)]    -- Note: to be generalised to one or more test suites running sequentially.
selTestSuiteOption inps = case _testSuites inps of 
  -- No valid test suites:
  []   -> do
    liftIO $ putStr "\n\n"
    liftIO (putStrLn "  No valid test suites.")
    let go = do liftIO $ putStrLn ""
                liftIO $ putStrLn $ unlines 
                  [ "  * View parse results [P]" 
                  , "  * Exit               [E]" ]
                fmap (fmap toLower . strip) <$> getInputLine "> " >>= \case 
                  Nothing  -> return []
                  Just "e" -> return [] 
                  Just "p" -> liftIO (putStrLn "\n" >> showUserInputs >> putStrLn "\n") >> go
                  Just _   -> inpErr >> go
    go
  -- One valid test suite: automatically selected.
  [ts] -> return [ts]
  -- Two or more test suites: user picks /one for the time being/.
  -- This will be generalised to picking multiple for sequential executing.
  _  -> do 
    liftIO $ putStr "\n\n"
    liftIO (putStrLn "  Multiple valid test suites:")
    liftIO (showTestSuites $ _testSuites inps)
    let go = do liftIO $ putStrLn ""
                liftIO $ putStrLn $ unlines 
                  [ "  * Run a test suite   [1" ++ endRange
                  , "  * View test suites   [V]"
                  , "  * View parse results [P]" 
                  , "  * Exit               [E]" ]
                fmap (fmap toLower . strip) <$> getInputLine "> " >>= \case 
                  Nothing  -> return []
                  Just "e" -> return [] 
                  Just "p" -> liftIO (putStrLn "\n" >> showUserInputs >> putStrLn "\n") >> go
                  Just "v" -> liftIO (showTestSuites $ _testSuites inps) >> go
                  Just inp -> case reads inp :: [(Int, String)] of 
                    []         -> inpErr >> go
                    (n, _) : _ -> if n >= 1 && n <= l
                                  then do 
                                    liftIO $ putStrLn ""
                                    return [_testSuites inps !! (n - 1)]
                                  else inpErr >> go
    go
 
  where 
    -- How many test suites are valid?
    l        = length (_testSuites inps)
    endRange = if l > 1
               then ".." ++ show (l :: Int) ++ "]"
               else "]"
    -- Invalid user input message.
    inpErr   = liftIO $ putStrLn "\n Error: invalid choice.\n"

    -- A simplified pretty printing for 'TestSuite's.
    showTestSuites tss = do 
      putStrLn ""
      print $ PP.nest 4 $ PP.vcat $ (PP.punctuate (PP.text "\n") $ 
        fmap (uncurry showTestSuite) $ zip [1..] tss)
      where
        showTestSuite :: Int -> (Id, TestSuite) -> PP.Doc
        showTestSuite idx (idt, ts) = PP.vcat 
          [ PP.text $ show idx ++ ") " ++ idt
          , PP.nest 10 $ docTestSuite ts ]

    -- Use 'docUserInputs' but nest 2.
    showUserInputs = print $ PP.nest 2 $ docUserInputs inps

-- * User output

-- | Output the results of statistical analysis.
outputAnalysisReport :: AnalOpts -> TestReport -> AnalysisReport -> IO ()
outputAnalysisReport aOpts tr ar = do 

  -- Console output:
  putStrLn ""
  print fullReport
  putStrLn ""

  -- File output:
  maybe (return ()) (reportToFile fullReport)             (_reportFP aOpts)
  maybe (return ()) (coordsToFile (_anlys ar) (_blAn ar)) (_coordsFP aOpts)
  maybe (return ()) (graphToFile  (_anlys ar) (_blAn ar)) (_graphFP  aOpts)

  where 
     
    -- Full test and analysis report.
    fullReport :: PP.Doc 
    fullReport = PP.vcat 
      [ -- Test report in case 'TestReport' has been loaded from file.
        PP.nest 1 $ PP.text $ "-- \ESC[3mTest summary\ESC[0m " ++ replicate 62 '-' ++ "\n"  -- Headers are 80 wide.
      , PP.nest 2 trReport
      -- Analysis of results.
      , PP.nest 1 $ PP.text $ "-- \ESC[3mAnalysis\ESC[0m " ++ replicate 66 '-'
      -- Measurements for each individual test program.
      , PP.nest 2 $ docSimpleResults $ _anlys ar ++ case _blAn ar of 
          Nothing -> []
          Just sr -> [sr] -- Display baseline measurements if there are any.
      -- Improvements report.
      , improvementsReport
      -- Footer 
      , PP.nest 1 $ PP.text $ replicate 65 '-' ++ " \ESC[3mAutoBench\ESC[0m --"
      ]

    -- Report of improvements/optimisations.
    improvementsReport :: PP.Doc 
    improvementsReport  = case (_eql tr, _imps ar) of 
      (_, [])       -> PP.empty  -- No improvements/optimisations.
      (True, imps)  -> PP.vcat   -- One or more optimisations.
        [ 
          if length imps == 1 
             then PP.nest 2 $ PP.text "Optimisation:\n" -- Hack some space.
             else PP.nest 2 $ PP.text "Optimisations:\n"
        , PP.nest 4 . PP.vcat . fmap PP.text . sort . lines $    -- Print alphabetically.
            showImprovements True imps  -- 'showImprovements' returns a string because of 'deggar'ing.
        ] 
      (False, imps) -> PP.vcat   -- One or more improvements.
        [ 
          if length imps == 1 
             then PP.nest 2 $ PP.text "Improvement:\n"
             else PP.nest 2 $ PP.text "Improvements:\n"
        , PP.nest 4 . PP.vcat . fmap PP.text . sort . lines $   -- Print alphabetically.
            showImprovements False imps
        ]

    -- Test report.
    trReport :: PP.Doc 
    trReport  = PP.vcat $ fmap (uncurry (<<+>>)) (zip headers values) -- Side by side two spaces.
      where
        -- Left side headers.
        headers = fmap PP.text . deggar $  -- 'deggar' them to the same width.
          [ "Programs"
          , "Data"
          , "Normalisation"
          , "QuickCheck"
          , "GHC flags" 
          ]

        -- Values for each heading.
        values =
          [ wrapPPList 64 ", " (_tProgs tr)   -- Test programs.
          , PP.text (show $ _tDataOpts tr)    -- Data options.
          , if _tNf tr                        -- Normal form/weak head normal form.
               then PP.text "nf" 
               else PP.text "whnf"  
          , if _eql tr                        -- QuickCheck equal.
               then PP.text "\10004" 
               else PP.text "\10007"
          , if null (_tGhcFlags tr)           -- GHC flags.
               then PP.text "n/a" 
               else wrapPPList 64 ", " (_tGhcFlags tr)
          ]

    -- File output: -----------------------------------------------------------
    
    -- Write full report to file.
    reportToFile :: PP.Doc -> FilePath -> IO ()
    reportToFile doc fp = writeToFile fp "Report" $ replace "\ESC[3m" "" 
      . replace "\ESC[0m" "" $ "\n" ++ PP.render doc

    -- Write coordinates of each test case to file.
    coordsToFile :: [SimpleResults] -> Maybe SimpleResults -> FilePath -> IO ()
    coordsToFile srs mbls fp = writeToFile fp "Coords file" $ PP.render $ 
      PP.vcat $ fmap (\sr -> PP.vcat $ [ PP.text $ "\n" ++ (_srIdt sr),
        docCoords $ _srRaws sr]) (srs ++ maybe [] return mbls)

    -- Generate the runtime graph:
    graphToFile :: [SimpleResults] -> Maybe SimpleResults -> FilePath -> IO ()
    graphToFile srs mbls fp = undefined


-- | Output quick analysis results.
outputQuickAnalysis :: AnalOpts -> Bool -> QuickAnalysis -> IO ()
outputQuickAnalysis aOpts eql qa = do 

  -- Console output:
  putStrLn ""
  print fullReport
  putStrLn ""

  -- File output:
  maybe (return ()) (reportToFile fullReport)   (_reportFP aOpts)
  maybe (return ()) (coordsToFile $ _qAnlys qa) (_coordsFP aOpts)
  --maybe (return ()) (graphToFile  $ _qAnlys qa) (_graphFP  aOpts) 
  
  where 
 
    -- Full test and analysis report.
    fullReport :: PP.Doc 
    fullReport = PP.vcat 
      [
      -- Analysis of results.
        PP.nest 1 $ PP.text $ "-- \ESC[3mAnalysis\ESC[0m " ++ replicate 66 '-' ++ "\n"
      -- Measurements for each individual test program.
      , PP.nest 2 $ docQuickResults $ _qAnlys qa
      -- Improvements report.
      , improvementsReport
      -- Footer 
      , PP.nest 1 $ PP.text $ replicate 65 '-' ++ " \ESC[3mAutoBench\ESC[0m --"
      ]

    -- Report of improvements/optimisations.
    improvementsReport :: PP.Doc 
    improvementsReport  = case (eql, _qImps qa) of 
      (_, [])       -> PP.empty                                   -- No improvements/optimisations.
      (True, imps)  -> PP.vcat                                    -- One or more optimisations.
        [ 
          if length imps == 1 
             then PP.nest 2 $ PP.text "Optimisation:\n"           -- Hack some space.
             else PP.nest 2 $ PP.text "Optimisations:\n"
        , (PP.nest 4 . PP.vcat . fmap PP.text . sort . lines $    -- Print alphabetically.
            showImprovements True imps) PP.<> PP.text "\n"        -- 'showImprovements' returns a string because of 'deggar'ing.
        ] 
      (False, imps) -> PP.vcat   -- One or more improvements.
        [ 
          if length imps == 1 
             then PP.nest 2 $ PP.text "Improvement:\n"
             else PP.nest 2 $ PP.text "Improvements:\n"
        , (PP.nest 4 . PP.vcat . fmap PP.text . sort . lines $   -- Print alphabetically.
            showImprovements False imps) PP.<> PP.text "\n"
        ]

    -- File output: -----------------------------------------------------------
    
    -- Write full report to file.
    reportToFile :: PP.Doc -> FilePath -> IO ()
    reportToFile doc fp = writeToFile fp "Report" $ replace "\ESC[3m" "" 
      . replace "\ESC[0m" "" $ "\n" ++ PP.render doc

    -- Write coordinates of each test case to file.
    coordsToFile :: [QuickResults] -> FilePath -> IO ()
    coordsToFile qrs fp = writeToFile fp "Coords file" $ PP.render $ 
      PP.vcat $ fmap (\qr -> PP.vcat $ [ PP.text $ "\n" ++ (_qrIdt qr),
        docCoords $ _qrRaws qr]) qrs

    -- Generate the runtime graph:
    graphToFile :: [QuickResults] -> FilePath -> IO ()
    graphToFile qrs fp = undefined


-- * Helper functions

-- | Write output to file with a success/fail prompt and catch and print any 
-- errors.
writeToFile :: FilePath -> String -> String -> IO ()
writeToFile fp prompt output = 
 ( do writeFile fp output
      b <- doesFileExist fp 
      if b
      then putStrLn $ prompt ++ " created: " ++ fp
      else putStrLn $ prompt ++ " could not be created."
 ) `catch` (\(e :: SomeException) -> putStrLn $ 
     prompt ++ " could not be created: " ++ show e)