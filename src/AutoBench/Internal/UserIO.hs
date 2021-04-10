
{-# OPTIONS_GHC -Wall            #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|

  Module      : AutoBench.Internal.UserIO
  Description : AutoBench's user IO.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  This module deals with all AutoBench's user IO, including:

  * Users selecting test suites to fun and linear models of best fit;
  * Outputting analysis results to the console and to file.

  Note: these functions are separated from 'AutoBench.Internal.IO' because that
  file uses hidden GHC functions that require the @-package ghc@ flag. This
  /shouldn't/ be a requirement of QuickBench.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - No support for 3D graphs;
   - Both output functions have some similarities, abstract;
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
  , printGoodbyeMessage                 -- Say goodbye.

  ) where

import           Control.Arrow             ((&&&))
import           Control.Exception         (SomeException, catch)
import           Control.Monad.Catch       (MonadMask)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Char                 (toLower)
import           Data.List                 (sort)
import           Data.List.Utils           (replace)
import           Data.Maybe                (catMaybes)
import qualified Data.Vector.Storable      as V
import           System.Console.Haskeline  ( InputT
                                           , defaultSettings, getInputLine
                                           , runInputT )
import           System.Directory          (doesFileExist)
import           System.FilePath.Posix     (makeValid)
import qualified Text.PrettyPrint.HughesPJ as PP
import           Text.Printf               (printf)

import AutoBench.Internal.AbstractSyntax (Id, unqualIdt)
import AutoBench.Internal.Chart          (plotAndSaveAnalGraph)
import AutoBench.Internal.Utils          ((<<+>>), deggar, strip, wrapPPList)

import AutoBench.Internal.PrettyPrinting 
  ( docCoords
  , docDataOpts
  , docQuickResultss
  , docLinearType
  , docSimpleResultss
  , docStats
  , docTestSuite_ProgsDataOpts
  , docUserInputs
  , showImprovements
  , wrapDocExpr
  )

import AutoBench.Internal.Types
  ( AnalOpts(..)
  , AnalysisReport(..)
  , Coord
  , LinearFit(..)
  , QuickAnalysis(..)
  , QuickResults(..)
  , SimpleResults(..)
  , TestReport(..)
  , TestSuite(..)
  , UserInputs(..)
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
  :: (MonadIO m, MonadMask m)
  => UserInputs
  -> InputT m [(Id, TestSuite)]
  -- Note: /to be generalised to one or more test suites running sequentially/.
selTestSuiteOption inps = case _testSuites inps of
  -- No valid test suites.
  -- Users have the option to view the parsed results in the 'UserInputs'
  -- data structure or exit.
  [] -> do
    liftIO $ putStr "\n"                                                                            -- <TO-DO>: All users options should be PP.
    liftIO (putStrLn "  No valid test suites.")
    let go = do liftIO $ putStrLn ""
                liftIO $ putStrLn $ unlines
                  [ "  \9656 View parse results [P]"
                  , "  \9656 Exit               [E]" ]
                fmap (fmap toLower . strip) <$> getInputLine "> " >>= \case
                  Nothing  -> return []                                                             -- <TO-DO>: Error handling here?
                  Just "e" -> return []
                  Just "p" -> liftIO showUserInputs >> go
                  Just _   -> inpErr >> go
    go
  -- One valid test suite gets automatically selected.
  [ts] -> return [ts]                                                                               -- <TO-DO>: Is this what users want?
  -- Two or more test suites: user picks /one for the time being/.
  -- In the future this will be generalised to picking one or more for
  -- sequential execution. Can still view the parsed results in the 'UserInputs'
  -- data structure or exit.
  _ -> do
    -- liftIO $ putStr "\n"                                                                          -- <TO-DO>: This is all a big jumble of manual spacing and PP.
    -- liftIO (putStrLn "  Multiple valid test suites:")
    liftIO (showTestSuites $ _testSuites inps)
    let go = do liftIO $ putStrLn ""
                liftIO $ putStrLn $ unlines
                  [ "  \9656 Run a test suite   [1" ++ endRange
                  , "  \9656 View test suites   [V]"
                  , "  \9656 View parse results [P]"
                  , "  \9656 Exit               [E]" ]
                fmap (fmap toLower . strip) <$> getInputLine "> " >>= \case
                  Nothing  -> return []
                  Just "e" -> return []                                                             -- <TO-DO>: Too much white space.
                  Just "p" -> liftIO showUserInputs >> go
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
    l = length (_testSuites inps)
    -- End range of "run a test suite" option.
    endRange = if l > 1
               then ".." ++ show (l :: Int) ++ "]"
               else "]"
    -- Invalid user input message.
    inpErr :: (MonadIO m, MonadMask m) => InputT m ()
    inpErr  = liftIO $ putStrLn "\n Error: invalid choice."

    -- A simplified pretty printing for 'TestSuite's.
    showTestSuites :: [(Id, TestSuite)] -> IO ()
    showTestSuites tss = do
      putStrLn ""
      putStrLn $ PP.render $ PP.vcat
       [ PP.nest 1 $ PP.text $ "\x2015\x2015 \ESC[3mTest suites\ESC[0m " ++
           replicate 63 '\x2015' ++ "\n"
       , PP.nest 2 $ (PP.vcat $ PP.punctuate (PP.text "\n") $                                       -- <TO-DO>: How to 'PP.vsep'?
           zipWith3 (\idx idt (_, ts) -> showTestSuite idx idt ts) idxs' idts' tss)
           PP.<> PP.text "\n"
       , PP.nest 1 $ PP.text $ replicate 78 '\x2015'
       ]
      where
        (idts, idxs) = unzip $ zipWith (\(idt, _) idx ->
          (idt, show idx ++ ") ")) tss ([1..] :: [Int])
        idts' = deggar idts
        idxs' = deggar idxs

        showTestSuite :: String -> Id -> TestSuite -> PP.Doc  -- Indexed.
        showTestSuite idx idt ts = (PP.text $ idx ++ idt)
          <<+>> docTestSuite_ProgsDataOpts ts -- Note: test programs are wrapped to 60 width.

    -- Use 'docUserInputs' to show the user inputs data structure
    -- but nest 2.
    showUserInputs :: IO ()
    showUserInputs  = do
     putStrLn ""
     putStrLn $ PP.render $ PP.vcat
       [ PP.nest 1 $ PP.text $ "\x2015\x2015 \ESC[3mParse results\ESC[0m " ++
           replicate 61 '\x2015' ++ "\n"  -- Headers are 80 wide
       , PP.nest 2 $ docUserInputs inps PP.<> PP.text "\n" -- No wrapping.
       , PP.nest 1 $ PP.text $ replicate 78 '\x2015'
       ]

-- | Select which model is the best fit for each set of runtime measurements.
--
-- In the 'AnalOpts', users have the option to select from the @n@ '_topModels'
-- (1 by default). If this is set to > 1 and more than one is available (i.e.,
-- not filtered by '_statsFilt'), then users can pick. In practice this is
-- useful as "one strategy for picking the top model in all cases" doesn't
-- seem to be the best option. Instead, users can review the fitting 'Stats'
-- and make a more informed decision. The models are still ranked, and so users
-- can see which the system thought was the best fitting model regardless.
--
-- * If no models are available for a given set of measurements (i.e., all
--   filtered by '_statsFilt'), then users have no choice;
-- * If precisely one is available, then it is picked automatically;
-- * If more than one is available, then users have a choice.
selFitOptions
  :: (MonadIO m, MonadMask m)
  => [(Id, [LinearFit])]          -- Name of each test program and zero or more model choices.
  -> InputT m [(Id, LinearFit)]   -- Name of each test program and choice.
selFitOptions xss = catMaybes <$> mapM (uncurry selFitOption) xss

  where

    -- One choice per test program.
    selFitOption
      :: (MonadIO m, MonadMask m)
      => Id
      -> [LinearFit]
      -> InputT m (Maybe (Id, LinearFit))
    selFitOption _ []     = return Nothing            -- Nothing to choose from.
    selFitOption idt [lf] = return $ Just (idt, lf)   -- Precisely one choice.
    selFitOption idt lfs  = do                        -- Two or more choices, users pick.
      showFitOptions idt lfs                          -- Show the equation of each model and its rank.
      let go :: (MonadIO m, MonadMask m) => InputT m (Maybe (Id, LinearFit))
          go = do liftIO $ putStrLn ""
                  liftIO $ putStrLn $ unlines                                                       -- <TO-DO>: PP.
                    [ "  \9656 Select a fit       [1" ++ endRange
                    , "  \9656 View fits          [V]"
                    , "  \9656 View statistics    [S]"                                              -- <TO-DO>: Too much white space.
                    , "  \9656 Don't plot         [X]" ]
                  fmap (fmap toLower . strip) <$> getInputLine "> " >>= \case
                    Nothing  -> do
                      liftIO $ putStrLn ""                                                          -- <TO-DO>: Fix this manual spacing.
                      return Nothing                                                                -- <TO-DO>: Error handling here?
                    Just "v" -> do
                      liftIO $ putStrLn ""
                      showFitOptions idt lfs >> go
                    Just "s" -> do
                      liftIO $ putStrLn ""
                      showStats lfs
                      go
                    Just "x" -> do
                      liftIO $ putStrLn ""
                      return Nothing    -- Don't plot a line of best fit for this data set.
                    Just inp -> case reads inp :: [(Int, String)] of
                      []         -> inpErr >> go
                      (n, _) : _ -> if n >= 1 && n <= l
                                    then do
                                      liftIO $ putStrLn ""
                                      return $ Just $ (idt, lfs !! (n - 1))
                                    else inpErr >> go

      go

      where
        -- How many options?
        l = length lfs
        -- End range of "select a fit" option.
        endRange = if l > 1
                   then ".." ++ show (l :: Int) ++ "]"
                   else "]"

        inpErr :: (MonadIO m, MonadMask m) => InputT m ()
        inpErr  = liftIO $ putStrLn "\n Error: invalid choice."


    -- Show the 'Stats' for each model: allowing users to see which
    -- has the best fit according to a /range/ of different fitting statistics.
    showStats
      :: (MonadIO m, MonadMask m)
      => [LinearFit]  -- Model options.
      -> InputT m ()
    showStats lfs = liftIO $ do
      print $ PP.nest 1 $ PP.text $ "\x2015\x2015 \ESC[3mFitting statistics\ESC[0m " ++
        replicate 56 '\x2015' ++ "\n"  -- Headers are 80 wide
      mapM_ (\lf -> (print $ PP.vcat
        [
          PP.nest 2 $ PP.text "y =" PP.<+> (wrapDocExpr 70 $ _ex lf)    -- Equation of each model.
          -- Show instance returns a String, lines and 'vcat' so can nest.
        , PP.nest 4 $ PP.vcat $ fmap PP.text $ lines $ PP.render $ docStats (_sts lf)   -- All 'Stats', no wrapping.
        ]) >> putStrLn "") lfs                                                                      -- <TO-DO>: Manual spacing.
      print $ PP.nest 1 $ PP.text $ replicate 78 '\x2015'

    -- Show only the equations of the models on offer, with a ranking.
    showFitOptions
      :: (MonadIO m, MonadMask m)
      => Id
      -> [LinearFit]
      -> InputT m ()
    showFitOptions idt lfs = liftIO $ putStrLn $ PP.render $ PP.vcat
      [
        PP.nest 2 $ PP.text (unqualIdt idt)             -- Name of test program.
      , PP.nest 4 $ PP.vcat $ zipWith (<<+>>) idxs fits -- Ranked model's equations.
      ]

      where
        fits = fmap ((PP.text "y =" PP.<+>) . wrapDocExpr 70 . _ex) lfs    -- Wrap each equation to 70 width.
        idxs = fmap (PP.text . printIdx . show) ([1..] :: [Int])           -- Indices.

        -- Ensure all indices have the same printed width.
        l  = length lfs
        sl = length (show l)
        printIdx :: String -> String
        printIdx  = printf ("%-" ++ show sl ++ "s)")

-- * User output

-- | Output all the results of statistical analysis.
-- Also gives a brief test summary. This will be more useful in the future
-- when 'TestReport's can be loaded from file.
outputAnalysisReport :: AnalOpts -> TestReport -> AnalysisReport -> IO ()
outputAnalysisReport aOpts tr ar = do

  -- Console output:                                                                                       
  print fullReport
  putStrLn ""

  -- File output:
  maybe (return ()) (graphToFile  (_anlys ar) (_blAn ar)) (_graphFP  aOpts) -- Graph to PNG file.
  maybe (return ()) (reportToFile fullReport)             (_reportFP aOpts) -- Full report to TXT file.
  maybe (return ()) (coordsToFile (_anlys ar) (_blAn ar)) (_coordsFP aOpts) -- Coords CSV.


  where

    -- Console output: --------------------------------------------------------

    -- Full test and analysis report.
    fullReport :: PP.Doc
    fullReport  = PP.vcat
      [ -- Test report in case 'TestReport' has been loaded from file.
        PP.nest 1 $ PP.text $ "\x2015\x2015 \ESC[3mTest summary\ESC[0m " ++
          replicate 62 '\x2015' ++ "\n"  -- Headers are 80 wide.
      , PP.nest 2 trReport
      -- Analysis of results.
      , PP.nest 1 $ PP.text $ "\x2015\x2015 \ESC[3mAnalysis\ESC[0m " ++
          replicate 66 '\x2015' ++ "\n"                                                             -- <TO-DO>: So much hacked spacing here.
      -- Measurements for each individual test program.
      , PP.nest 2 $ docSimpleResultss $ _anlys ar ++ case _blAn ar of
          Nothing -> []
          Just sr -> [sr] -- Display baseline measurements if there are any.
      -- Improvements report.
      , improvementsReport
      -- Footer
      , PP.nest 1 $ PP.text $ replicate 78 '\x2015'
      ]

    -- Test report summary. If the 'TestReport' has been loaded from file,
    -- then this is useful because otherwise users might not have any
    -- information on the test setup.
    trReport :: PP.Doc
    trReport  = (PP.vcat $ zipWith (<<+>>) headers values)                                          -- <TO-DO>: More hacked space here.
      PP.<> PP.text "\n" -- Side by side two spaces.
      where
        -- Left side headers.
        headers = fmap PP.text . deggar $  -- 'deggar' them to the same width.
          [ "Programs"                                                                              -- <TO-DO>: Include more test information? Separate & more detailed 'TestReport' overview?
          , "Data"
          , "Normalisation"
          , "QuickCheck"
          , "GHC flags"
          ]

        -- Values for each heading.
        values =
          [ wrapPPList 64 ", " (fmap unqualIdt $ _tProgs tr)   -- Test programs.
          , docDataOpts (_tDataOpts tr)                        -- Data options.
          , if _tNf tr                                         -- Normal form/weak head normal form.
               then PP.text "nf"
               else PP.text "whnf"
          , if _eql tr                                         -- QuickCheck equal.
               then PP.text "\10004"
               else PP.text "\10007"
          , if null (_tGhcFlags tr)                            -- GHC flags.
               then PP.text "n/a"
               else wrapPPList 64 ", " (_tGhcFlags tr) -- Wrapped.
          ]

    -- Report of improvements/optimisations.
    -- Note: if '_eql', then improvements are "upgraded" to optimisations.
    -- '_eql' is whether QuickCheck testing says the results of test programs
    -- are equal, i.e., @\x -> p1 x == p2 x == p3 x@.
    -- Note: no wrapping but shouldn't need any as only two program names,
    improvementsReport :: PP.Doc
    improvementsReport  = case (_eql tr, _imps ar) of
      (_, [])      -> PP.empty  -- No improvements/optimisations.
      (True, imps) -> PP.vcat   -- One or more /optimisations/.
        [
          if length imps == 1
             then PP.nest 2 $ PP.text "Optimisation:\n" -- Hack some space.
             else PP.nest 2 $ PP.text "Optimisations:\n"                                            -- <TO-DO>: So much hacked spacing here.
        , (PP.nest 4 . PP.vcat . fmap PP.text . sort . lines $  -- 'lines' and 'sort' to print alphabetically.
            showImprovements True imps) PP.<> PP.text "\n"      -- 'showImprovements' returns a string because of 'deggar'.
        ]
      (False, imps) -> PP.vcat   -- One or more /improvements/.
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
    reportToFile doc fp = writeToFile fp "Report" $ replace "\ESC[3m" "" -- Remove unicode.
      . replace "\ESC[0m" "" $ "\n" ++ PP.render doc

    -- Write coordinates of each test case to file.
    -- Boring PP code.
    coordsToFile :: [SimpleResults] -> Maybe SimpleResults -> FilePath -> IO ()
    coordsToFile srs mbls fp = writeToFile fp "Coords file" $ PP.render $
      PP.vcat $ fmap (\sr -> PP.vcat $ [ PP.text $ "\n" ++ (unqualIdt $ _srIdt sr),
        docCoords $ _srRaws sr]) (srs ++ maybe [] return mbls)

    -- Generate the runtime graph:
    graphToFile
      :: [SimpleResults]         -- Test program results.
      -> Maybe SimpleResults     -- Baselines.
      -> FilePath
      -> IO ()
    graphToFile [] _ _ = return () -- Nothing to generate.                                          -- <TO-DO>: Warning here?
    graphToFile srs mbls fp = case _srRaws $ head srs of
      Right{} -> putStrLn "3D graphs coming soon."                                                  -- <TO-DO>: 3D GRAPHS!
      Left{}  -> do
        putStrLn "  \9656 Select trend lines for the graph of results:\n"
        (progFits, blsFit) <- runInputT defaultSettings $ do                           -- Have users pick the model to plot on the graph.
          (,) <$> (selFitOptions $ fmap (_srIdt &&& _srFits) srs)                      -- Test programs.
              <*> (selFitOptions $ fmap (_srIdt &&& _srFits) $ maybe [] return mbls)   -- Baseline measurements.

        -- Project raw coordinates from 'SimpleResults', they are all
        -- definitely 'Lefts', so strip the 'Left'.
        -- Make the plots for the results of each test program ready to graph.
        -- Also make the baseline plots but only the trend line will be plotted.
        let raws    = fmap (_srIdt &&& ((\(Left x) -> x) . _srRaws)) srs                            -- <TO-DO>: \(Left x) -> x is hacky.
            plots   = fmap (makePlots . (\(idt, coords) ->
              (unqualIdt idt, coords, lookup idt progFits))) raws -- Lookup to see if best fitting model was chosen by user.
            blPlot = case mbls of
               Nothing  -> Nothing -- No baselines.
               Just bls -> let idt     = _srIdt bls
                               Left cs = _srRaws bls -- Will definitely be a 'Left'.
               -- Only trend line will be plotted.
                           in Just $ makePlots (unqualIdt idt, cs, lookup idt blsFit)

        -- Output runtime graph.
        plotAndSaveAnalGraph fp plots blPlot

-- | Output quick analysis results.
outputQuickAnalysis :: AnalOpts -> Bool -> QuickAnalysis -> IO ()
outputQuickAnalysis aOpts eql qa = do -- 'eql' is whether test programs give same results according to QuickCheck.

  -- Console output:
  putStrLn ""
  print fullReport
  putStrLn ""

  -- File output:
  maybe (return ()) (graphToFile  $ _qAnlys qa) (_graphFP  aOpts)
  maybe (return ()) (reportToFile fullReport)   (_reportFP aOpts)
  maybe (return ()) (coordsToFile $ _qAnlys qa) (_coordsFP aOpts)

  where

    -- Console output: --------------------------------------------------------

    -- Just analysis report as tests have definitely just been run.
    fullReport :: PP.Doc
    fullReport  = PP.vcat
      [
      -- Analysis of results.
        PP.nest 1 $ PP.text $ "\x2015\x2015 \ESC[3mAnalysis\ESC[0m " ++
          replicate 66 '\x2015' ++ "\n"                                                             -- <TO-DO>: More hacked space here.
      -- Measurements for each individual test program.
      , PP.nest 2 $ docQuickResultss $ _qAnlys qa
      -- Improvements report.
      , improvementsReport
      -- Footer
      , PP.nest 1 $ PP.text $ replicate 78 '\x2015'
      ]

    -- Report of improvements/optimisations.
    -- 'eql' is passed in as an argument as the QuickCheck test has just been
    -- run.
    improvementsReport :: PP.Doc
    improvementsReport  = case (eql, _qImps qa) of
      (_, [])      -> PP.empty  -- No improvements/optimisations.
      (True, imps) -> PP.vcat   -- One or more /optimisations/.
        [
          if length imps == 1
             then PP.nest 2 $ PP.text "Optimisation:\n"           -- Hack some space.               -- <TO-DO>: More hacked space here.
             else PP.nest 2 $ PP.text "Optimisations:\n"
        , (PP.nest 4 . PP.vcat . fmap PP.text . sort . lines $    -- Print alphabetically.
            showImprovements True imps) PP.<> PP.text "\n"        -- 'showImprovements' returns a string because of 'deggar'ing.
        ]
      (False, imps) -> PP.vcat   -- One or more /improvements/.
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
    -- Boring PP code.
    coordsToFile :: [QuickResults] -> FilePath -> IO ()
    coordsToFile qrs fp = writeToFile fp "Coords file" $ PP.render $
      PP.vcat $ fmap (\qr -> PP.vcat $ [ PP.text $ "\n" ++ (unqualIdt $ _qrIdt qr),
        docCoords $ _qrRaws qr]) qrs

    -- Generate the runtime graph:
    -- No baseline measurements here as QuickBench doesn't support that.
    graphToFile :: [QuickResults] -> FilePath -> IO ()
    graphToFile [] _ = return ()  -- Nothing to generate.                                           -- <TO-DO>: Warning here?
    graphToFile qrs fp = case _qrRaws $ head qrs of
      Right{} -> putStrLn "3D graphs coming soon."                                                  -- <TO-DO>: 3D GRAPHS!
      Left {} -> do -- User picks zero or one model for each program.
        putStrLn "  \9656 Select trend lines for the graph of results:\n"
        fits <- runInputT defaultSettings $ selFitOptions $
          fmap (_qrIdt &&& _qrFits) qrs
        let raws  = fmap (_qrIdt &&& ((\(Left x) -> x) . _qrRaws)) qrs                              -- <TO-DO>: \(Left x) -> x is hacky.
            plots = fmap (makePlots . (\(idt, coords) ->
              (unqualIdt idt, coords, lookup idt fits))) raws

        -- Output runtime graph.
        plotAndSaveAnalGraph fp plots Nothing

-- * Helper functions

-- | For a given test program, raw measurements, and model, generate the
-- trend line coordinates from equation of the chosen model. Also return
-- the 'LinearType's pretty name for the runtime graph's legend.
--
-- Important note: Runtimes predicted by models are 'cleansed'. Infinite values
-- can be predicted by logarithmic models in one particular case: size 0. This
-- is because @LogBase _ 0 = -Infinity@. In this case, the @(0, -Infinity)@
-- coordinate is removed from the dataset.
--
-- As the coordinate is /not/ used for fitting purposes anyway (it also cleansed
-- in AutoBench.Internal.Analysis), this is OK for now.
makePlots
  :: (Id, [Coord], Maybe LinearFit)
  -> (Id, [Coord], Maybe String, Maybe [Coord])
makePlots (idt, coords, Nothing) = (idt, coords, Nothing, Nothing)   -- No model chosen/available.
makePlots (idt, coords, Just lf) =
  ( idt                                             -- Name of test program.
  , coords                                          -- Raw measurements.
  , Just $ PP.render $ docLinearType $ _lft lf      -- 'LinearType's pretty name.
  -- Cleanse predicted runtimes.
  , Just $ cleanse $ zip xs ys -- Line of best fit coordinates calculated from model's equation.
  )
  where
    (xs, _) = unzip coords                          -- Raw x-coordinates.
    ys      = V.toList $ (_yhat lf) (V.fromList xs) -- yhats predicted by the model.

    -- Cleanse in case transformation produces infinite values.                                     -- <TO-DO> *** This needs addressing. ***
    -- /This happens precisely for size 0 only/, @LogBase _ 0 = -Infinity@.
    -- Remove the @(0, -Infinity)@ coordinate.
    -- ** Only cleanse runtimes predicted by models. **
    cleanse :: [Coord] -> [Coord]
    cleanse  = filter (\(_, y) -> not
      (isNaN y || isNegativeZero y || isInfinite y))

-- | Write output to file with a success/fail prompt and catch and print /any/
-- errors.
writeToFile :: FilePath -> String -> String -> IO ()
writeToFile fp prompt output =
 ( do writeFile fp' output
      b <- doesFileExist fp'
      if b
        then putStrLn $ prompt ++ " created: " ++ fp'
        else putStrLn $ prompt ++ " could not be created."
 ) `catch` (\(e :: SomeException) -> putStrLn $      -- Catch all errors here.
     prompt ++ " could not be created: " ++ show e)  -- Show error.
 where fp' = makeValid fp

-- | Say goodbye.
printGoodbyeMessage :: IO ()
printGoodbyeMessage  = putStrLn "Leaving AutoBench."
