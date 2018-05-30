
{-# OPTIONS_GHC -Wall             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|

  Module      : AutoBench.Internal.Types
  Description : Datatypes and associated helper functions\/defaults.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  Datatypes used internally throughout AutoBench's implementation and any 
  associated helper functions\/defaults.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - 'DataOpts' Discover setting;
   - Split Types into InternalTypes and Types;
   - Make AnalOpts in TestSuite a maybe type? In case users don't want to 
     analyse right away;
   - Comment docTestSuite, docUserInputs;
   - 
-}

module AutoBench.Internal.Types 
  (

  -- * Re-exports
    module AutoBench.Types
  -- * User inputs
  -- ** Test data options
  , toHRange               -- Convert @Gen l s u :: DataOpts@ to a Haskell range.
  , minInputs              -- Minimum number of distinctly sized test inputs.
  , defBenchRepFilename    -- Default benchmarking JSON report filename.
  -- ** Statistical analysis options
  , maxPredictors          -- Maximum number of predictors for models to be used for regression analysis.         
  , minCVTrain             -- Minimum percentage of data set to use for cross-validation.
  , maxCVTrain             -- Maximum percentage of data set to use for cross-validation.
  , minCVIters             -- Minimum number of cross-validation iterations.
  , maxCVIters             -- Maximum number of cross-validation iterations.
  -- ** Internal representation of user inputs
  , UserInputs(..)         -- A data structure maintained by the system to classify user inputs.
  , initUserInputs         -- Initialise a 'UserInputs' data structure.
  -- * Benchmarking
  , BenchReport(..)        -- A report to summarise the benchmarking phase of testing.
  , Coord                  -- (Input Size, Runtime) results as coordinates for unary test programs.
  , Coord3                 -- (Input Size, Input Size, Runtime) results as coordinates for binary test programs.
  , DataSize(..)           -- The size of unary and binary test data.
  , SimpleReport(..)       -- A simplified version of Criterion's 'Report'. See 'Criterion.Types.Report'.
  -- ** QuickBench
  , QuickReport(..)        -- A report to summarise the QuickBench phase of testing.
  -- * Statistical analysis                                                                                        -- <TO-DO>
  , numPredictors          --  Number of predictors for each type of model.
  -- * Errors
  -- ** System errors
  , SystemError(..)        -- System errors.
  -- ** Input errors
  , InputError(..)         -- User input errors.
  -- * Helpers 
  -- ** Pretty printing
  , docBenchReport         -- Generate a 'PP.Doc' for a 'BenchReport'.
  , docSimpleReport        -- Generate a 'PP.Doc' for a 'SimpleReport'.
  , docTestSuite           -- Generate a 'PP.Doc' for a 'TestSuite'.
  , docUserInputs          -- Generate a 'PP.Doc' for a 'UserInputs'.

  ) where

import           Control.Arrow             ((&&&))
import           Control.Exception.Base    (Exception)
import           Criterion.Types           (OutlierEffect)
import           Data.List                 (sort, sortBy)
import           Data.Ord                  (comparing)
import           Data.Tuple.Select         (sel1, sel2)
import qualified Text.PrettyPrint.HughesPJ as PP

import           AutoBench.Internal.Utils (deggar)
import           AutoBench.Types  -- Re-export.

import AutoBench.Internal.AbstractSyntax 
  ( HsType
  , Id
  , ModuleElem(..)
  , TypeString
  , prettyPrint
  )


-- * User inputs

-- ** Test suites

-- | Convert @Gen l s u :: DataOpts@ to a Haskell range.
toHRange :: DataOpts -> [Int]
toHRange Manual{}    = []
toHRange (Gen l s u) = [l, (l + s) .. u]

-- | Each test suite requires a minimum number of distinctly sized test inputs.
-- 
-- > minInputs = 20
minInputs :: Int 
minInputs  = 20

-- | Default benchmarking JSON report filename.
defBenchRepFilename :: String
defBenchRepFilename  = "autobench_tmp.json"

-- ** Statistical analysis options

{-
instance Show LinearType where 
  show (Poly      0) = "constant"
  show (Poly      1) = "linear"
  show (Poly      2) = "quadratic"
  show (Poly      3) = "cubic"
  show (Poly      4) = "quartic"
  show (Poly      5) = "quintic"
  show (Poly      6) = "sextic"
  show (Poly      7) = "septic"
  show (Poly      8) = "octic"
  show (Poly      9) = "nonic"
  show (Poly      n) = "n" ++ superNum n
  show (Log     b n) = "log" ++ subNum b ++ superNum n ++ "n"
  show (PolyLog b n) = "n" ++ superNum n ++ "log" ++ subNum b ++ superNum n ++ "n"
  show (Exp       n) = show n ++ "\x207F"-}

-- | Maximum number of predictors for models to be used for regression analysis.
--
-- > maxPredictors = 10
maxPredictors :: Int 
maxPredictors  = 10

-- | Minimum percentage of data set to use for cross-validation.
--
-- > minCVTrain = 0.5
minCVTrain :: Double 
minCVTrain  = 0.5

-- | Maximum percentage of data set to use for cross-validation.
--
-- > maxCVTrain = 0.8
maxCVTrain :: Double 
maxCVTrain  = 0.8

-- | Minimum number of cross-validation iterations.
--
-- > minCVIters = 100
minCVIters :: Int 
minCVIters  = 100

-- | Maximum number of cross-validation iterations.
--
-- > maxCVIters = 500
maxCVIters :: Int 
maxCVIters  = 500

-- ** Internal representation of user inputs

-- | While user inputs are being analysed by the system, a 'UserInputs' data
-- structure is maintained. The purpose of this data structure is to classify 
-- user inputs according to the properties they satisfy. For example, when the 
-- system first interprets a user input file, all of its definitions are added 
-- to the '_allElems' list. This list is then processed to determine which 
-- definitions have function types that are syntactically compatible with the 
-- requirements of the system (see 'AutoBench.Internal.StaticChecks'). 
-- Definitions that are compatible are added to the '_validElems' list, and 
-- those that aren't are added to the '_invalidElems' list. Elements in the 
-- '_validElems' list are then classified according to, for example, whether 
-- they are nullary, unary, or binary functions. This check process continues
-- until all user  inputs are classified according to the list headers below. 
-- Note that both static ('AutoBench.Internal.StaticChecks') and dynamic 
-- ('AutoBench.Internal.DynamicChecks') checks are required to classify user 
-- inputs.
--
-- Notice that each /invalid/ definitions has one or more input errors 
-- associated with it.
--
-- After the system has processed all user inputs, users can review this data 
-- structure to see how the system has classified their inputs, and if any 
-- input errors have been generated. 
data UserInputs = 
  UserInputs
   {
     _allElems           :: [(ModuleElem, Maybe TypeString)]         -- ^ All definitions in a user input file.
   , _invalidElems       :: [(ModuleElem, Maybe TypeString)]         -- ^ Syntactically invalid definitions (see 'AutoBench.Internal.AbstractSyntax').
   , _validElems         :: [(Id, HsType)]                           -- ^ Syntactically valid definitions (see 'AutoBench.Internal.AbstractSyntax').
   , _nullaryFuns        :: [(Id, HsType)]                           -- ^ Nullary functions.
   , _unaryFuns          :: [(Id, HsType)]                           -- ^ Unary functions.
   , _binaryFuns         :: [(Id, HsType)]                           -- ^ Binary functions.
   , _arbFuns            :: [(Id, HsType)]                           -- ^ Unary/binary functions whose input types are members of the Arbitrary type class.
   , _benchFuns          :: [(Id, HsType)]                           -- ^ Unary/binary functions whose input types are members of the NFData type class.
   , _nfFuns             :: [(Id, HsType)]                           -- ^ Unary/binary functions whose result types are members of the NFData type class.
   , _invalidData        :: [(Id, HsType, [InputError])]             -- ^ Invalid user-specified test data. 
   , _unaryData          :: [(Id, HsType, [Int])]                    -- ^ Valid user-specified test data for unary functions /with size information/.
   , _binaryData         :: [(Id, HsType, [(Int, Int)])]             -- ^ Valid user-specified test data for binary functions /with size information/.
   , _invalidTestSuites  :: [(Id, [InputError])]                     -- ^ Invalid test suites.
   , _testSuites         :: [(Id, TestSuite)]                        -- ^ Valid test suites.
   }

-- | Initialise a 'UserInputs' data structure by specifying the '_allElems' 
-- list. 
initUserInputs :: [(ModuleElem, Maybe TypeString)] -> UserInputs
initUserInputs xs = 
  UserInputs
    {
      _allElems          = xs
    , _invalidElems      = []
    , _validElems        = []
    , _nullaryFuns       = []
    , _unaryFuns         = []
    , _binaryFuns        = []
    , _arbFuns           = []
    , _benchFuns         = []
    , _nfFuns            = []
    , _invalidData       = []
    , _unaryData         = []
    , _binaryData        = []
    , _invalidTestSuites = []
    , _testSuites        = []
    }

-- * Benchmarking

-- | (Input Size, Runtime) results as coordinates for unary test programs.
type Coord = (Double, Double)

-- | (Input Size, Input Size, Runtime) results as coordinates for binary test 
-- programs.        
type Coord3 = (Double, Double, Double) 

-- | The size of unary and binary test data.
data DataSize = 
    SizeUn Int       -- ^ The size of unary test data.
  | SizeBin Int Int  -- ^ The size of binary test data.
    deriving (Ord, Eq)

instance Show DataSize where 
  show (SizeUn n)      = show n 
  show (SizeBin n1 n2) = show (n1, n2)

-- | A report to summarise the benchmarking phase of testing.
data BenchReport =
  BenchReport 
    {
      _bProgs    :: [String]          -- ^ Names of all test programs.
    , _bDataOpts :: DataOpts          -- ^ Which test data options were used.
    , _bNf       :: Bool              -- ^ Whether test cases were evaluated to normal form.
    , _bGhcFlags :: [String]          -- ^ Flags used when compiling the benchmarking file.
    , _reports   :: [[SimpleReport]]  -- ^ Individual reports for each test case, per test program.
    , _baselines :: [SimpleReport]    -- ^ Baseline measurements (will be empty if '_baseline' is set to @False@).
    }

-- | A simplified version of Criterion's 'Report' datatype, see 
-- 'Criterion.Types.Report'.
data SimpleReport = 
  SimpleReport 
   { 
     _name       :: Id             -- ^ Name of test program.
   , _size       :: DataSize       -- ^ Size of test data.
   , _samples    :: Int            -- ^ Number of samples used to calculate statistics below.
   , _runtime    :: Double         -- ^ Estimate runtime.
   , _stdDev     :: Double         -- ^ Estimate standard deviation.
   , _outVarEff  :: OutlierEffect  -- ^ Outlier effect. 
   , _outVarFrac :: Double         -- ^ Outlier effect as a percentage.
   }

-- ** QuickBench 

-- | A report to summarise the QuickBench phase of testing. For both unary and 
-- binary test programs (using 'Coord' or 'Coord3').
data QuickReport = 
  QuickReport 
    {
      _qName     :: Id                      -- ^ Name of test program.
    , _qRuntimes :: Either [Coord] [Coord3] -- ^ [(Input size(s), mean runtime)].
    }

-- * Statistical analysis

-- | Number of predictors for each type of model.
numPredictors :: LinearType -> Int 
numPredictors (Poly      k) = k + 1 
numPredictors (Log     _ k) = k + 1 
numPredictors (PolyLog _ k) = k + 1 
numPredictors Exp{}         = 2

-- * Errors 

-- | Errors raised by the system due to implementation failures. These can be 
-- generated at any time but are usually used to report unexpected IO results. 
-- For example, when dynamically checking user inputs (see 
-- 'AutoBench.Internal.UserInputChecks'), system errors are used to relay 
-- 'InterpreterError's thrown by functions in the hint package in cases
-- where the system didn't expect errors to result.
data SystemError = InternalErr String

instance Show SystemError where 
  show (InternalErr s) = "Internal error: " ++ s ++ "\nplease report on GitHub."

instance Exception SystemError

-- ** Input errors

-- | Input errors are generated by the system while analysing user input 
-- files. Examples input errors include erroneous test options, invalid test 
-- data, and test programs with missing Arbitrary/NFData instances.
--
-- In general, the system always attempts to continue with its execution for as 
-- long as possible. Therefore, unless a critical error is encountered, such as 
-- a filepath or file access error, it will collate all non-critical input 
-- errors. These will then be summarised after the user input file has been 
-- fully analysed.
data InputError = 
    FilePathErr  String    -- ^ Invalid filepath.
  | FileErr      String    -- ^ File access error.
  | TestSuiteErr String    -- ^ Invalid test suite.
  | DataOptsErr  String    -- ^ Invalid data options.
  | AnalOptsErr  String    -- ^ Invalid statistical analysis options.
  | TypeErr      String    -- ^ Invalid type signature.
  | InstanceErr  String    -- ^ One or more missing instances

instance Show InputError where 
  show (FilePathErr  s) = "File path error: "        ++ s
  show (FileErr      s) = "File error: "             ++ s
  show (TestSuiteErr s) = "Test suite error: "       ++ s
  show (DataOptsErr  s) = "Test data error: "        ++ s
  show (AnalOptsErr  s) = "Analysis options error: " ++ s
  show (TypeErr      s) = "Type error: "             ++ s
  show (InstanceErr  s) = "Instance error: "         ++ s

instance Exception InputError

-- * Helpers 

-- ** Pretty printing 

-- | Simplified pretty printing for the 'TestSuite' data structure.
-- Note: prints test programs and 'DataOpts' only.
docTestSuite :: TestSuite -> PP.Doc                                                                        
docTestSuite ts = PP.vcat 
  [ 
    PP.hcat $ PP.punctuate (PP.text ", ") $ fmap PP.text $ _progs ts
  , PP.text $ show $ _dataOpts ts
  ]

-- | Full pretty printing for the 'UserInputs' data structure. 
-- Note: prints all fields; invalids are printed last. 
-- NB. always print in alphabetical order.                                                
docUserInputs :: UserInputs -> PP.Doc 
docUserInputs inps = PP.vcat $ PP.punctuate (PP.text "\n")
  [ PP.text "All module elements:"     PP.$$ (PP.nest 2 $ showElems             $ _allElems          inps)
  , PP.text "Valid module elements:"   PP.$$ (PP.nest 2 $ showTypeableElems     $ _validElems        inps)
  , PP.text "Nullary functions:"       PP.$$ (PP.nest 2 $ showTypeableElems     $ _nullaryFuns       inps)
  , PP.text "Unary functions:"         PP.$$ (PP.nest 2 $ showTypeableElems     $ _unaryFuns         inps)
  , PP.text "Binary functions:"        PP.$$ (PP.nest 2 $ showTypeableElems     $ _binaryFuns        inps)
  , PP.text "Benchmarkable functions:" PP.$$ (PP.nest 2 $ showTypeableElems     $ _benchFuns         inps)
  , PP.text "Arbitrary functions:"     PP.$$ (PP.nest 2 $ showTypeableElems     $ _arbFuns           inps)
  , PP.text "NFData functions:"        PP.$$ (PP.nest 2 $ showTypeableElems     $ _nfFuns            inps)
  , PP.text "Unary test data:"         PP.$$ (PP.nest 2 $ showTypeableElems     $ fmap (sel1 &&& sel2) $ _unaryData  inps) -- Don't print sizing information.
  , PP.text "Binary test data:"        PP.$$ (PP.nest 2 $ showTypeableElems     $ fmap (sel1 &&& sel2) $ _binaryData inps) -- Don't print sizing information.
  , PP.text "Test suites:"             PP.$$ (PP.nest 2 $ showTestSuites        $ _testSuites        inps)

  -- Invalids come last because they have 'InputError's.
  , PP.text "Invalid module elements:" PP.$$ (PP.nest 2 $ showElems             $ _invalidElems      inps) 
  , PP.text "Invalid test data:"       PP.$$ (PP.nest 2 $ showInvalidData       $ _invalidData       inps)
  , PP.text "Invalid test suites:"     PP.$$ (PP.nest 2 $ showInvalidTestSuites $ _invalidTestSuites inps)
  ]
  where 

    -- Pretty printing for @[(ModuleElem, Maybe TypeString)]@.
    showElems :: [(ModuleElem, Maybe TypeString)] -> PP.Doc 
    showElems [] = PP.text "N/A"
    showElems xs = PP.vcat [showDs, showCs, showFs]
      where 
        -- Split into (Fun, Class, Data).
        ((fs, tys), cs, ds) = foldr splitShowModuleElems (([], []), [], []) xs

        -- Data.
        showDs | null ds   = PP.empty 
               | otherwise = PP.vcat 
                   [ PP.text "Data:"
                   , PP.nest 2 $ PP.vcat $ fmap PP.text $ sort ds
                   ]
        -- Class.
        showCs | null cs   = PP.empty 
               | otherwise = PP.vcat 
                   [ PP.text "Class:"
                   , PP.nest 2 $ PP.vcat $ fmap PP.text $ sort cs
                   ]
        -- Fun.
        showFs | null fs   = PP.empty 
               | otherwise = PP.vcat 
                   [ PP.text "Fun:"
                   , PP.nest 2 $ PP.vcat $ fmap PP.text $ sort $ 
                       zipWith (\idt ty -> idt ++ " :: " ++ ty) (deggar fs) tys
                   ]

    -- Pretty printing for @[(Id, HsType)]@..
    showTypeableElems :: [(Id, HsType)] -> PP.Doc
    showTypeableElems [] = PP.text "N/A"
    showTypeableElems xs = PP.vcat $ fmap PP.text $ sort $ 
      zipWith (\idt ty -> idt ++ " :: " ++ prettyPrint ty) (deggar idts) tys
      where (idts, tys) = unzip xs

    -- Pretty printing for 'TestSuite's.
    showTestSuites :: [(Id, TestSuite)] -> PP.Doc 
    showTestSuites [] = PP.text "N/A"
    showTestSuites xs = PP.vcat $ fmap (uncurry showTestSuite) $ 
      sortBy (comparing fst) xs
      where 
        showTestSuite :: Id -> TestSuite -> PP.Doc 
        showTestSuite idt ts = PP.vcat 
          [ PP.text idt PP.<+> PP.text ":: TestSuite"
          , PP.nest 2 $ PP.vcat $ fmap PP.text (_progs ts)
          ]

    -- Invalids, need additional nesting for input errors: --------------------
    -- Don't forget to sort alphabetically. 

    showInvalidData :: [(Id, HsType, [InputError])] -> PP.Doc
    showInvalidData [] = PP.text "N/A"
    showInvalidData xs = PP.vcat $ fmap showInvalidDat $ 
      sortBy (comparing sel1) xs
      where 
        showInvalidDat :: (Id, HsType, [InputError]) -> PP.Doc
        showInvalidDat (idt, ty, errs) = PP.vcat 
          [ PP.text $ idt ++ " :: " ++ prettyPrint ty
          , PP.nest 2 $ PP.vcat $ fmap (PP.text . show) $ 
              sortBy (comparing show) errs 
          ]

    showInvalidTestSuites :: [(Id, [InputError])]  -> PP.Doc 
    showInvalidTestSuites [] = PP.text "N/A"
    showInvalidTestSuites xs = PP.vcat $ fmap showInvalidTestSuite $ 
      sortBy (comparing fst) xs
      where 
        showInvalidTestSuite :: (Id, [InputError]) -> PP.Doc 
        showInvalidTestSuite (idt, errs) = PP.vcat 
          [ PP.text idt PP.<+> PP.text ":: TestSuite"
          , PP.nest 2 $ PP.vcat $ fmap (PP.text . show) $ 
              sortBy (comparing show) errs 
          ]

    -- Helpers:

    -- Split the 'ModuleElem's to display 'Fun' types by the side of 'Fun' 
    -- identifiers.
    -- (The 'Class' and 'Data' 'ModuleElem's don't have typing information.)
    splitShowModuleElems 
      :: (ModuleElem, Maybe TypeString)
      -> (([String], [String]), [String], [String]) 
      -> (([String], [String]), [String], [String])
    -- Types.
    splitShowModuleElems (Fun idt, Just ty) ((fs, tys), cs, ds) = 
      ((idt : fs, ty : tys), cs, ds)
    -- No types.
    splitShowModuleElems (Fun idt, Nothing) ((fs, tys), cs, ds) = 
      ((idt : fs, "" : tys), cs, ds) -- Shouldn't happen.
    splitShowModuleElems (Class idt _, _) (fs, cs, ds) = (fs, idt : cs, ds)
    splitShowModuleElems (Data idt _, _)  (fs, cs, ds) = (fs, cs, idt : ds)

-- | Pretty printing for the 'SimpleReport' data structure. 
docSimpleReport :: SimpleReport -> PP.Doc 
docSimpleReport sr = PP.vcat $
  [ PP.text (_name sr)
  , PP.nest 2 $ ppSizeRuntime (_size sr) (_runtime sr)
  ]
  where 
    ppSizeRuntime (SizeUn n) d = PP.char '(' PP.<> PP.int n PP.<> PP.text ", " 
      PP.<> PP.double d PP.<> PP.char ')'
    ppSizeRuntime (SizeBin n1 n2) d = PP.char '(' PP.<> PP.int n1 PP.<> 
      PP.text ", " PP.<> PP.int n2 PP.<> PP.text ", " PP.<> 
      PP.double d PP.<> PP.char ')'

-- | Full pretty printing for 'BenchReport' data structure.
docBenchReport :: BenchReport -> PP.Doc 
docBenchReport br = PP.vcat
  [ PP.hcat $ (PP.text "Test programs: ") : (PP.punctuate (PP.text ", ") $
      fmap PP.text $ _bProgs br)
  , PP.text "Data options:" PP.<+> PP.text (show $ _bDataOpts br)
  , PP.text "Normal form:" PP.<+> PP.text (show $ _bNf br)
  , ppGhcFlags (_bGhcFlags br)
  , PP.text ""
  , PP.text "Reports:" PP.$$ PP.nest 2 (PP.vcat $ fmap ppTestResults $ _reports br)
  , ppBaselines (_baselines br)
  ]
  where 
    -- Pretty print list of GHC flags.
    ppGhcFlags :: [String] -> PP.Doc 
    ppGhcFlags [] = PP.empty
    ppGhcFlags flags = PP.vcat $ (PP.text "GHC flags: ") : 
      (PP.punctuate (PP.text ", ") $ fmap PP.text $ flags)
    
    -- Pretty print 'SimpleReport's belonging to same test program.
    ppTestResults :: [SimpleReport] -> PP.Doc 
    ppTestResults [] = PP.empty
    ppTestResults srs = PP.vcat $ 
      [ PP.text (_name $ head srs)
      , PP.nest 2 $ PP.vcat $ 
          fmap (\sr -> ppSizeRuntime (_size sr) (_runtime sr)) srs
      ]

    -- Pretty print baseline measurements.
    ppBaselines :: [SimpleReport] -> PP.Doc
    ppBaselines []  = PP.empty
    ppBaselines bls = PP.text "Baseline measurements:" PP.$$ (PP.nest 2 $ 
      PP.vcat $ fmap (\bl -> ppSizeRuntime (_size bl) (_runtime bl)) bls)

    -- PrettyPrint (input size(s), runtime) 2- or 3-tuples.
    ppSizeRuntime :: DataSize -> Double -> PP.Doc
    ppSizeRuntime (SizeUn n) d = PP.char '(' PP.<> PP.int n PP.<> PP.text ", " 
      PP.<> PP.double d PP.<> PP.char ')'
    ppSizeRuntime (SizeBin n1 n2) d = PP.char '(' PP.<> PP.int n1 PP.<> 
      PP.text ", " PP.<> PP.int n2 PP.<> PP.text ", " PP.<> 
      PP.double d PP.<> PP.char ')'