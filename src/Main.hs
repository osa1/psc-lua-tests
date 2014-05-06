
module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import qualified Data.Set            as S
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO.Error
import           System.Process
import           System.Random

import           Test.HUnit.Base
import           Test.HUnit.Text
import           Test.Tasty
import           Test.Tasty.HUnit

randomSelect :: [a] -> IO a
randomSelect lst =
    (!!) lst <$> randomRIO (0, length lst - 1)

generateTestFolder :: IO FilePath
generateTestFolder = do
    tmp <- getTemporaryDirectory
    folderName <- (++) "psc-lua" <$> replicateM 10 (randomSelect validLetters)
    return $ tmp </> folderName
  where
    validLetters = ['a'..'z'] ++ ['0'..'9']

runPassingTest :: FilePath -> Maybe FilePath -> IO ()
runPassingTest pgmPath outPath = do
    testFolder <- generateTestFolder
    ret <- tryIOError $ createDirectory testFolder
    case ret of
      Left err
        | isAlreadyExistsError err -> runPassingTest pgmPath outPath
        | otherwise -> ioError err
      _ -> do
        setCurrentDirectory testFolder
        (exit, stdout, stderr) <-
          readProcessWithExitCode "psc" ["--lua", "--main", "Main", pgmPath] ""
        assertBool ("exit code failure\nstderr:\n" ++ stderr) (exit == ExitSuccess)
        (exit', stdout', stderr') <-
          readProcessWithExitCode "lua" [testFolder </> "Main.lua"] ""
        assertBool ("exit code failure\nstderr:\n" ++ stderr') (exit' == ExitSuccess)
        case outPath of
          Nothing -> return ()
          Just outFile -> do
            outContents <- readFile outFile
            assertEqual "unexpected program output" outContents stdout'

getPassingTests :: IO [(FilePath, Maybe FilePath)]
getPassingTests = do
    curDir <- getCurrentDirectory
    dirContents <- S.fromList <$> getDirectoryContents passingTestFolder
    let prefix = curDir </> passingTestFolder
        pursFiles = S.filter ((==) ".purs" . takeExtension) dirContents
        pursWOutput = flip map (S.toList pursFiles) $ \p ->
          let outFile = dropExtension p <.> "out"
          in (prefix </> p
             ,if S.member outFile dirContents then Just (prefix </> outFile) else Nothing)
    return pursWOutput
  where
    passingTestFolder = "examples/passing"

getPassingTestSuite :: IO TestTree
getPassingTestSuite = do
    passingTests <- getPassingTests
    let assertions = map (\(pgmPath, outPath) ->
          testCase pgmPath $ runPassingTest pgmPath outPath) passingTests
    return $ testGroup "Passing tests" assertions

main :: IO ()
main = do
    passingTests <- getPassingTestSuite
    defaultMain $ adjustOption (const $ mkTimeout 5000000) $ testGroup "tests" [passingTests]
