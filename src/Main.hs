
module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Char
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

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

runTestFile :: FilePath -> IO ()
runTestFile absPath = do
    testFolder <- generateTestFolder
    ret <- tryIOError $ createDirectory testFolder
    case ret of
      Left err
        | isAlreadyExistsError err -> runTestFile absPath
        | otherwise -> ioError err
      _ -> do
        setCurrentDirectory testFolder
        (exit, stdout, stderr) <-
          readProcessWithExitCode "psc" ["--lua", "--main", "Main", absPath] ""
        assertBool ("exit code failure\nstderr:\n" ++ stderr) (exit == ExitSuccess)
        (exit', stdout', stderr') <-
          readProcessWithExitCode "lua" [testFolder </> "Main.lua"] ""
        assertBool ("exit code failure\nstderr:\n" ++ stderr') (exit' == ExitSuccess)
        assertEqual "unexpected program output" "Done" (strip stdout')

getPassingTests :: IO [FilePath]
getPassingTests = do
    curDir <- getCurrentDirectory
    map ((curDir </> passingTestFolder) </>) . filter ((==) ".purs" . takeExtension)
      <$> getDirectoryContents (curDir </> passingTestFolder)
  where
    passingTestFolder = "examples/passing"

getPassingTestSuite :: IO TestTree
getPassingTestSuite = do
    passingTests <- getPassingTests
    let assertions = map (\path -> testCase path $ runTestFile path) passingTests
    return $ testGroup "Passing tests" assertions

main :: IO ()
main = do
    passingTests <- getPassingTestSuite
    defaultMain $ adjustOption (const $ mkTimeout 5000000) $ testGroup "tests" [passingTests]
