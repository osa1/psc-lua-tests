{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import qualified Data.Set                  as S
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO.Error
import           System.Process
import           System.Random

import           Data.Tagged
import           Data.Typeable
import           Options.Applicative
import           Options.Applicative.Types hiding (Option)

import           Test.HUnit.Base
import           Test.HUnit.Text
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients
import           Test.Tasty.Options

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

newtype LuaExec = LuaExec { runLuaExec :: String } deriving (Typeable)

instance IsOption LuaExec where
    defaultValue = LuaExec "lua"
    parseValue s = Just $ LuaExec s
    optionName = Tagged "with-lua"
    optionHelp = Tagged "Use Lua executable"
    optionCLParser =
      nullOption
        (  reader parse
        <> long name
        <> short 'w'
        <> help helpString
        )
      where
        name = untag (optionName :: Tagged LuaExec String)
        helpString = untag (optionHelp :: Tagged LuaExec String)
        parse =
          ReadM .
          maybe (Left (ErrorMsg $ "Could not parse " ++ name)) Right .
          parseValue

runPassingTest :: LuaExec -> FilePath -> Maybe FilePath -> IO ()
runPassingTest luaExec pgmPath outPath = do
    testFolder <- generateTestFolder
    ret <- tryIOError $ createDirectory testFolder
    case ret of
      Left err
        | isAlreadyExistsError err -> runPassingTest luaExec pgmPath outPath
        | otherwise -> ioError err
      _ -> do
        setCurrentDirectory testFolder
        (exit, stdout, stderr) <-
          readProcessWithExitCode "psc-lua" ["--main", "Main", pgmPath] ""
        assertBool ("exit code failure\nstderr:\n" ++ stderr) (exit == ExitSuccess)
        (exit', stdout', stderr') <-
          readProcessWithExitCode (runLuaExec luaExec) [testFolder </> "Main.lua"] ""
        assertBool ("exit code failure\nstderr:\n" ++ stderr') (exit' == ExitSuccess)
        case outPath of
          Nothing -> return ()
          Just outFile -> do
            outContents <- readFile outFile
            assertEqual "unexpected program output" outContents stdout'

getPassingTestFiles :: IO [(FilePath, Maybe FilePath)]
getPassingTestFiles = do
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

getPassingTestSuite :: IO (LuaExec -> TestTree)
getPassingTestSuite = do
    passingTestFiles <- getPassingTestFiles
    return $ \lua ->
      let assertions = map (\(pgmPath, outPath) ->
            testCase pgmPath $ runPassingTest lua pgmPath outPath) passingTestFiles
      in testGroup "Passing tests" assertions

ingredients :: [Ingredient]
ingredients = includingOptions [Option (Proxy :: Proxy LuaExec)] : defaultIngredients

main :: IO ()
main = do
    passingTests <- getPassingTestSuite

    defaultMainWithIngredients ingredients $
      adjustOption (const $ mkTimeout 5000000) $
        testGroup "tests" [askOption passingTests]
