{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import Data.Yaml(decodeEither)
import System.Directory(getCurrentDirectory, setCurrentDirectory, makeRelativeToCurrentDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath(takeDirectory,(</>))
import System.Environment(getArgs,setEnv)
import System.IO(hPutStrLn, stderr)
import System.Exit(exitWith,exitFailure,exitSuccess, ExitCode(..))
import Control.Monad
import qualified Data.ByteString as B
import System.Process(runProcess, waitForProcess)
import System.Posix.Process(executeFile)
import Data.Foldable(toList)
import Config

main :: IO ()
main = do
  (configPathOption, args) <- processArgs =<< getArgs
  configPath <- case configPathOption of
    Nothing -> discoverConfig
    Just x -> return x
  configData <- B.readFile configPath
  let yamlE = decodeEither configData
  yaml <- case yamlE of
            Left e -> do
              diagLn ("np: Could not parse " ++ quote configPath ++ ": " ++ e)
              exitFailure
            Right x -> return x

  pivotWorkingDirectory (takeDirectory configPath)

  createDirectoryIfMissing True ".nix-project/cache"

  let tool = nixProjectTool yaml

  when (length (location tool) + length (expression tool) /= 1) $ do
    diagLn ("np: Exactly one of ‘expression’ or ‘location’ must be provided in " ++ quote configPath)
    exitFailure
  
  let pathArgs = case location tool of
        -- git
        Just (IsL l) -> ["--expr", "import ((import <nixpkgs> {}).fetchgit { url = "
                   ++ (escape (git l)) ++ "; rev = "
                   ++ (escape (rev l)) ++ "; sha256 = "
                   ++ (escape (sha256 l)) ++ "; }) {}"
                 ]
        Just (IsR path) -> [path]
        Nothing ->
          -- pattern binding is valid: see check above
          let (Just expr) = expression tool
          in ["--expr", expr]

  let attributeArgs = toList (attribute tool) >>=
                         \attr -> [ "-A", attr ]
                         
  let outputArgs = [ "-o", ".nix-project/cache/tool" ]
  
  let nixBuildArgs = outputArgs ++ attributeArgs ++ pathArgs
  
  p <- runProcess "nix-build" nixBuildArgs Nothing Nothing Nothing Nothing Nothing
  
  e <- waitForProcess p
  
  when (e /= ExitSuccess) $ do
    diagLn "np: the build tool could not be built"
    exitWith e

  executeFile ".nix-project/cache/tool/bin/-nix-project-tool" False args Nothing

-- | Nix string escaping
escape :: String -> String
-- Nix manual:
--   The most common way is to enclose the string between
--   double quotes, e.g., "foo bar". Strings can span multiple
--   lines. The special characters " and \ and the character sequence ${
--   must be escaped by prefixing them with a backslash (\). Newlines,
--   carriage returns and tabs can be written as \n, \r and \t,
--   respectively.
--
-- Let's also do the \n \r \t for good measure.
escape s = "\"" ++ f s ++ "\""
  where f = concatMap g
        g '"' = "\\\""
        g '\\' = "\\\\"
        g '$' = "\\$" -- escaping only "${" would be sufficient but escaping every '$' does no harm
        g '\n' = "\\n"
        g '\r' = "\\r"
        g '\t' = "\\t"
        g x = x : []

-- | `cd`s to the directory of the passed path
--
-- Sets the NIXPROJECT_FOCUS environment variable
pivotWorkingDirectory :: FilePath -> IO ()
pivotWorkingDirectory newDir = do
  oldDir <- getCurrentDirectory
  setCurrentDirectory newDir
  pathToOld <- makeRelativeToCurrentDirectory oldDir
  setEnv "NIXPROJECT_FOCUS" pathToOld



---- Command line interface

helpText :: String
helpText = unlines
  [ "Usage:"
  , ""
  , "  np --help           This help text for the generic np tool"
  , ""
  , "  np                  Help text for the project"
  , "  np help"
  , ""
  , "  np COMMAND          Run any COMMAND according to project configuration"
  , ""
  , "  np -f FILE COMMAND  Run any COMMAND according to alternate"
  , "                      project configuration as specified in FILE"
  , ""
  , "The topmost " ++ quote defaultName ++ " file, relative to the current"
  , "working directory is consulted first, to determine which build tool"
  , "to delegate to."
  , "It is widely believed that this command should not be aliased to ‘p’."
  ]

-- | Parse arguments, exiting on error or help
processArgs :: [String] -> IO (Maybe String, [String])
processArgs args = do
  case args of
    "--help" : _ -> do
      diagLn helpText
      exitSuccess
    "-help" : _ -> do
      diagLn helpText
      exitSuccess
    "-h" : _ -> do
      diagLn helpText
      exitSuccess
    ["-f"] -> do
      diagLn "np: -f requires an argument"
      exitFailure
    "-f" : path : more -> return (Just path, more)
    ('-':'f':path) : more -> return (Just path, more)
    o@('-':_) : _ -> do
      diagLn ("np does not support option " ++ quote o)
      exitFailure
    more -> return (Nothing, more)

-- | Diagnostic output
diagLn :: String -> IO ()
diagLn = hPutStrLn stderr

quote :: String -> String
quote x = "‘" ++ x ++ "’"

-- | Name of the config file to look for
defaultName :: FilePath
defaultName = "nix-project.yaml"

discoverConfig :: IO FilePath
discoverConfig = do
  cwd <- getCurrentDirectory
  projectDirs <- filterM isProjectDir (reverse (selfAndAncestry cwd))
  case projectDirs of
    [] -> do
      diagLn ("np: could not find " ++ quote defaultName ++
             " in the working directory or its directory ancestry")
      exitFailure
    projectDir : redundantDirs -> do
      warnRedundantDirs projectDir redundantDirs
      return (dirToConfig projectDir)

-- | The path and all its parents, parents' parents etc.
selfAndAncestry :: FilePath -> [FilePath]
selfAndAncestry fp | takeDirectory fp == fp  = [fp]
selfAndAncestry fp                           = fp : selfAndAncestry (takeDirectory fp)

-- | Does the yaml file exist there?
isProjectDir :: FilePath -> IO Bool
isProjectDir dir = do
  doesFileExist (dirToConfig dir)

-- | Warn when ignoring config files.
--
-- Maybe there should not be warnings? Remove this in the future?
warnRedundantDirs :: FilePath -> [FilePath] -> IO ()
warnRedundantDirs _picked [] = mempty
warnRedundantDirs picked redundant = do
  diagLn ("np: using " ++ quote (dirToConfig picked))
  forM_ redundant (\dir -> diagLn ("np: skipping " ++ quote (dirToConfig dir)))

-- | Directory to config file path
dirToConfig :: FilePath -> FilePath
dirToConfig dir = (dir </> defaultName)
