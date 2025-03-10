import WhileModule
import System.IO ( hGetContents, withFile, IOMode(ReadMode), isEOF )
import System.Environment (getArgs)
import System.Directory (doesFileExist)

{- CI INTEGRATION -}

main :: IO ()
main = do 
  args <- getArgs
  case length args of
    -- REPL
    0 -> do 
      showTitle
      putStrLn "REPL mode - code to your hearts content"
      putStrLn "If you want to execute a file instead, provide it's path as an argument to this executable"
      putStrLn ""
      repl []
      putStrLn "Bye :)"
    -- FILE  
    1 -> do 
      let [arg1] = args
      case arg1 of 
        "--help" -> showHelp
        "-h"     -> showHelp
        _        -> withFile arg1 ReadMode $ \handle -> do
          fe <- doesFileExist arg1
          if not fe then putStrLn $ "Error: Cannot find file at path " ++ arg1 else do
            whileProgramString <- hGetContents handle
            putStrLn $ runProgram whileProgramString 
    _ -> putStrLn "Either give no arguments for the repl, or only the file path to a while-program to execute it"
  where
  programHelper :: Bool -> String -> (While -> Either String AllocationList) -> (String -> String -> String) -> Either String (Either String (String,AllocationList))
  programHelper needAllocList code interpreter onError  = case getProgram code of 
    Left err -> Left $ onError err code
    Right program -> case interpreter program of 
      Left err2 -> Left $ err2
      Right allocList -> let result = "Results: " ++ (foldl (\acc (varname, value) -> acc ++ varname ++ " -> " ++ showLiteral value ++ ", ") "" allocList) in
        if needAllocList 
          then Right $ Right (result, allocList)
          else Right $ Left result
  runProgram :: String -> String
  runProgram code = case programHelper False code sos (\err _ -> err) of
    Left err -> err
    Right (Right _) -> "Internal Error"
    Right (Left a) -> a
  interpretProgram :: String -> AllocationList -> IO ()
  interpretProgram code alloc = do 
    case programHelper True code (sosrepl alloc) (\err code -> "Couldn't read program, so will try to read an expression:\n" ++ (interpretExpression alloc code)) of
      Left err -> do putStrLn err; repl alloc
      Right (Left _) -> do putStrLn "Internal Error"; repl alloc
      Right (Right (result, alloc2)) -> do
        putStrLn result
        repl alloc2
  interpretExpression :: AllocationList -> String -> String
  interpretExpression alloc code = case getExpression code of 
    Left err -> err
    Right exp -> case interpretGeneralExpression exp alloc of 
      Left err2 -> err2
      Right (Left int) -> code ++ " results in " ++ show (LitA int) 
      Right (Right bool) -> code ++ " results in " ++ show (LitB bool) 
  showLiteral :: Literal -> String
  showLiteral (Left  int ) = show (LitA int)
  showLiteral (Right bool) = show (LitB bool)
  repl :: AllocationList -> IO ()
  repl alloc = do
    done <- isEOF
    if done then return () else do
      code <- readProgram 
      interpretProgram code alloc
  readProgram :: IO String
  readProgram = do
    line <- getLine
    case line of
      'w':'h':'i':'l':'e':rest -> (readProgram >>= (\s -> case s of "" -> return line; _ -> return $ line ++ "\n" ++ s))
      'i':'f':rest             -> (readProgram >>= (\s -> case s of "" -> return line; _ -> return $ line ++ "\n" ++ s))
      'e':'l':'s':'e':rest     -> (readProgram >>= (\s -> case s of "" -> return line; _ -> return $ line ++ "\n" ++ s))
      '\t':rest                -> (readProgram >>= (\s -> case s of "" -> return line; _ -> return $ line ++ "\n" ++ s))
      []                       -> return ""
      _                        -> return line
  showTitle = do    
    putStrLn " __    __ _     _         __"
    putStrLn "/ / /\\ \\ \\ |__ (_)       / / "
    putStrLn "\\ \\/  \\/ / '_ \\| |_____ / /"
    putStrLn " \\  /\\  /| | | | |_____/ /__"
    putStrLn "  \\/  \\/ |_| |_|_|     \\____/"
    putStrLn ""
    putStrLn "A While language interpreter"
    putStrLn ""
  showHelp = do
    putStrLn "Usage:"
    putStrLn " 1.  WHILE-Language                    -- for REPL-mode"
    putStrLn " 2.  WHILE-Language [file path]        -- for execution of the file at the file path"
    putStrLn ""
    putStrLn "The REPL saves the assignment of variables for as long as the REPL runs."
    putStrLn "You can override the value of a variable with an assignment"
    putStrLn "To see the current variable allocations, use the \"skip\" command"
    putStrLn ""

{- DEBUG -}
showFile :: String -> IO ()
showFile filePath = do
  withFile filePath ReadMode $ \handle -> do
    file <- hGetContents handle
    putStrLn $ show file
