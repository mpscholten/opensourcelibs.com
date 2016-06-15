import qualified WebServer
import System.Environment (lookupEnv)
import System.Directory (getCurrentDirectory)

main = do
    port <- readPort
    publicDir <- readPublicDir
    putStrLn ("publicDir = " ++ show publicDir)
    putStrLn ("Starting server at *:" ++ show port)
    WebServer.run port publicDir

readPort = do
    value <- lookupEnv "PORT"
    return $
        case value of
            Just thePort -> read thePort
            Nothing -> defaultPort

readPublicDir = do
    value <- lookupEnv "PUBLIC"
    currentWorkingDirectory <- getCurrentDirectory
    return $
        case value of
            Just thePublicDir -> thePublicDir
            Nothing -> currentWorkingDirectory `mappend` "/src/public"

defaultPort =
    8080
