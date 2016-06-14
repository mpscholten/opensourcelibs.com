import qualified WebServer
import System.Environment (lookupEnv)

main = do
    port <- readPort
    putStrLn ("Starting server at *:" ++ show port)
    WebServer.run port

readPort = do
    value <- lookupEnv "PORT"
    return $
        case value of
            Just thePort -> read thePort
            Nothing -> defaultPort

defaultPort =
    8080
