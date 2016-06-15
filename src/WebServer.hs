{-# LANGUAGE NoImplicitPrelude #-}

module WebServer (run) where
    import BasicPrelude

    import Network.Wai
    import Network.HTTP.Types
    import qualified Network.Wai.Handler.Warp as Warp
    import Network.Wai.Middleware.RequestLogger
    import Network.Wai.Middleware.Static
    import Text.Blaze.Renderer.Utf8
    import qualified Templates
    import Library

    run :: Warp.Port -> String -> IO ()
    run port publicDir = do
        Warp.run port (staticPolicy (addBase "src/public") $ logStdoutDev app)

    app :: Application
    app request respond = do
        response <- case pathInfo request of
                        [] ->
                            return $ html Templates.landingPage
                        [platformName, problemName] -> do
                            putStrLn $ show $ Library.findProblemsByPlatform Library.PHP
                            return $ html $ Templates.resultPage (read platformName) (problemName) $ Library.findLibraries (read platformName) problemName
                        _ -> notFound
        respond response

    notFound :: IO Response
    notFound =
        return $
            responseLBS status404 [("Content-Type", "text/plain")] "404 - Not Found"

    html template =
        responseLBS status200 [("Content-Type", "text/html")] $
            renderHtml template
