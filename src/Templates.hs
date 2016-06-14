{-# LANGUAGE NoImplicitPrelude #-} 

module Templates where
    import BasicPrelude
    import Text.Blaze.Html5 as H
    import Text.Blaze.Html5.Attributes as A
    import Control.Monad (forM_)
    import qualified Data.Aeson as JSON
    import qualified Data.ByteString.Lazy

    import qualified Library

    layout selectedPlatform selectedProblem pageContent =
        docTypeHtml ! lang "en" $ do
            H.head $ do
                meta ! charset "UTF-8"
                meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge"
                meta ! name "viewport" ! content "width=device-width, initial-scale=1"
                H.title "opensourcelibs: Find the right open source libraries for your project"
                link ! rel "stylesheet" ! href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
                link ! rel "stylesheet" ! href "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.10.0/css/bootstrap-select.min.css"
                link ! rel "stylesheet" ! href "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"
                link ! rel "stylesheet" ! href "/main.css"
                H.style ".bootstrap-select { margin-left: 12px !important; margin-right: 12px; }"
                script ! src "https://code.jquery.com/jquery-2.2.4.min.js" $ mempty
                script ! src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js" $ mempty
                script ! src "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.10.0/js/bootstrap-select.min.js" $ mempty
                script ! src "/main.js" $ mempty
            body $ do
                H.div ! class_ "container" $ do
                    h2 ! A.style "text-align:center; padding-bottom: 120px;" $ do
                        a "opensourcelibs.com" ! A.style "text-decoration: none !important; color: #333" ! A.href "/"
                        small "Find the right open source library for your problem" ! A.style "margin-left: 12px"
                    h2 ! A.style "text-align: center" $ do
                        "I'm using"
                        select ! class_ "selectpicker" ! dataAttribute "live-search" "true" ! A.id "platform" $ do
                            forM_ Library.allPlatforms
                                (\platform ->
                                    let
                                        element = option $ text $ show platform
                                    in
                                        if platform == selectedPlatform then
                                            element ! A.selected "selected"
                                        else
                                            element
                                )
                        "and look for a library doing"
                        select ! class_ "selectpicker" ! dataAttribute "live-search" "true" ! A.id "problem" $ do
                            forM_ (Library.findProblemsByPlatform selectedPlatform)
                                (\problem ->
                                    let
                                        element = option $ text problem
                                    in
                                        if problem == selectedProblem then
                                            element ! A.selected "selected"
                                        else
                                            element
                                )
                script $ "window.platformsAndProblems = " `mappend` (unsafeByteString $ Data.ByteString.Lazy.toStrict $ JSON.encode (BasicPrelude.map (\(a, b) -> (show a, b)) Library.platformsWithProblems))
                pageContent

    landingPage = 
        layout Library.Haskell "prelude" mempty

    resultPage selectedPlatform selectedProblem libraries =
        layout selectedPlatform selectedProblem $ do
            H.div ! A.style "width: 600px; padding-top: 50px; margin-left: auto; margin-right: auto" $ do
                forM_ libraries entry
                H.div ! A.style "padding-top: 20px" $ do
                    a ! A.href "https://github.com/mpscholten/opensourcelibs.com/blob/master/README.md#adding-a-library" $ "Your library is missing? Just add it via GitHub"

    entry library = 
        H.div $ do
            H.div ! class_ "row" $ do
                H.div ! class_ "col-md-6" $ h3 $ do
                    text $ Library.name library
                    case Library.githubStars library of
                        Just stars ->
                            small ! A.style "margin-left: 22px" $ do
                                i ! class_ "fa fa-star" $ mempty
                                " " `mappend` (text $ show stars)
                        Nothing ->
                            mempty
                H.div ! class_ "col-md-6" $ H.div ! A.style "margin-top: 20px;" $ mempty
            H.div $ text $ Library.description library
            H.div ! A.style "margin-top: 10px" ! class_ "library-links" $ do
                case Library.codeSampleUrl library of
                    Just url -> 
                        a ! href (toValue url) $ do
                            i ! class_ "fa fa-code" $ mempty
                            " Code Sample"
                    Nothing -> mempty
                case Library.githubUrl library of
                    Just url -> 
                        a ! href (toValue url) $ do
                            i ! class_ "fa fa-github" $ mempty
                            " GitHub"
                    Nothing -> mempty
                case Library.websiteUrl library of
                    Just url ->
                        a ! href (toValue url) $ do
                            i ! class_ "fa fa-smile-o" $ mempty
                            " Website"
                    Nothing -> mempty
