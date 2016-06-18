{-# LANGUAGE NoImplicitPrelude #-} 

module Library where
    import BasicPrelude
    import qualified Data.List as List

    data Platform = Haskell | PHP
                  deriving (Eq, Show, Read, Enum)

    allPlatforms = [Haskell ..]
    allProblems = concat $ map tags libraries

    data Library = Library {
            --id :: Int,
            --platformId :: Int,
            platform :: Platform,
            name :: Text,
            description :: Text,
            githubName :: Maybe Text,
            githubStars :: Maybe Int,
            codeSampleUrl :: Maybe Text,
            websiteUrl :: Maybe Text,
            tags :: [Text]
        }

    githubUrl library =
        case githubName library of
            Just githubName -> Just $ "https://github.com/" `mappend` githubName
            Nothing -> Nothing

    libraries =
        [
            library {
                platform = Haskell,
                name = "BasicPrelude",
                description = "BasicPrelude mostly re-exports several key libraries in their entirety. The exception is Data.List, where various functions are replaced by similar versions that are either generalized, operate on Text, or are implemented strictly.",
                githubName = Just "snoyberg/basic-prelude",
                githubStars = Just 32,
                tags = ["prelude"]
            },
            library {
                platform = Haskell,
                name = "Blaze",
                description = "BlazeHtml is an HTML combinator library. It provides a way to embed HTML in Haskell in an efficient and convenient way, with a light-weight syntax",
                githubName = Just "jaspervdj/blaze-html",
                githubStars = Just 134,
                websiteUrl = Just "https://jaspervdj.be/blaze/",
                codeSampleUrl = Just "https://jaspervdj.be/blaze/tutorial.html",
                tags = ["templates", "html"]
            },
            library {
                platform = Haskell,
                name = "warp",
                description = "A fast, light-weight web server for WAI applications",
                githubName = Just "yesodweb/wai",
                githubStars = Just 355,
                tags = ["web-server"]
            },
            library {
                platform = Haskell,
                name = "http-conduit",
                description = "Library for doing http requests",
                codeSampleUrl = Just "https://github.com/commercialhaskell/jump/blob/master/doc/http-client.md#request-building",
                websiteUrl = Just "https://www.stackage.org/package/http-conduit",
                tags = ["http requests"]
            },
            library {
                platform = PHP,
                name = "Propel2",
                description = "Propel2 is an open-source high-performance Object-Relational Mapping (ORM) for modern PHP",
                githubName = Just "propelorm/Propel2",
                githubStars = Just 833,
                tags = ["orm", "orm (active record)"]
            },
            library {
                platform = PHP,
                name = "Doctrine2",
                description = "Data Mapper ORM",
                githubName = Just "doctrine/doctrine2",
                githubStars = Just 3079,
                tags = ["orm", "orm (data mapper)"]
            },
            library {
                platform = PHP,
                name = "Symfony Dependency Injection",
                description = "A service container",
                githubName = Just "symfony/dependency-injection",
                githubStars = Just 80,
                tags = ["dependency injection"]
            },
            library {
                platform = PHP,
                name = "unbox",
                description = "Unbox is a fast, simple, opinionated dependency injection container, with a gentle learning curve",
                githubName = Just "mindplay-dk/unbox",
                githubStars = Just 25,
                tags = ["dependency injection"]
            },
            library {
                platform = PHP,
                name = "Stringy",
                description = "A PHP string manipulation library with multibyte support",
                githubName = Just "danielstjules/stringy",
                githubStars = Just 1487,
                tags = ["string handling"],
                codeSampleUrl = Just "https://github.com/danielstjules/stringy#oo-and-chaining"
            }
        ]
        where
            library = Library { githubName = Nothing, githubStars = Nothing, websiteUrl = Nothing, codeSampleUrl = Nothing }

    findLibrariesByPlatform :: Platform -> [Library] -> [Library]
    findLibrariesByPlatform platform' = List.filter (\library -> platform library == platform')

    findProblemsByPlatform :: Platform -> [Text]
    findProblemsByPlatform platform' = nub $ List.concat $ map tags (findLibrariesByPlatform platform' libraries)

    platformsWithProblems :: [(Platform, [Text])]
    platformsWithProblems = map (\platform -> (platform, findProblemsByPlatform platform)) allPlatforms 

    findLibraries :: Platform -> Text -> [Library]
    findLibraries platform' problem =
            List.filter byPlatformAndProblem libraries
        where
            byPlatformAndProblem library =
                platform' == (platform library) && problem `elem` (tags library)
