module Database where
    import Database.PostgreSQL.Simple.FromRow
    import qualified Database.PostgreSQL.Simple as PG
    import Safe (headMay)
    import Data.Text
    import Platform (Platform (Platform))
    import qualified Platform
    import Problem (Problem (Problem))
    import qualified Problem
    import Library (Library (Library))
    import qualified Library
    import Database.PostgreSQL.Simple.FromRow

    connect =
        PG.connect PG.defaultConnectInfo { PG.connectUser = "marc", PG.connectDatabase = "opensourcelibs" }

    instance PG.FromRow Platform where
        fromRow = Platform <$> field <*> field

    instance PG.FromRow Problem where
        fromRow = Problem <$> field <*> field <*> field

    instance PG.FromRow Library where
        fromRow = Library <$> field <*> field <*> field <*> field <*> field <*> field

    findPlatformByName :: PG.Connection -> Text -> IO (Maybe Platform)
    findPlatformByName db name = do
            results <- PG.query db theQuery theParams
            return $ headMay results
        where
            theQuery = "select id, name from platforms where name = ?"
            theParams = [name]

    findProblemByName :: PG.Connection -> Text -> IO (Maybe Problem)
    findProblemByName db name = do
            results  <- PG.query db theQuery theParams
            return $ headMay results
        where
            theQuery = "select id, platformId, name from problems where name = ?"
            theParams = [name]

    findLibraries :: PG.Connection -> Platform -> Problem -> IO ([Library])
    findLibraries db platform problem = do
            PG.query db theQuery theParams
        where
            theQuery = "select id, platformId, name, description, githubName, stars from (select libraries.* from libraries, librariesProblems where libraries.platformid = ? and libraries.id = librariesProblems.libraryId and librariesProblems.problemId = ?) as a"
            theParams = [Platform.id platform, Problem.id problem]

