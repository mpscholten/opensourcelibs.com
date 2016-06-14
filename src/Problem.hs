module Problem where
    import Data.Text

    data Problem = Problem {
            id :: Int,
            platformId :: Int,
            name :: Text
        }
