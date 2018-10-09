{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}

module Main(main) where
    import qualified Data.ByteString.Lazy as BSL
    import Data.Csv
    import qualified Data.Vector as V -- High Performance Arrays
    import GHC.Generics
    import System.Random

    -- Data structures for generating routes
    data Node   = Node String Float Float deriving (Show)
    data Route  = Route (V.Vector Node) deriving (Show)
    -- data Routes = Routes (V.Vector Node) deriving (Show)
    data City   = City String Float Float deriving Generic

    instance FromRecord City

    print_ (City id x y) =
        putStrLn (id ++ " city loaded")

    parseRoute (id, x, y) =
        Node id x y

    run route =
        putStrLn route

    generateGraph :: BSL.ByteString -> IO (Route)
    generateGraph csvData =
        case decode NoHeader csvData of
            Left err    -> error err
            Right cities -> return (parseCities cities)
        where parseCities cities = Route (V.map (\(id, x, y) -> Node id x y) cities -- Probably redundant

    main    = do
        -- BSL.writeFile "u16.csv" (encode cities)
        csvData <- BSL.readFile "u16.csv"
        graph   <- generateGraph csvData

        putStr graph