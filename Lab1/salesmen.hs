{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}

module Main(main) where
    import System.Random
    import qualified Data.ByteString.Lazy as BSL
    import Data.Csv
    import qualified Data.Vector as V -- High Performance Arrays
    import Data.Matrix as M
    import Data.List as L
    import Data.Maybe
    import GHC.Generics
    import System.Environment

    -- Stop recursion from running off accidentally
    runOffMax   = 1000

    -- Data structures for generating routes
    data Node   = Node {
        nodeId  :: String,
        nodeX   :: Float,
        nodeY   :: Float
    } deriving (Show, Generic)
    data Route  = Route (V.Vector Node) deriving (Show)

    instance FromRecord Node

    -- Route helpers
    routeLength     :: Route -> Int
    routeLength (Route route) = V.length route

    routeNodes      :: Route -> V.Vector Node
    routeNodes (Route route) = route

    -- Swap 2 nodes in route
    shuffleValues   :: Int -> Route -> Route -> (Route, Route)
    shuffleValues node (Route stdRoute) (Route randRoute) =
        let (front,back)    = V.splitAt node stdRoute
            newRoute        = V.snoc randRoute (stdRoute V.! node)
        in
            (Route (front V.++ (V.tail back)), Route newRoute)

    -- Recursive generation of random, legal route
    shuffle         :: StdGen -> Route -> Route -> Int -> Route
    shuffle g original newRoute loop
        | (randLength == 0) && (stdLength == 0) = error "No Default Route"
        | stdLength > 0 && loop <= runOffMax    =
            let (leftOver, shuffled)    = shuffleValues randomNumber original newRoute
            in shuffle newGen leftOver shuffled (loop+1)
        | otherwise                             = newRoute
        where
            stdLength               = routeLength original
            randLength              = routeLength newRoute
            (randomNumber, newGen)  = randomR (0, stdLength-1) g :: (Int, StdGen)

    -- Recursively follow provided path, returning its length
    getRoute        :: Matrix Float -> [Int] -> Float
    getRoute map path
        | (length path) < 2 = 0
        | otherwise         =
            let current = path!!0
                next    = path!!1
            in (M.getElem current next map) + getRoute map (tail path)

    generateGraph   :: BSL.ByteString -> IO (Route)
    generateGraph csvData =
        case decode NoHeader csvData of
            Left err    -> error err
            Right cities -> return (parseCities cities)
        where parseCities cities = Route (V.map (\(id, x, y) -> Node id x y) cities) -- Probably redundant

    distanceMatrix  :: Route -> Matrix Float
    distanceMatrix (Route graph) =
        let size    = V.length graph
        in M.matrix size size (\(i, j) -> (calDistance i j graph))
    
    calDistance     :: Int -> Int -> V.Vector Node -> Float
    calDistance from to nodes
            | from == to    = 0
            | otherwise     =
                let aX  = nodeX (nodes V.! (from-1))
                    aY  = nodeY (nodes V.! (from-1))
                    bX  = nodeX (nodes V.! (to-1))
                    bY  = nodeY (nodes V.! (to-1))
                in sqrt $ (bX - aX)^2 + (bY - aY)^2

    runTests         :: StdGen -> Matrix Float -> Route -> Float -> Int -> (Float, [Int])
    runTests gen routes nodes shortest runs
        | runs <= 0 = (1000, [])
        | otherwise =
            let (_, newGen)     = random gen :: (Int, StdGen)
                (length, nPath) = runTest newGen routes nodes
                (best, path)    = runTests newGen routes nodes shortest (runs-1)
            in  if (isBest length best) then (length, nPath) else (best, path)
            where isBest thisResult prevBest = thisResult < prevBest
    
    runTest          :: StdGen -> Matrix Float -> Route -> (Float, [Int])
    runTest gen routes nodes    =

        let shuffled    = shuffle gen nodes (Route V.empty) 0
            pathNodes   = graphNodes shuffled
            path        = pathNodes ++ [pathNodes!!0]
        in  (getRoute routes path, path)

        where graphNodes (Route graph) = V.toList (V.map (\node -> (read (nodeId node) ::Int)) graph)

    main    = do
        args    <- getArgs
        csvData <- BSL.readFile "u16.csv"
        graph   <- generateGraph csvData
        gen     <- newStdGen

        let runs                = read $ args !! 0
            routes              = distanceMatrix graph
            (shortest, path)    = runTests (mkStdGen 1) routes graph 1000 runs
        
        print $ "route: " ++ show path ++ " -> " ++ show shortest ++ " is the shortest route found"