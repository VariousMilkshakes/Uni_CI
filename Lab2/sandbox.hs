module Main(main) where
    import Control.Concurrent
    import Control.Exception

    timeoutIterate msec f x = do
        mvar    <- newMVar x

        let loop    = do
            x   <- takeMVar mvar
            evaluate (f x) >>= putMVar mvar
            loop
        
        thread  <- forkIO loop
        threadDelay msec

        u       <- takeMVar mvar
        killThread thread

        return u

    count   :: Integer  -> Integer
    count x =
        x + 1

    main = do
        result <- timeoutIterate 10000000 (count) 1
        
        print result