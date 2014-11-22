module Utils where

import System.IO
import System.IO.Unsafe

vlog :: (Show a) => a -> a
vlog var = unsafePerformIO $ do
    print  var
    return var

slog :: (Show a) => String -> a -> a
slog s var = unsafePerformIO $ do
    putStrLn $ s ++ ": " ++ show var
    return var
