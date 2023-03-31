module Assert where

assert :: Show a => Bool -> a -> a 
assert False x = error ("assertion failed: " ++ show x)
assert _ x = x 

assertWith :: Show a => (a -> Bool) -> a -> a 
assertWith f x 
    | f x = x 
    | otherwise = error ("assertion failed" ++ show x)