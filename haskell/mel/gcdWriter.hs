import Control.Monad.Writer
import Control.Monad.State

(%) = mod

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b | b == 0 = do tell ["Finished with " ++ show a]
                       return a
         | otherwise = do tell [show a ++ " mod " ++ show b ++ " = " ++ show (a % b)]
                          gcd' b (a % b)

stackStuff :: State Stack ()
stackStuff = do
  a <- pop
  if a == 5
    then push 5
    else do push 3
            push 8