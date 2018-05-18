module Main where

import Prelude hiding (Maybe, Nothing, Just)
import Control.Applicative (Applicative)
import Control.Monad       (ap, return)

-------------------------------------------------------------------------------
-- I - Maybe Functor / Monad
-------------------------------------------------------------------------------

data Maybe a = Nothing | Just a deriving Show

instance Functor Maybe where
-- fmap :: (a -> b) -> (Maybe a -> Maybe b)
    fmap f Nothing = Nothing
    fmap f (Just a) = Just (f a)

instance Applicative Maybe where
    pure  = return
    (<*>) = ap

instance Monad Maybe where
    --  return :: a -> Maybe a
    return a = Just a
    -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    (>>=) Nothing k = Nothing
    (>>=) (Just a) k = k a

-- optional
(>=>) :: (b -> Maybe c) -> (a -> Maybe b) -> (a -> Maybe c)
(>=>) f g = \a -> (g a) >>= f

join :: Maybe(Maybe a) -> Maybe a
join Nothing = Nothing
join (Just(Just a)) = Just a

-------------------------------------------------------------------------------
-- I - Maybe Example 
-------------------------------------------------------------------------------

-- sqrt :: Float -> Float

inv :: Float -> Float
inv x = 1/x

-- Imperative, Impure pseudo code
-- safeInvSqrt x = try sqrt inv x catch error 

-- Pure, Kleisli arrow
safeInvSqrt1 x = if (x < 0 || x == 0)
                then Nothing
                else Just (sqrt (inv x))


-- Pure, with the Maybe Monad:

safeSqrt :: Float -> Maybe Float
safeSqrt x = if (x < 0)
            then Nothing
            else Just(sqrt x)

safeInv :: Float -> Maybe Float
safeInv x = if (x == 0)
            then Nothing
            else Just(inv x)

safeInvSqrt2 :: Float -> Maybe Float
safeInvSqrt2 x = safeSqrt x >>= safeInv

safeInvSqrt3 :: Float -> Maybe Float
safeInvSqrt3 = safeSqrt >=> safeInv

neg:: Float -> Float
neg x = -x

safeNegInvSqrt :: Float -> Maybe Float
safeNegInvSqrt x = fmap neg (safeInvSqrt3 x)


-------------------------------------------------------------------------------
-- II State Functor / Monad
-------------------------------------------------------------------------------

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
--    fmap :: (a -> b) -> (State s a -> State s b)
    fmap f (State g) = State $ \s ->
        let (a, s') = g s
        in (f a, s')

instance Applicative (State s) where
    pure  = return
    (<*>) = ap

instance Monad (State s) where
--  return :: a -> State s a
    return a = State $ \s -> (a, s)
-- (>>=) :: State s a -> (a -> State s b) -> State s b
    (>>=) (State f) k = State $ \s ->
        let (a, s') = f s
        in runState (k a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= \x -> put (f x)

evalState :: State s a -> s -> a
evalState f = fst . runState f

execState :: State s a -> s -> s
execState f = snd . runState f

-------------------------------------------------------------------------------
-- II - State Example
-------------------------------------------------------------------------------

-- Stack

type Stack = [Int]

emptyStack :: Stack
emptyStack = []

pop :: State Stack Int
pop = State $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((), a:xs)

topOfStack :: State Stack Int
topOfStack = State $ \(x:xs) -> (x, x:xs)

stackManip :: State Stack Int
stackManip = do
    push 10
    push 20
    a <- pop
    b <- pop
    push (a + b)
    topOfStack

stackManip2 = (push 10)
    >>= \_ -> (push 20)
    >>= \_ -> pop 
    >>= \a -> pop
    >>= \b -> push(a + b)
    >>= \_ -> topOfStack

main :: IO ()
main = do
    let res = evalState stackManip2 emptyStack
    print res
--    let res = safeNegInvSqrt (4)
--    print res