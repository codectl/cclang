module Stack where

data Stack a = Stack [a] deriving (Eq, Show)

dup :: Stack a -> Stack a
dup (Stack []) = Stack []
dup (Stack (x:xs)) = Stack (x:x:xs)

swap :: Stack a -> Stack a
swap (Stack []) = Stack []
swap (Stack [x]) = Stack [x]
swap (Stack (x:y:xs)) = Stack (y:x:xs)

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x:xs)) = Just x

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

size :: Stack a -> Int
size (Stack xs) = length xs

nil :: Stack a -> Stack a
nil (Stack xs) = Stack xs