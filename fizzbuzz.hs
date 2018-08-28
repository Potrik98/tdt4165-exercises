r = [(3, "Fizz"), (5, "Buzz")]

fizzBuzz :: [Integer] -> [String]
fizzBuzz l = map (\a -> let s = concat [if a `mod` fst t == 0 then snd t else "" | t <- r] in if null s then show a else s) l
