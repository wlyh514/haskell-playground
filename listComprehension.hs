multTable = [[x * y | y <- [1..10]] | x <- [1..10]]

fib = 1 : 1 : [x + y | (x, y) <- zip fib (tail fib)]

prime x = null [y | y <- [2..(x-1)], x `mod` y == 0]
primes = filter prime [2 ..]