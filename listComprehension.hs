multTable = [[x * y | y <- [1..10]] | x <- [1..10]]

fib = 1 : 1 : [x + y | (x, y) <- zip fib (tail fib)]