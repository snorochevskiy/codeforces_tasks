-- http://codeforces.com/problemset/problem/805/A

-- Тавак И Саид — хорошие друзья.
-- Саид очень смешной, сегодня он попросил Тавака решить следующую задачу вместо задачи о самом длинном пути.
-- Даны l и r. Для каждого целого числа между l и r, включительно, выпишем все их целочисленные делители кроме 1.
-- Найдите число, которое мы выписали максимальное число раз.
-- Решите эту задачу, чтобы показать, что она не является NP-полной.

-- Входные данные: Первая строка содержит два целых числа l и r (2 ≤ l ≤ r ≤ 109).
-- Выходные данные: Выведите одно целое число: число, которое встречается в выписанных делителях максимальное число раз.
-- Если существует несколько ответов, выведите любой.

-- Пример 1
-- Входные данные:19 29
-- Выходные данные:2

-- Пример 2
-- Входные данные:3 6
-- Выходные данные:3

import Data.List
import Data.Ord
import Data.Char (digitToInt)

main = do
    line <- getLine
    let params = map (\s->read s::Int) $ words line
    putStr $ show $ findMostOftenDivider (params!!0) (params!!1)

-- NP full solution
findMostOftenDivider :: Int -> Int -> Int
findMostOftenDivider l r = snd $
    foldl1 max $                                                            -- get a tuple with maximum first element
    map (\all@(firstEl:restEl)->(length all, firstEl)) $                    -- costruct tuples (occurrences, divider)
    group . sort $                                                          -- sort and group dividers
    [n1 | n1 <- [l..r]] >>= (\el -> [n2 | n2 <- [2..el], el `mod` n2 == 0]) -- get all dividers

-- To make things clear:
-- 1) group $ sort [5,3,6,9,1,5,1,2,1]
--    [[1,1,1],[2],[3],[5,5],[6],[9]]
-- 2) map (\all@(firstEl:restEl)->(length all, firstEl)) [[1,1,1],[2],[3],[5,5],[6],[9]]
--    [(3,1),(1,2),(1,3),(2,5),(1,6),(1,9)]
-- 3) foldl1 max [(3,1),(1,2),(1,3),(2,5),(1,6),(1,9)]
--    (3,1)


