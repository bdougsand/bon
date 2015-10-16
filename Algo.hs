module Main where

doSplitWith f [] splut = splut
doSplitWith f (x:xs) (accept, reject) = doSplitWith f xs
                                                    (if (f x)
                                                        then (accept ++ [x], reject)
                                                        else (accept, reject ++ [x]))

-- Given a predicate `f` and a list `xs` of elements x, returns a tuple
-- containing a list for which (f x) is True and a list for which (f x)
-- is False.
splitWith f xs = doSplitWith f xs ([], [])

-- Given two sorted lists, returns a sorted list from their elements.
merge xs [] = xs
merge [] ys = ys
merge xs@(x:_) ys@(y:_)
      | x <= y = [x] ++ merge (tail xs) ys
      | y < x = [y] ++ merge xs (tail ys)

-- Sorts an unsorted list using merge sort
mergeSort [] = []
mergeSort (x:[]) = [x]
mergeSort xs = merge (mergeSort listA) (mergeSort listB)
          where (listA, listB) = splitAt ((length xs) `div` 2) xs


quickSort [] = []
quickSort (x:xs) = (quickSort (filter (<= x) xs)) ++ [x] ++
                   (quickSort (filter (> x) xs))


quickSort2 [] = []
quickSort2 (x:xs) = (quickSort2 listA) ++ [x] ++ (quickSort2 listB)
           where (listA, listB) = splitWith (<= x) xs
