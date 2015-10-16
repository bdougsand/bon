module Main where

import Graphics.Gloss

makeCloser incChar decChar = \c acc -> acc + (if incChar == c then 1 else if decChar == c then -1 else 0)

-- Found close character:
doFindClose _ 0 pos _ = pos
-- Close character not found:
doFindClose _ _ _ [] = -1
doFindClose closer acc pos (c:cs) =
            doFindClose closer (closer c acc) (pos+1) cs

findClose incChar decChar cs = doFindClose (makeCloser incChar decChar) 1 (-1) cs

splitExclusive (-1) xs = (xs, [])
splitExclusive pos xs = (take pos xs, drop (pos+1) xs)



drawString [] = Blank
drawString ('F':cs) = (Pictures [(Line [(0, 0), (0, 10)]), (Translate 0 10 (drawString cs))])
drawString ('G':cs) = (Translate 0 10 (drawString cs))
drawString ('+':cs) = (Rotate 45 (drawString cs))
drawString ('-':cs) = (Rotate (-45) (drawString cs))
drawString ('[':cs) = Pictures [(drawString inside), (drawString outside)]
                    where off = findClose '[' ']' cs
                          (inside, outside) = splitExclusive off cs
drawString (']':cs) = error "Unmatched ]"
--drawString (_:cs) = drawString cs
drawString (n:cs) = error "Unknown directive"

draw cs = (color red (drawString cs))


main = display (InWindow "Turtle" (200, 200) (10, 10)) white (draw "FF+F-F-F[GGGF++F++F]F")
