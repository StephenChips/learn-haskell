module Geo.Area (
    circle,
    square,
    triangle
) where

import SimpleMath

circle r = 2 * r * pi

square a = a * a

triangle a b c = sqrt (p * (p-a) * (p - b) * (p - c)) where
    p = (a + b + c) / 2

timeSquare area = (area * 4)