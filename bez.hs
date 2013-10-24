data Point = Point Int Int deriving Show
type Vector = Point

data CubicBezier = Cubic Point Point Point Point
