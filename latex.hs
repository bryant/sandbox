import Data.Ratio (numerator, denominator)
import Data.Monoid ((<>))
import Data.List (intersperse)

newtype MathTex = MathTex { to_str :: String }

surround l r expr = MathTex l <> expr <> MathTex r

curlies = surround "{" "}"

parens = surround "\\left(" "\\right)"

raw = MathTex

var = raw

binop op a b = curlies a <> raw op <> curlies b

unop op expr = raw op <> curlies expr

instance Show MathTex where show = to_str

instance Monoid MathTex where
    mempty = MathTex ""
    mappend a b = MathTex $ to_str a `mappend` to_str b

instance Num MathTex where
    fromInteger = raw . show
    (+) = binop "+"
    (-) = binop "-"
    (*) = binop " "  -- mult is space
    abs = surround "\\lvert" "\\rvert"
    negate expr = raw "-" <> curlies expr
    signum _ = error "why are you using signum on latex, good sir?"

instance Fractional MathTex where
    fromRational k = numer / denom
        where
        numer = fromInteger . numerator $ k
        denom = fromInteger . denominator $ k

    a / b = raw "\\frac" <> curlies a <> curlies b

instance Floating MathTex where
    pi = MathTex "\\pi"
    exp k = var "e" ** k
    log = unop "\\log"
    sin = unop "sin"
    cos = unop "cos"
    tan = unop "tan"
    (**) = binop "^"
    --asin = asin
    --acos = acos
    --atan = atan

vector v = raw "\\mathbf" <> curlies (var v)

sum_ from to expr = raw "\\sum_" <> lower <> raw "^" <> upper <> curlies expr
    where
    lower = curlies $ maybe mempty id from
    upper = curlies $ maybe mempty id to

infixl 1 =.
(=.) = binop "="

f `of_` args = f <> parens xs
    where xs = mconcat $ intersperse (raw ", ") args

boltzmann = p `of_` [v] =. boltz v / sum_ (Just u) Nothing (boltz u)
    where
    p = var "p"
    v = vector "v"
    u = vector "u"
    boltz r = exp $ - (e `of_` [r]) / (boltzconst * temp)
        where
        e = var "E"
        boltzconst = var "k"
        temp = var "T"

paraboloid = f `of_` [x, y] =. a * x ** 2 + b * y ** 2 + c
    where
    [f, a, b, c, x, y] = map var $ words "f a b c x y"
