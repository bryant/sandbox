import Data.Ratio (numerator, denominator)
import Data.Monoid ((<>))

newtype MathTex = MathTex { to_str :: String }

surround l r s = l ++ s ++ r

curlies = surround "{" "}"

parens = surround "\\left(" "\\right)"

var = MathTex

binop op (MathTex a) (MathTex b) = MathTex $ curlies a ++ op ++ curlies b

unop op (MathTex t) = MathTex $ op ++ curlies t

instance Show MathTex where show = to_str

instance Monoid MathTex where
    mempty = MathTex ""
    mappend a b = MathTex $ to_str a `mappend` to_str b

instance Num MathTex where
    fromInteger = MathTex . show
    (+) = binop "+"
    (-) = binop "-"
    (*) = binop " "  -- mult is space
    abs = MathTex . surround "\\lvert" "\\rvert" . to_str
    negate n = MathTex $ "-" ++ curlies (to_str n)
    signum _ = error "why are you using signum on latex, good sir?"

instance Fractional MathTex where
    fromRational k = numer / denom
        where
        numer = fromInteger . numerator $ k
        denom = fromInteger . denominator $ k

    (MathTex a) / (MathTex b) = MathTex $ "\\frac" ++ curlies a ++ curlies b

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

vector v = var $ "\\mathbb" ++ curlies v
