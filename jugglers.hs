import Data.List (minimumBy)
import Data.Array (Array, (//), assocs, array, (!))
import Data.Attoparsec.Char8 (char, space, (<?>), sepBy, sepBy1, parseOnly,
                              decimal)
import qualified Data.ByteString as B
import qualified Data.Sequence as Seq

type HEP = (Int, Int, Int)
type JugglerID = Int
type TeamID = Int
data Juggler = Juggler { _jid :: JugglerID, jhep :: HEP, prefs :: [TeamID] }
data Team = Team { roster :: [Juggler], maxjugs :: Int, thep :: HEP}

instance Show Juggler where show (Juggler id _ _) = 'J' : show id
instance Show Team where show (Team rs _ _) = show rs
instance Eq Juggler where (Juggler id _ _) == (Juggler id' _ _) = id == id'

cons_snd x (y, xs) = (y, x : xs)

drop_choice jugg@Juggler{prefs=prefs} = jugg { prefs = tail prefs }

compat :: Team -> Juggler -> Int
compat Team{thep=(h, e, p)} Juggler{jhep=(h', e', p')} = h*h' + e*e' + p*p'

worst_on :: Team -> Juggler
worst_on team@Team{roster=rs} = minimumBy cmpt rs
    where cmpt x y = compare (compat team x) (compat team y)

drop_worst_in :: Team -> Team
drop_worst_in team@Team{roster=rs} = team { roster = filter (/= w) rs }
    where w = worst_on team

round_robin :: Array TeamID Team -> Seq.Seq Juggler
            -> (Array TeamID Team, [Juggler])
round_robin ts js
    | Seq.length js == 0 = (ts, [])
    | otherwise = case j of
        Juggler _ _ [] -> j `cons_snd` round_robin ts jdropped
        Juggler _ _ (tid:tids) -> case overfull jhired of
            False -> round_robin (ts // [(tid, jhired)]) jdropped
            True -> (ts // [(tid, jhired')]) `reject` (worst_on jhired)
            where
            prefer = ts ! tid
            jhired = prefer { roster = (drop_choice j) : roster prefer }
            jhired' = drop_worst_in jhired

            overfull Team{roster=rs, maxjugs=mx} = length rs > mx
            reject ts w = case w of
                Juggler _ _ [] -> w `cons_snd` round_robin ts jdropped
                otherwise -> round_robin ts $ jdropped Seq.|> w
    where
    j = Seq.index js 0
    jdropped = Seq.drop 1 js

donate_rejects :: Array TeamID Team -> [Juggler] -> Array TeamID Team
donate_rejects ts [] = ts
donate_rejects ts (j:js) = donate_rejects ts' js
    where
    ts' = ts // [(best_fit_id, jhired)]
    avail = filter (is_full . snd) $ assocs ts
    (best_fit_id, best_fit) = minimumBy better_fit avail
    jhired = best_fit { roster = j : roster best_fit }
    better_fit (_, t) (_, t') = compare (compat t j) (compat t' j)
    is_full Team{roster=rs, maxjugs=mx} = length rs == mx

newline = char '\n'

read_id prefix = flip (<?>) "read_id" $ char prefix >> decimal

read_hep = flip (<?>) "read_hep" $ do
    h <- char 'H' >> char ':' >> decimal
    e <- space >> char 'E' >> char ':' >> decimal
    p <- space >> char 'P' >> char ':' >> decimal
    return (h, e, p)

-- C C1999 H:9 E:5 P:9
circuit_line = flip (<?>) "circuit_line" $ do
    tid <- char 'C' >> space >> read_id 'C'
    hep <- space >> read_hep
    return (tid, hep)

-- J J0 H:7 E:6 P:0 C453,C1706,C318,C271,C1958,C1051,C241,C1736,C304,C518
juggler_line = flip (<?>) "juggler_line" $ do
    jid <- char 'J' >> space >> read_id 'J'
    space
    hep <- read_hep
    space
    prefs <- read_id 'C' `sepBy` char ','
    return $ Juggler jid hep prefs

specs = do
    (ids, theps) <- unzip `fmap` (circuit_line `sepBy1` newline)
    newline >> newline
    js <- juggler_line `sepBy1` newline
    let n = length js `quot` length ids
    let ts = array (minimum ids, maximum ids) . zip ids $ map (Team [] n) theps
    return (ts, js)

main = do
    rv <- parseOnly specs `fmap` B.getContents
    case rv of
        Left e -> print e
        Right (ts, js) -> print $ foldl sumjugs 0 $ roster (soln ! 1970)
            where
            sumjugs acc Juggler{_jid=jid} = acc + jid
            soln = uncurry donate_rejects . round_robin ts $ Seq.fromList js
