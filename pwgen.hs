import List
import Ratio
import Data.Bits
import qualified Data.Map as Map
import Debug.Trace
import ProbabilityTree
import Text.PrettyPrint.HughesPJ

vowel = 1
consonant = 2
diphthong = 4
notfirst = 8

maxlen = 8

elements = [( "a",	vowel ),
	( "ae", vowel .|. diphthong ),
	( "ah",	vowel .|. diphthong ),
	( "ai", vowel .|. diphthong ),
	( "b",  consonant ),
	( "c",	consonant ),
	( "ch", consonant .|. diphthong ),
	( "d",	consonant ),
	( "e",	vowel ),
	( "ee", vowel .|. diphthong ),
	( "ei",	vowel .|. diphthong ),
	( "f",	consonant ),
	( "g",	consonant ),
	( "gh", consonant .|. diphthong .|. notfirst ),
	( "h",	consonant ),
	( "i",	vowel ),
	( "ie", vowel .|. diphthong ),
	( "j",	consonant ),
	( "k",	consonant ),
	( "l",	consonant ),
	( "m",	consonant ),
	( "n",	consonant ),
	( "ng",	consonant .|. diphthong .|. notfirst ),
	( "o",	vowel ),
	( "oh",	vowel .|. diphthong ),
	( "oo",	vowel .|. diphthong),
	( "p",	consonant ),
	( "ph",	consonant .|. diphthong ),
	( "qu",	consonant .|. diphthong),
	( "r",	consonant ),
	( "s",	consonant ),
	( "sh",	consonant .|. diphthong),
	( "t",	consonant ),
	( "th",	consonant .|. diphthong),
	( "u",	vowel ),
	( "v",	consonant ),
	( "w",	consonant ),
	( "x",	consonant ),
	( "y",	consonant ),
	( "z",	consonant )]

-- type -> compte
-- Map.Map type compte
compact :: Map.Map Int  Int
compact = Map.fromListWith (+) $ map tocassoc elements

elist :: Map.Map Int [String]
elist = Map.fromListWith (++) $ map (\(a,b) -> (b,[a])) elements

toelist :: Int -> [String]
toelist n = elist Map.! n

tocassoc :: (String, Int) -> (Int, Int)
tocassoc (_, t) = (t, 1)

-- next (current string length) (shouldbe flags) (prev flags) (max length) -> Map.Map type count
nextstate :: Int -> Int -> Int -> Int -> Map.Map Int Int
nextstate curstrlen shouldbe prev maxlen = Map.mapWithKey (\k v -> v) acceptable 
    where
        acceptable = Map.filterWithKey ca compact
        ca k _  | ((k .&. notfirst) /= 0) && (curstrlen == 0) = False
                | ( (prev .&. vowel .&. k) /= 0 ) && ( (k .&. diphthong) /= 0) = False
                | (curstrlen + 1 + ( (k .&. diphthong) `div` diphthong )) <= maxlen = (k .&. shouldbe) /= 0
                | otherwise = False

-- resultat [(compte, next shouldbe)]
whatsnext :: Int -> Int -> Int -> [(Int, Int)]
whatsnext shouldbe prev flags   | shouldbe == consonant = [(1, vowel)]
                                | (prev .&. vowel /= 0) || (flags .&. diphthong /= 0) = [(1, consonant)]
                                | otherwise = [(6, consonant), (4, vowel)]

data PwState = PwState {
    stypes :: [Int],
    sshouldbe :: Int,
    smaxlen :: Int
} deriving (Show,Eq,Ord)


stypelen :: [Int] -> Int
stypelen n = sum $ map (\flags -> 1 + (flags .&. diphthong) `div` diphthong ) n

pwNextState :: PwState -> [(PwState,Rational)]
pwNextState pws = newlist
    where
        lastelem    | (length $ stypes pws) > 0 = last $ stypes pws
                    | otherwise = 0
        ns = Map.toList $ nextstate (stypelen $ stypes pws) (sshouldbe pws) lastelem (smaxlen pws)
        shouldbes = concatMap (sb2ns (sshouldbe pws) lastelem) ns
        allpos = sum $ map (\(x,_,_) -> x) shouldbes
        newlist = map (\(ncnt, ntype, snextsb) -> (PwState ((stypes pws) ++ [ntype]) snextsb (smaxlen pws), ncnt / allpos)) shouldbes

sb2ns :: Int -> Int -> (Int,Int) -> [(Rational, Int, Int)]
sb2ns prevshouldbe lastelem (ntype, ncount) = map (\(a,b,c) -> (a%totalscore,b,c)) pscore
    where
        sbs = whatsnext prevshouldbe lastelem ntype
        pscore = map (\(scount, snextsb) -> (fromIntegral $ scount*ncount, ntype, snextsb)) sbs
        totalscore :: Integer
        totalscore = sum $ map (\(x,_,_) -> x) pscore

allstatesv = getAllStates (pwNextState) (PwState [] vowel maxlen)
allstatesc = getAllStates (pwNextState) (PwState [] consonant maxlen)

allstates = ProbaTree (PwState [] 0 0) [(allstatesv, 1%2),(allstatesc, 1%2)]

cardinaltype :: [Int] -> Integer
cardinaltype x = product $ map (fromIntegral . length .toelist) x

out :: [(([Int], Integer),Rational)]
out = sortBy (\((_,c1),p1) ((_,c2),p2) -> compare (p2 * (1%c2)) (p1 * (1%c1))) $ Map.toList $ Map.fromListWith (+) $ map (\(st, proba) -> ((stypes st, cardinaltype (stypes st)),proba)) $ finalStateProbability allstates

displayPwState :: PwState -> Doc
displayPwState (PwState tps sshouldbe _) = (parens ((text $ show tps) <+> int sshouldbe))

displayed = showProbaTree (displayPwState) allstates
