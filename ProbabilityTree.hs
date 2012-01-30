module ProbabilityTree where

import Ratio
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ

swap (a,b) = (b,a)

data ProbaTree cst = ProbaTree {
    curstate :: cst,
    outcome :: [(ProbaTree cst, Rational)]
} deriving (Ord, Eq, Show)

getAllStates :: (cst -> [(cst, Rational)]) -> cst -> ProbaTree cst
getAllStates getnextstate curstate = ProbaTree curstate nextTrees
    where
        outcomes = getnextstate curstate
        nextTrees = map (\(st, p) -> (getAllStates getnextstate st, p)) outcomes

finalStateProbability :: Ord cst => ProbaTree cst -> [(cst, Rational)]
finalStateProbability tree  | length (outcome tree) == 0 = [(curstate tree, 1%1)]
                            | otherwise = Map.toList $ Map.fromListWith (+) $ concat resolvedoutcomes
    where
        resolvedoutcomes = map (\(ct, pb) -> 
            map (\(x,y) -> (x, y*pb)) (finalStateProbability ct)
            ) (outcome tree)

showProbaTree :: (cst -> Doc) -> ProbaTree cst -> Doc
showProbaTree showstate tree = (showstate $ curstate tree) $$ (vcat $ map (\x -> (text $ show $ snd x) <+> (showProbaTree (showstate) $ fst x)) (outcome tree))

nbPTNodes :: ProbaTree cst -> Integer
nbPTNodes tree = 1 + (sum $ map (nbPTNodes.fst) (outcome tree))

