import ProbabilityTree
import Ratio

-- total, nbtoroll, max
data DiceState = DiceState Int Int deriving(Show, Ord, Eq)

basestate = DiceState 0 3

nextstate :: DiceState -> [(DiceState,Rational)]
nextstate (DiceState _ 0) = []
nextstate (DiceState curscore rolled) = [ (DiceState (curscore+n) (rolled-1), 1%6) | n <- [1..6] ]

allrolls = getAllStates (nextstate) basestate
finalproba = finalStateProbability allrolls

