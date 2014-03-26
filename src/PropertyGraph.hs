module PropertyGraph where

type PG m = ListT (StateT Gr m)
type Properties = Map Text Text
type Label = Text


newNode :: Properties -> PG m Node
newNode = undefined

newEdge :: Label -> Node -> Node -> PG m Edge
newEdge = undefined

out :: Node -> PG m Edge
out = undefined

inn :: Node -> PG m Edge
inn = undefined

source :: Edge -> PG m Node
source = undefined

target :: Edge -> PG m Node
target = undefined

properties :: Node -> PG m Properties
properties = undefined

label :: Node -> PG m Label
label = undefined

gather :: PG m a -> PG m [a]
gather = undefined

scatter :: [a] -> PG m a
scatter = undefined

strain :: (a -> Bool) -> a -> PG m a
strain = undefined










