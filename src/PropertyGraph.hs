module PropertyGraph where

import Data.Graph.Inductive (Node,Edge)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Pipes (ListT)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Map (Map)
import Data.Text (Text)

type PG m = ListT (StateT PropertyGraph m)
type PropertyGraph = Gr Properties Label
type Properties = Map Text Text
type Label = Text

runPropertyGraph :: PG m a -> m PropertyGraph
runPropertyGraph = undefined

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










