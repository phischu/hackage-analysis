module PropertyGraph where

import qualified Data.Graph.Inductive as Gr (Node,LEdge,newNodes,insNode,insEdge,out,inn,lab,empty)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Pipes (ListT(Select,enumerate),each,runEffect,(>->))
import Pipes.Prelude (toListM,drain)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT,execStateT,get,put)
import Control.Monad (mzero)
import Data.Map (Map)
import Data.Text (Text)

type PG m = ListT (StateT PropertyGraph m)
type Node = Gr.Node
type Edge = Gr.LEdge Text
type PropertyGraph = Gr Properties Label
type Properties = Map Text Text
type Label = Text

runPropertyGraph :: (Monad m) => PG m a -> m PropertyGraph
runPropertyGraph pg = execStateT (runEffect (enumerate pg >-> drain)) Gr.empty

newNode :: (Monad m) => Properties -> PG m Node
newNode p = do
    graph <- lift get
    let [n] = Gr.newNodes 1 graph
    lift (put (Gr.insNode (n,p) graph))
    return n

newEdge :: (Monad m) => Label -> Node -> Node -> PG m Edge
newEdge l s t = do
    graph <- lift get
    lift (put (Gr.insEdge (s,t,l) graph))
    return (s,t,l)

out :: (Monad m) => Node -> PG m Edge
out n = do
    graph <- lift get
    scatter (Gr.out graph n)

inn :: (Monad m) => Node -> PG m Edge
inn n = do
    graph <- lift get
    scatter (Gr.inn graph n)

source :: (Monad m) => Edge -> PG m Node
source (s,_,_) = return s

target :: (Monad m) => Edge -> PG m Node
target (_,t,_) = return t

properties :: (Monad m) => Node -> PG m Properties
properties n = do
    graph <- lift get
    case Gr.lab graph n of
        Nothing -> mzero
        Just p -> return p

label :: (Monad m) => Edge -> PG m Label
label (_,_,l) = return l

gather :: (Monad m) => PG m a -> PG m [a]
gather pg = do
    as <- lift (toListM (enumerate pg))
    return as

scatter :: (Monad m) => [a] -> PG m a
scatter as = Select (each as)

strain :: (Monad m) => (a -> Bool) -> a -> PG m a
strain p a
    | p a = return a
    | otherwise = mzero

unique :: (Monad m) => PG m a -> PG m (Maybe a)
unique pg = do
    as <- gather pg
    case as of
        [] -> return Nothing
        [a] -> return (Just a)
        _ -> return Nothing









