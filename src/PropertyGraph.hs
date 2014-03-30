{-# LANGUAGE OverloadedStrings #-}
module PropertyGraph where

import Pipes (ListT(Select,enumerate),each,runEffect,(>->))
import Pipes.Prelude (toListM,drain)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT,execStateT,get,gets,put)
import Control.Monad (mzero,guard)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as LabelMap (lookup,empty,singleton,insertWith,elems)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as NodeMap (lookup,insert,singleton,adjust,toList)
import Data.Text (Text)

type PG m = ListT (StateT (PropertyGraph,Int) m)
type Node = Int
type Label = Text
type PropertyGraph = IntMap (HashMap Label [Node],Label,HashMap Label [Node])


rootmap :: PropertyGraph
rootmap = NodeMap.singleton rootnode (LabelMap.empty,"ROOT",LabelMap.empty)

runPropertyGraph :: (Monad m) => PG m a -> m PropertyGraph
runPropertyGraph pg = execStateT (runEffect (enumerate pg >-> drain)) (rootmap,1) >>= return . fst

rootnode :: Node
rootnode = 0

start :: (Monad m) => Node -> PG m Node
start = return

next :: (Monad m) => Label -> Node -> PG m Node
next label node = do
    graph <- lift (gets fst)
    case NodeMap.lookup node graph of
        Nothing -> mzero
        Just (_,_,nextmap) -> case LabelMap.lookup label nextmap of
            Nothing -> mzero
            Just nexts -> scatter nexts

prev :: (Monad m) => Label -> Node -> PG m Node
prev label node = do
    graph <- lift (gets fst)
    case NodeMap.lookup node graph of
        Nothing -> mzero
        Just (prevmap,_,_) -> case LabelMap.lookup label prevmap of
            Nothing -> mzero
            Just prevs -> scatter prevs

lab :: (Monad m) => Node -> PG m Label
lab node = do
    graph <- lift (gets fst)
    case NodeMap.lookup node graph of
        Nothing -> mzero
        Just (_,label,_) -> return label

newNext :: (Monad m) => Label -> Node -> PG m Node
newNext newlabel node = do
    (graph,newnode) <- lift get
    label <- lab node
    let graph' = NodeMap.adjust (\(prevmap,_,nextmap) -> (prevmap,label,LabelMap.insertWith (++) newlabel [newnode] nextmap)) node graph 
        graph'' = NodeMap.insert newnode (LabelMap.singleton label [node],newlabel,LabelMap.empty) graph'
    lift (put (graph'',newnode+1))
    return newnode

newPrev :: (Monad m) => Label -> Node -> PG m Node
newPrev newlabel node = do
    label <- lab node
    (graph,newnode) <- lift get
    let graph' = NodeMap.adjust (\(prevmap,_,nextmap) -> (LabelMap.insertWith (++) newlabel [newnode] prevmap,label,nextmap)) node graph 
        graph'' = NodeMap.insert newnode (LabelMap.empty,newlabel,LabelMap.singleton label [node]) graph'
    lift (put (graph'',newnode+1))
    return newnode

newLink :: (Monad m) => Node -> Node -> PG m ()
newLink node1 node2 = do
    label1 <- lab node1
    label2 <- lab node2
    (graph,n) <- lift get
    let graph' = NodeMap.adjust (\(prevmap,_,nextmap) -> (prevmap,label1,LabelMap.insertWith (++) label2 [node2] nextmap)) node1 graph
        graph'' = NodeMap.adjust (\(prevmap,_,nextmap) -> (LabelMap.insertWith (++) label1 [node1] prevmap,label2,nextmap)) node2 graph'
    lift (put (graph'',n))
    return ()

newLinkTo :: (Monad m) => Node -> Node -> PG m ()
newLinkTo node2 node1 = newLink node1 node2

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

ensure :: (Monad m) => PG m a -> PG m ()
ensure p = do
    as <- gather p
    guard (not (null as))

has :: (Monad m) => (a -> PG m b) -> a -> PG m a
has p a = do
    ensure (p a)
    return a

graphviz :: PropertyGraph -> String
graphviz propertygraph = unlines (["digraph {"] ++ statements ++ ["}"]) where
    statements = do
        (node,(_,label,nextmap)) <- NodeMap.toList propertygraph
        let nodestatement = show node ++ " [ label = " ++ show label ++ "];"
            edgestatements = do
                targets <- LabelMap.elems nextmap
                target <- targets
                let edgestatement = show node ++ " -> " ++ show target ++ ";"
                return edgestatement
        (nodestatement:edgestatements)



