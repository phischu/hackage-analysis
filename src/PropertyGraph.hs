{-# LANGUAGE OverloadedStrings #-}
module PropertyGraph where

import Pipes (ListT(Select,enumerate),each,runEffect,(>->))
import Pipes.Prelude (toListM,drain)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT,execStateT,get,gets,put)
import Control.Monad (mzero,guard)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (lookup,empty,singleton,insertWith,elems)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap (lookup,insert,singleton,adjust,toList,empty,insertWith)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet (empty,intersection,union,singleton,toList)
import Data.Text (Text)
import Data.Strict.Tuple (Pair((:!:)))
import qualified Data.Strict.Tuple as Strict (fst)

type PG m = ListT (StateT (Pair PropertyGraph Int) m)
type Node = Int
type Label = Text
type NodeSet = IntSet
data PropertyGraph = PropertyGraph !LabelMap !PrevMap !NextMap !LabelIndex
type LabelMap = IntMap Label
type NextMap = IntMap NodeSet
type PrevMap = IntMap NodeSet
type LabelIndex = HashMap Label NodeSet

rootmap :: PropertyGraph
rootmap = PropertyGraph (IntMap.singleton rootnode "ROOT") IntMap.empty IntMap.empty (HashMap.singleton "ROOT" (IntSet.singleton rootnode))

runPropertyGraph :: (Monad m) => PG m a -> m PropertyGraph
runPropertyGraph pg = execStateT (runEffect (enumerate pg >-> drain)) (rootmap :!: 1) >>= return . Strict.fst

rootnode :: Node
rootnode = 0

start :: (Monad m) => Node -> PG m Node
start = return

labeled :: (Monad m) => Label -> PG m NodeSet
labeled label = do
    PropertyGraph _ _ _ labelindex <- lift (gets Strict.fst)
    case HashMap.lookup label labelindex of
        Nothing -> return IntSet.empty
        Just nodeset -> return nodeset

next :: (Monad m) => Node -> PG m NodeSet
next node = do
    PropertyGraph _ _ nextmap _ <- lift (gets Strict.fst)
    case IntMap.lookup node nextmap of
        Nothing -> return IntSet.empty
        Just nodeset -> return nodeset

prev :: (Monad m) => Node -> PG m NodeSet
prev node = do
    PropertyGraph _ prevmap _ _ <- lift (gets Strict.fst)
    case IntMap.lookup node prevmap of
        Nothing -> return IntSet.empty
        Just nodeset -> return nodeset

lab :: (Monad m) => Node -> PG m Label
lab node = do
    PropertyGraph labelmap _ _ _ <- lift (gets Strict.fst)
    case IntMap.lookup node labelmap of
        Nothing -> mzero
        Just label -> return label

newNode :: (Monad m) => Label -> PG m Node
newNode newlabel = do
    (PropertyGraph labelmap prevmap nextmap labelindex :!: newnode) <- lift get
    let labelmap' = IntMap.insert newnode newlabel labelmap
        labelindex' = HashMap.insertWith IntSet.union newlabel (IntSet.singleton newnode) labelindex
    lift (put (PropertyGraph labelmap' prevmap nextmap labelindex' :!: newnode + 1))
    return newnode

newNext :: (Monad m) => Label -> Node -> PG m Node
newNext newlabel node = do
    newnode <- newNode newlabel
    newEdge node newnode
    return newnode

newPrev :: (Monad m) => Label -> Node -> PG m Node
newPrev newlabel node = do
    newnode <- newNode newlabel
    newEdge newnode node
    return newnode

newEdge :: (Monad m) => Node -> Node -> PG m ()
newEdge node1 node2 = do
    (PropertyGraph labelmap prevmap nextmap labelindex :!: n) <- lift get
    let prevmap' = IntMap.insertWith IntSet.union node2 (IntSet.singleton node1) prevmap
        nextmap' = IntMap.insertWith IntSet.union node1 (IntSet.singleton node2) nextmap
    lift (put (PropertyGraph labelmap prevmap' nextmap' labelindex :!: n))
    return ()

newEdgeTo :: (Monad m) => Node -> Node -> PG m ()
newEdgeTo node2 node1 = newEdge node1 node2

nextLabeled :: (Monad m) => Label -> Node -> PG m Node
nextLabeled label node = do
    nexts <- next node
    labeleds <- labeled label
    scatter (IntSet.toList (IntSet.intersection nexts labeleds))

newNextLabeled :: (Monad m) => Label -> Node -> PG m Node
newNextLabeled = newNext

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
graphviz (PropertyGraph labelmap _ nextmap _) = unlines (["digraph {"] ++ nodestatements ++ edgestatements ++ ["}"]) where
    nodestatements = do
        (node,label) <- IntMap.toList labelmap
        return (show node ++ " [ label = " ++ show label ++ "];")
    edgestatements = do
        (node,targetnodes) <- IntMap.toList nextmap
        targetnode <- IntSet.toList targetnodes
        return (show node ++ " -> " ++ show targetnode ++ ";")



