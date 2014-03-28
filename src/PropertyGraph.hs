{-# LANGUAGE OverloadedStrings #-}
module PropertyGraph where

import Pipes (ListT(Select,enumerate),each,runEffect,(>->))
import Pipes.Prelude (toListM,drain)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT,execStateT,get,gets,put)
import Control.Monad (mzero,guard)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as LabelMap (lookup,empty,singleton,insertWith)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as NodeMap (lookup,insert,singleton,adjust)
import Data.Text (Text)

type PG m = ListT (StateT (PropertyGraph,Int) m)
type Node = Int
type Label = Text
type PropertyGraph = IntMap (HashMap Label [Node],Label,HashMap Label [Node])


rootmap :: PropertyGraph
rootmap = NodeMap.singleton 0 (LabelMap.empty,"ROOT",LabelMap.empty)

runPropertyGraph :: (Monad m) => PG m a -> m PropertyGraph
runPropertyGraph pg = execStateT (runEffect (enumerate pg >-> drain)) (rootmap,1) >>= return . fst

succ :: (Monad m) => Label -> Node -> PG m Node
succ label node = do
    graph <- lift (gets fst)
    case NodeMap.lookup node graph of
        Nothing -> mzero
        Just (_,_,succmap) -> case LabelMap.lookup label succmap of
            Nothing -> mzero
            Just succs -> scatter succs

pred :: (Monad m) => Label -> Node -> PG m Node
pred label node = do
    graph <- lift (gets fst)
    case NodeMap.lookup node graph of
        Nothing -> mzero
        Just (predmap,_,_) -> case LabelMap.lookup label predmap of
            Nothing -> mzero
            Just preds -> scatter preds

lab :: (Monad m) => Node -> PG m Label
lab node = do
    graph <- lift (gets fst)
    case NodeMap.lookup node graph of
        Nothing -> mzero
        Just (_,label,_) -> return label

newSucc :: (Monad m) => Label -> Node -> PG m Node
newSucc newlabel node = do
    (graph,newnode) <- lift get
    label <- lab node
    let graph' = NodeMap.adjust (\(predmap,_,succmap) -> (predmap,label,LabelMap.insertWith (++) newlabel [newnode] succmap)) node graph 
        graph'' = NodeMap.insert newnode (LabelMap.singleton label [node],newlabel,LabelMap.empty) graph'
    lift (put (graph'',newnode+1))
    return newnode

newPred :: (Monad m) => Label -> Node -> PG m Node
newPred newlabel node = do
    label <- lab node
    (graph,newnode) <- lift get
    let graph' = NodeMap.adjust (\(predmap,_,succmap) -> (LabelMap.insertWith (++) newlabel [newnode] predmap,label,succmap)) node graph 
        graph'' = NodeMap.insert newnode (LabelMap.empty,newlabel,LabelMap.singleton label [node]) graph'
    lift (put (graph'',newnode+1))
    return newnode

newLink :: (Monad m) => Node -> Node -> PG m ()
newLink node1 node2 = do
    label1 <- lab node1
    label2 <- lab node2
    (graph,n) <- lift get
    let graph' = NodeMap.adjust (\(predmap,_,succmap) -> (predmap,label1,LabelMap.insertWith (++) label2 [node2] succmap)) node1 graph
        graph'' = NodeMap.adjust (\(predmap,_,succmap) -> (LabelMap.insertWith (++) label1 [node1] predmap,label2,succmap)) node2 graph'
    lift (put (graph'',n))
    return ()

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





