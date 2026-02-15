{-# LANGUAGE TemplateHaskell #-}
module Jiexi.Gateway.TH where

import Control.Monad
import Language.Haskell.TH

import Jiexi.Gateway.Types

deriveTupleToTypst :: Int -> Q [Dec]
deriveTupleToTypst n = do
  tyNames <- forM [1 .. n] $ \i -> newName ("a" ++ show i)
  compNames <- forM [1 .. n] $ \i -> newName ("x" ++ show i)
  let tyVars = map varT tyNames
  let tupleTy = foldl appT (tupleT n) tyVars
  let ctx = map (\var -> [t| ToTypst $var |]) tyVars
  let
    implDecl = funD 'toTypst [clause
      [tupP (varP <$> compNames)]
      (normalB [| TypList $(listE (map (\x -> [| toTypst $(varE x) |]) compNames)) |])
      []]
  let instDecl = instanceD (sequenceA ctx) (conT ''ToTypst `appT` tupleTy) [implDecl]
  (:[]) <$> instDecl

deriveTupleFromTypst :: Int -> Q [Dec]
deriveTupleFromTypst n = do
  tyNames <- forM [1 .. n] $ \i -> newName ("a" ++ show i)
  compNames <- forM [1 .. n] $ \i -> newName ("x" ++ show i)
  let tyVars = map varT tyNames
  let tupleTy = foldl appT (tupleT n) tyVars
  let ctx = map (\var -> [t| FromTypst $var |]) tyVars
  let
    implDecl = funD 'fromTypst [clause
      [conP 'TypList [listP $ varP <$> compNames]]
      (normalB (foldl 
        (\acc y -> [| $acc <*> $y |])
        [|pure $(conE (tupleDataName n))|]
        (map (\x -> [| fromTypst $(varE x) |]) compNames)
      ))
      [],
      clause [wildP] (normalB [| Nothing |]) []
      ]
  let instDecl = instanceD (sequenceA ctx) (conT ''FromTypst `appT` tupleTy) [implDecl]
  (:[]) <$> instDecl

deriveTupleFromToTypst :: Int -> Q [Dec]
deriveTupleFromToTypst n = liftA2 (++) (deriveTupleFromTypst n) (deriveTupleToTypst n)

deriveTuplesFromToTypst :: [Int] -> Q [Dec]
deriveTuplesFromToTypst ns = concat <$> mapM deriveTupleFromToTypst ns