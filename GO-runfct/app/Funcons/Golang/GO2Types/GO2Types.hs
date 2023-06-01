-- GeNeRaTeD fOr: ./GO-2-Types/GO-2-Types.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Golang.GO2Types.GO2Types where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("int8",NullaryFuncon stepInt8),("int16",NullaryFuncon stepInt16),("int32",NullaryFuncon stepInt32),("int",NullaryFuncon stepInt32),("int64",NullaryFuncon stepInt64),("uint8",NullaryFuncon stepUint8),("uint16",NullaryFuncon stepUint16),("uint32",NullaryFuncon stepUint32),("uint",NullaryFuncon stepUint32),("uint64",NullaryFuncon stepUint64)]

int8_ = FName "int8"
stepInt8 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "bounded-ints" [TApp "int-neg" [TApp "int-pow" [TFuncon (FValue (Nat 2)),TFuncon (FValue (Nat 7))]],TApp "int-add" [TApp "int-pow" [TFuncon (FValue (Nat 2)),TFuncon (FValue (Nat 7))],TFuncon (FValue (Nat (-1)))]]) env

int16_ = FName "int16"
stepInt16 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "bounded-ints" [TApp "int-neg" [TApp "int-pow" [TFuncon (FValue (Nat 2)),TFuncon (FValue (Nat 15))]],TApp "int-add" [TApp "int-pow" [TFuncon (FValue (Nat 2)),TFuncon (FValue (Nat 15))],TFuncon (FValue (Nat (-1)))]]) env

int32_ = FName "int32"
int_ = FName "int32"
stepInt32 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "bounded-ints" [TApp "int-neg" [TApp "int-pow" [TFuncon (FValue (Nat 2)),TFuncon (FValue (Nat 31))]],TApp "int-add" [TApp "int-pow" [TFuncon (FValue (Nat 2)),TFuncon (FValue (Nat 31))],TFuncon (FValue (Nat (-1)))]]) env

int64_ = FName "int64"
stepInt64 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "bounded-ints" [TApp "int-neg" [TApp "int-pow" [TFuncon (FValue (Nat 2)),TFuncon (FValue (Nat 63))]],TApp "int-add" [TApp "int-pow" [TFuncon (FValue (Nat 2)),TFuncon (FValue (Nat 63))],TFuncon (FValue (Nat (-1)))]]) env

uint8_ = FName "uint8"
stepUint8 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "bounded-ints" [TFuncon (FValue (Nat 0)),TApp "int-add" [TApp "int-pow" [TFuncon (FValue (Nat 2)),TFuncon (FValue (Nat 8))],TFuncon (FValue (Nat (-1)))]]) env

uint16_ = FName "uint16"
stepUint16 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "bounded-ints" [TFuncon (FValue (Nat 0)),TApp "int-add" [TApp "int-pow" [TFuncon (FValue (Nat 2)),TFuncon (FValue (Nat 16))],TFuncon (FValue (Nat (-1)))]]) env

uint32_ = FName "uint32"
uint_ = FName "uint32"
stepUint32 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "bounded-ints" [TFuncon (FValue (Nat 0)),TApp "int-add" [TApp "int-pow" [TFuncon (FValue (Nat 2)),TFuncon (FValue (Nat 32))],TFuncon (FValue (Nat (-1)))]]) env

uint64_ = FName "uint64"
stepUint64 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "bounded-ints" [TFuncon (FValue (Nat 0)),TApp "int-add" [TApp "int-pow" [TFuncon (FValue (Nat 2)),TFuncon (FValue (Nat 64))],TFuncon (FValue (Nat (-1)))]]) env