-- GeNeRaTeD fOr: ./GO-2-Types/GO-2-Types.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Golang.GO2Types.GO2Types where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("int",NullaryFuncon stepInt),("int8",NullaryFuncon stepInt8),("int16",NullaryFuncon stepInt16),("int32",NullaryFuncon stepInt32),("uint",NullaryFuncon stepUint),("uint8",NullaryFuncon stepUint8),("uint16",NullaryFuncon stepUint16),("uint32",NullaryFuncon stepUint32)]

int_ = FName "int"
stepInt = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TName "int32") env

int8_ = FName "int8"
stepInt8 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "bounded-ints" [TSortPower (TFuncon (FValue (Nat (-2)))) (TFuncon (FValue (Nat 7))),TApp "int-sub" [TSortPower (TFuncon (FValue (Nat 2))) (TFuncon (FValue (Nat 7))),TFuncon (FValue (Nat 1))]]) env

int16_ = FName "int16"
stepInt16 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "bounded-ints" [TSortPower (TFuncon (FValue (Nat (-2)))) (TFuncon (FValue (Nat 15))),TApp "int-sub" [TSortPower (TFuncon (FValue (Nat 2))) (TFuncon (FValue (Nat 15))),TFuncon (FValue (Nat 1))]]) env

int32_ = FName "int32"
stepInt32 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "bounded-ints" [TSortPower (TFuncon (FValue (Nat (-2)))) (TFuncon (FValue (Nat 31))),TApp "int-sub" [TSortPower (TFuncon (FValue (Nat 2))) (TFuncon (FValue (Nat 31))),TFuncon (FValue (Nat 1))]]) env

uint_ = FName "uint"
stepUint = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TName "uint32") env

uint8_ = FName "uint8"
stepUint8 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "bounded-ints" [TFuncon (FValue (Nat 0)),TApp "int-sub" [TSortPower (TFuncon (FValue (Nat 2))) (TFuncon (FValue (Nat 8))),TFuncon (FValue (Nat 1))]]) env

uint16_ = FName "uint16"
stepUint16 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "bounded-ints" [TFuncon (FValue (Nat 0)),TApp "int-sub" [TSortPower (TFuncon (FValue (Nat 2))) (TFuncon (FValue (Nat 16))),TFuncon (FValue (Nat 1))]]) env

uint32_ = FName "uint32"
stepUint32 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "bounded-ints" [TFuncon (FValue (Nat 0)),TApp "int-sub" [TSortPower (TFuncon (FValue (Nat 2))) (TFuncon (FValue (Nat 32))),TFuncon (FValue (Nat 1))]]) env