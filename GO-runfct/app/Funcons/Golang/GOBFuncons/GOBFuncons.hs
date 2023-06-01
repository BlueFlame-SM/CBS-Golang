-- GeNeRaTeD fOr: ./GO-B-Funcons/GO-B-Funcons.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Golang.GOBFuncons.GOBFuncons where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("channels",DataTypeMemberss "channels" [TPVar "T"] [DataTypeMemberConstructor "channel" [TName "nats",TName "bools",TName "syncs",TName "syncs",TName "syncs",TName "vars"] (Just [TPVar "T"])])]

funcons = libFromList
    [("list-snoc",StrictFuncon stepList_snoc),("list-init",StrictFuncon stepList_init),("list-last",StrictFuncon stepList_last),("lists-type",StrictFuncon stepLists_type),("variable-type",StrictFuncon stepVariable_type),("channel",StrictFuncon stepChannel),("channel-create",PartiallyStrictFuncon [NonStrict,Strict] NonStrict stepChannel_create),("channel-type",StrictFuncon stepChannel_type),("channels",StrictFuncon stepChannels)]

list_snoc_ fargs = FApp "list-snoc" (fargs)
stepList_snoc fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)],VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "list" [TVar "V*",TVar "V"]) env

list_init_ fargs = FApp "list-init" (fargs)
stepList_init fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp),VPAnnotated VPWildCard (TName "values")]] env
            rewriteTermTo (TApp "list" [TVar "V*"]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])])]] env
            rewriteTermTo (TSeq []) env

list_last_ fargs = FApp "list-last" (fargs)
stepList_last fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),VPAnnotated (VPSeqVar "___" StarOp) (TSortSeq (TName "values") StarOp),VPAnnotated (VPMetaVar "V") (TName "values")]] env
            rewriteTermTo (TVar "V") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])])]] env
            rewriteTermTo (TSeq []) env

lists_type_ fargs = FApp "lists-type" (fargs)
stepLists_type fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "lists" [VPMetaVar "T"]] env
            rewriteTermTo (TVar "T") env

variable_type_ fargs = FApp "variable-type" (fargs)
stepVariable_type fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "variable" [VPAnnotated VPWildCard (TName "locations"),VPAnnotated (VPMetaVar "T") (TName "types")]] env
            rewriteTermTo (TVar "T") env

channel_ fargs = FApp "channel" (fargs)
stepChannel fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1",VPMetaVar "_X2",VPMetaVar "_X3",VPMetaVar "_X4",VPMetaVar "_X5",VPMetaVar "_X6"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X2") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X3") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X4") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X5") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X6") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 104)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 108)])])),TVar "_X1",TVar "_X2",TVar "_X3",TVar "_X4",TVar "_X5",TVar "_X6"]) env

channel_create_ fargs = FApp "channel-create" (fargs)
stepChannel_create fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "T",PAnnotated (PMetaVar "N") (TName "nats")] env
            rewriteTermTo (TApp "channel" [TVar "N",TName "false",TName "exclusive-lock-create",TName "condition-create",TName "condition-create",TApp "alloc-init" [TApp "lists" [TVar "T"],TApp "list" []]]) env

channel_type_ fargs = FApp "channel-type" (fargs)
stepChannel_type fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPAnnotated (VPSeqVar "___" StarOp) (TSortSeq (TName "values") StarOp),VPAnnotated (VPMetaVar "Var") (TName "variables")]] env
            rewriteTermTo (TApp "lists-type" [TApp "variable-type" [TVar "Var"]]) env

channels_ = FApp "channels"
stepChannels ts = rewriteType "channels" ts