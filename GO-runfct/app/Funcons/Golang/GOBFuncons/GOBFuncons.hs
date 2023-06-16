-- GeNeRaTeD fOr: ./GO-B-Funcons/GO-B-Funcons.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Golang.GOBFuncons.GOBFuncons where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("type-constructs",DataTypeMemberss "type-constructs" [] [DataTypeInclusionn (TName "types"),DataTypeMemberConstructor "type-construct" [TName "types",TSortSeq (TName "type-constructs") StarOp] (Just [])]),("channels",DataTypeMemberss "channels" [] [DataTypeMemberConstructor "channel" [TName "vars",TName "vars",TName "vars",TName "syncs",TName "syncs",TName "syncs"] (Just [])])]

funcons = libFromList
    [("list-snoc",StrictFuncon stepList_snoc),("list-init",StrictFuncon stepList_init),("list-last",StrictFuncon stepList_last),("variable-type",StrictFuncon stepVariable_type),("type-construct",StrictFuncon stepType_construct),("type-construct-type",StrictFuncon stepType_construct_type),("type-construct-args",StrictFuncon stepType_construct_args),("channel",StrictFuncon stepChannel),("channel-create",StrictFuncon stepChannel_create),("channel-send-wait",StrictFuncon stepChannel_send_wait),("channel-recv-wait",StrictFuncon stepChannel_recv_wait),("channel-sync-else-wait",StrictFuncon stepChannel_sync_else_wait),("channel-release",StrictFuncon stepChannel_release),("channel-closed",StrictFuncon stepChannel_closed),("channel-send",StrictFuncon stepChannel_send),("channel-send-else-wait",StrictFuncon stepChannel_send_else_wait),("channel-recv",StrictFuncon stepChannel_recv),("channel-recv-else-wait",StrictFuncon stepChannel_recv_else_wait),("type-constructs",NullaryFuncon stepType_constructs),("channels",NullaryFuncon stepChannels)]

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

variable_type_ fargs = FApp "variable-type" (fargs)
stepVariable_type fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "variable" [VPAnnotated VPWildCard (TName "locations"),VPAnnotated (VPMetaVar "T") (TName "types")]] env
            rewriteTermTo (TVar "T") env

type_construct_ fargs = FApp "type-construct" (fargs)
stepType_construct fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1",VPSeqVar "_X2*" StarOp] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X2*") (TSortSeq (TSortSeq (TName "values") QuestionMarkOp) StarOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 121)]),FValue (ADTVal "unicode-character" [FValue (Int 112)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 45)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 117)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])])),TVar "_X1",TVar "_X2*"]) env

type_construct_type_ fargs = FApp "type-construct-type" (fargs)
stepType_construct_type fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "T") (TName "types")] env
            rewriteTermTo (TVar "T") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "type-construct" [VPAnnotated (VPMetaVar "T") (TName "types"),VPAnnotated (VPSeqVar "___" StarOp) (TSortSeq (TName "type-constructs") StarOp)]] env
            rewriteTermTo (TVar "T") env

type_construct_args_ fargs = FApp "type-construct-args" (fargs)
stepType_construct_args fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated VPWildCard (TName "types")] env
            rewriteTermTo (TSeq []) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "type-construct" [VPWildCard,VPSeqVar "Args*" StarOp]] env
            rewriteTermTo (TVar "Args*") env

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
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "T") (TName "types"),VPAnnotated (VPMetaVar "N") (TName "nats")] env
            rewriteTermTo (TApp "channel" [TApp "alloc-init" [TApp "lists" [TVar "T"],TApp "list" []],TApp "alloc-init" [TName "nats",TVar "N"],TApp "alloc-init" [TName "bools",TName "false"],TName "condition-create",TName "condition-create",TName "exclusive-lock-create"]) env

channel_send_wait_ fargs = FApp "channel-send-wait" (fargs)
stepChannel_send_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPWildCard,VPWildCard,VPWildCard,VPMetaVar "SendWait",VPWildCard,VPMetaVar "Mutex"]] env
            rewriteTermTo (TApp "condition-wait-with-lock" [TVar "SendWait",TVar "Mutex"]) env

channel_recv_wait_ fargs = FApp "channel-recv-wait" (fargs)
stepChannel_recv_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPWildCard,VPWildCard,VPWildCard,VPWildCard,VPMetaVar "RecvWait",VPMetaVar "Mutex"]] env
            rewriteTermTo (TApp "condition-wait-with-lock" [TVar "RecvWait",TVar "Mutex"]) env

channel_sync_else_wait_ fargs = FApp "channel-sync-else-wait" (fargs)
stepChannel_sync_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPWildCard,VPWildCard,VPWildCard,VPWildCard,VPWildCard,VPMetaVar "Mutex"]] env
            rewriteTermTo (TApp "exclusive-lock-sync-else-wait" [TVar "Mutex"]) env

channel_release_ fargs = FApp "channel-release" (fargs)
stepChannel_release fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPWildCard,VPWildCard,VPWildCard,VPWildCard,VPWildCard,VPMetaVar "Mutex"]] env
            rewriteTermTo (TApp "exclusive-lock-release" [TVar "Mutex"]) env

channel_closed_ fargs = FApp "channel-closed" (fargs)
stepChannel_closed fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPWildCard,VPWildCard,VPMetaVar "Closed",VPWildCard,VPWildCard,VPWildCard]] env
            rewriteTermTo (TApp "assigned" [TVar "Closed"]) env

channel_send_ fargs = FApp "channel-send" (fargs)
stepChannel_send fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPMetaVar "Queue",VPMetaVar "N",VPMetaVar "Closed",VPWildCard,VPMetaVar "RecvWait",VPWildCard],VPMetaVar "Val"] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "and" [TApp "not" [TApp "assigned" [TVar "Closed"]],TApp "is-greater" [TApp "assigned" [TVar "N"],TFuncon (FValue (Nat 0))],TApp "is-in-type" [TApp "list" [TVar "Val"],TApp "variable-type" [TVar "Queue"]]]],TApp "assign" [TVar "N",TApp "integer-subtract" [TApp "assigned" [TVar "N"],TFuncon (FValue (Nat 1))]],TApp "assign" [TVar "Queue",TApp "list-snoc" [TApp "assigned" [TVar "Queue"],TVar "Val"]],TApp "finalise-failing" [TApp "condition-notify-first" [TVar "RecvWait"]]]]) env

channel_send_else_wait_ fargs = FApp "channel-send-else-wait" (fargs)
stepChannel_send_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "Channel") (TName "channels"),VPMetaVar "Val"] env
            rewriteTermTo (TApp "sequential" [TApp "channel-sync-else-wait" [TVar "Channel"],TApp "if-true-else" [TApp "channel-closed" [TVar "Channel"],TApp "sequential" [TApp "channel-release" [TVar "Channel"],TName "fail"],TApp "else" [TApp "channel-send" [TVar "Channel",TVar "Val"],TApp "sequential" [TApp "channel-send-wait" [TVar "Channel"]],TApp "channel-send-else-wait" [TVar "Channel",TVar "Val"]]]]) env

channel_recv_ fargs = FApp "channel-recv" (fargs)
stepChannel_recv fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPMetaVar "Queue",VPMetaVar "N",VPMetaVar "Closed",VPMetaVar "SendWait",VPWildCard,VPWildCard]] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "and" [TApp "not" [TApp "assigned" [TVar "Closed"]],TApp "is-greater" [TApp "list-length" [TApp "assigned" [TVar "Queue"]],TFuncon (FValue (Nat 0))]]],TApp "give" [TApp "list-head" [TApp "assigned" [TVar "Queue"]],TApp "sequential" [TApp "assign" [TVar "N",TApp "integer-add" [TVar "N",TFuncon (FValue (Nat 1))]],TApp "assign" [TVar "Queue",TApp "list-tail" [TApp "assigned" [TVar "Queue"]]],TApp "finalise-failing" [TApp "condition-notify-first" [TVar "SendWait"]],TName "given"]]]]) env

channel_recv_else_wait_ fargs = FApp "channel-recv-else-wait" (fargs)
stepChannel_recv_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "Channel"] env
            rewriteTermTo (TApp "sequential" [TApp "channel-sync-else-wait" [TVar "Channel"],TApp "if-true-else" [TApp "channel-closed" [TVar "Channel"],TApp "sequential" [TApp "channel-release" [TVar "Channel"],TName "fail"],TApp "else" [TApp "channel-recv" [TVar "Channel"],TApp "sequential" [TApp "channel-recv-wait" [TVar "Channel"],TApp "channel-recv-else-wait" [TVar "Channel"]]]]]) env

type_constructs_ = FName "type-constructs"
stepType_constructs = rewriteType "type-constructs" []

channels_ = FName "channels"
stepChannels = rewriteType "channels" []