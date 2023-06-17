-- GeNeRaTeD fOr: ./GO-B-Funcons/GO-B-Funcons.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Golang.GOBFuncons.GOBFuncons where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("type-constructs",DataTypeMemberss "type-constructs" [] [DataTypeInclusionn (TName "types"),DataTypeMemberConstructor "type-construct" [TName "types",TSortSeq (TName "type-constructs") StarOp] (Just [])]),("channels",DataTypeMemberss "channels" [] [DataTypeMemberConstructor "channel" [TName "vars",TName "vars",TName "vars",TName "syncs",TName "syncs",TName "syncs"] (Just [])])]

funcons = libFromList
    [("passthrough",NonStrictFuncon stepPassthrough),("variable-type",StrictFuncon stepVariable_type),("type-construct",StrictFuncon stepType_construct),("type-construct-type",StrictFuncon stepType_construct_type),("type-construct-args",StrictFuncon stepType_construct_args),("exclusive-lock-held",StrictFuncon stepExclusive_lock_held),("exclusive-lock-holder",StrictFuncon stepExclusive_lock_holder),("exclusive-lock-owner",StrictFuncon stepExclusive_lock_owner),("channel",StrictFuncon stepChannel),("channel-create",StrictFuncon stepChannel_create),("channel-display",StrictFuncon stepChannel_display),("channel-size-add",StrictFuncon stepChannel_size_add),("channel-size-sub",StrictFuncon stepChannel_size_sub),("channel-sync-else-wait",StrictFuncon stepChannel_sync_else_wait),("channel-release",StrictFuncon stepChannel_release),("channel-closed",StrictFuncon stepChannel_closed),("channel-await-recv",StrictFuncon stepChannel_await_recv),("channel-send",StrictFuncon stepChannel_send),("channel-recv",StrictFuncon stepChannel_recv),("channel-send-else-wait",StrictFuncon stepChannel_send_else_wait),("channel-recv-else-wait",StrictFuncon stepChannel_recv_else_wait),("type-constructs",NullaryFuncon stepType_constructs),("channels",NullaryFuncon stepChannels)]

passthrough_ fargs = FApp "passthrough" (fargs)
stepPassthrough fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "Res",PSeqVar "Comp*" StarOp] env
            rewriteTermTo (TApp "give" [TVar "Res",TApp "sequential" [TVar "Comp*",TName "given"]]) env

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

exclusive_lock_held_ fargs = FApp "exclusive-lock-held" (fargs)
stepExclusive_lock_held fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-held"]]) env

exclusive_lock_holder_ fargs = FApp "exclusive-lock-holder" (fargs)
stepExclusive_lock_holder fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-holder"]]) env

exclusive_lock_owner_ fargs = FApp "exclusive-lock-owner" (fargs)
stepExclusive_lock_owner fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "and" [TApp "exclusive-lock-held" [TVar "SY"],TApp "is-equal" [TApp "exclusive-lock-holder" [TVar "SY"],TName "current-thread"]]) env

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
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "Size") (TName "nats")] env
            rewriteTermTo (TApp "channel" [TApp "alloc-init" [TApp "lists" [TName "values"],TApp "list" []],TApp "alloc-init" [TName "nats",TVar "Size"],TApp "alloc-init" [TName "bools",TName "false"],TName "condition-create",TName "condition-create",TName "exclusive-lock-create"]) env

channel_display_ fargs = FApp "channel-display" (fargs)
stepChannel_display fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "Str",PADT "channel" [VPMetaVar "Queue",VPMetaVar "Size",VPMetaVar "Closed",VPWildCard,VPWildCard,VPMetaVar "Mutex"]] env
            rewriteTermTo (TApp "thread-atomic" [TApp "print" [TVar "Str",TName "current-thread",TApp "assigned" [TVar "Queue"],TApp "assigned" [TVar "Size"],TApp "assigned" [TVar "Closed"],TApp "assigned" [TApp "sync-feature" [TVar "Mutex",TName "sync-held"]],TApp "assigned" [TApp "sync-feature" [TVar "Mutex",TName "sync-holder"]]]]) env

channel_size_add_ fargs = FApp "channel-size-add" (fargs)
stepChannel_size_add fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPWildCard,VPMetaVar "Size",VPWildCard,VPWildCard,VPWildCard,VPWildCard],VPMetaVar "N"] env
            rewriteTermTo (TApp "assign" [TVar "Size",TApp "integer-add" [TVar "Size",TVar "N"]]) env

channel_size_sub_ fargs = FApp "channel-size-sub" (fargs)
stepChannel_size_sub fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPWildCard,VPMetaVar "Size",VPWildCard,VPWildCard,VPWildCard,VPWildCard],VPMetaVar "N"] env
            rewriteTermTo (TApp "assign" [TVar "Size",TApp "integer-subtract" [TVar "Size",TVar "N"]]) env

channel_sync_else_wait_ fargs = FApp "channel-sync-else-wait" (fargs)
stepChannel_sync_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPWildCard,VPWildCard,VPWildCard,VPWildCard,VPWildCard,VPMetaVar "Mutex"]] env
            rewriteTermTo (TApp "else" [TApp "check-true" [TApp "exclusive-lock-owner" [TVar "Mutex"]],TApp "exclusive-lock-sync-else-wait" [TVar "Mutex"]]) env

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
            rewriteTermTo (TApp "thread-atomic" [TApp "assigned" [TVar "Closed"]]) env

channel_await_recv_ fargs = FApp "channel-await-recv" (fargs)
stepChannel_await_recv fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPWildCard,VPWildCard,VPMetaVar "Closed",VPWildCard,VPMetaVar "AwaitingRecv",VPMetaVar "Mutex"]] env
            rewriteTermTo (TApp "else" [TApp "check-true" [TApp "assigned" [TVar "Closed"]],TApp "sequential" [TApp "print" [TApp "exclusive-lock-owner" [TVar "Mutex"]],TApp "condition-wait-with-lock" [TVar "AwaitingRecv",TVar "Mutex"]]]) env

channel_send_ fargs = FApp "channel-send" (fargs)
stepChannel_send fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPMetaVar "Queue",VPMetaVar "Size",VPMetaVar "Closed",VPMetaVar "AwaitingSend",VPWildCard,VPWildCard],VPMetaVar "Val"] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "and" [TApp "not" [TApp "assigned" [TVar "Closed"]],TApp "is-less" [TApp "list-length" [TApp "assigned" [TVar "Queue"]],TApp "assigned" [TVar "Size"]]]],TApp "assign" [TVar "Queue",TApp "list-append" [TApp "assigned" [TVar "Queue"],TApp "list" [TVar "Val"]]],TApp "finalise-failing" [TApp "condition-notify-first" [TVar "AwaitingSend"]]]]) env

channel_recv_ fargs = FApp "channel-recv" (fargs)
stepChannel_recv fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPMetaVar "Queue",VPWildCard,VPMetaVar "Closed",VPWildCard,VPMetaVar "AwaitingRecv",VPWildCard]] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "and" [TApp "not" [TApp "assigned" [TVar "Closed"]],TApp "is-greater" [TApp "list-length" [TApp "assigned" [TVar "Queue"]],TFuncon (FValue (Nat 0))]]],TApp "passthrough" [TApp "list-head" [TApp "assigned" [TVar "Queue"]],TApp "assign" [TVar "Queue",TApp "list-tail" [TApp "assigned" [TVar "Queue"]]],TApp "finalise-failing" [TApp "condition-notify-first" [TVar "AwaitingRecv"]]]]]) env

channel_send_else_wait_ fargs = FApp "channel-send-else-wait" (fargs)
stepChannel_send_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "Channel",VPMetaVar "Val"] env
            rewriteTermTo (TApp "sequential" [TApp "channel-sync-else-wait" [TVar "Channel"],TApp "print" [TFuncon (FValue (Nat 100))],TApp "if-true-else" [TApp "channel-closed" [TVar "Channel"],TApp "sequential" [TApp "print" [TFuncon (FValue (Nat 200))],TApp "channel-release" [TVar "Channel"],TName "fail"],TApp "else" [TApp "sequential" [TApp "channel-send" [TVar "Channel",TVar "Val"],TApp "print" [TFuncon (FValue (Nat 300))],TApp "channel-release" [TVar "Channel"]],TApp "sequential" [TApp "print" [TFuncon (FValue (Nat 400))],TApp "channel-await-recv" [TVar "Channel"],TApp "print" [TFuncon (FValue (Nat 500))],TApp "channel-send-else-wait" [TVar "Channel",TVar "Val"]]]]]) env

channel_recv_else_wait_ fargs = FApp "channel-recv-else-wait" (fargs)
stepChannel_recv_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "Channel"] env
            rewriteTermTo (TApp "sequential" [TApp "channel-sync-else-wait" [TVar "Channel"],TApp "if-true-else" [TApp "channel-closed" [TVar "Channel"],TApp "sequential" [TApp "channel-release" [TVar "Channel"],TName "fail"],TApp "else" [TApp "passthrough" [TApp "channel-recv" [TVar "Channel"],TApp "channel-release" [TVar "Channel"]],TApp "sequential" [TApp "channel-size-add" [TVar "Channel",TFuncon (FValue (Nat 1))],TApp "channel-await-send" [TVar "Channel"],TApp "channel-size-sub" [TVar "Channel",TFuncon (FValue (Nat 1))],TApp "channel-recv-else-wait" [TVar "Channel"]]]]]) env

type_constructs_ = FName "type-constructs"
stepType_constructs = rewriteType "type-constructs" []

channels_ = FName "channels"
stepChannels = rewriteType "channels" []