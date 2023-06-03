-- GeNeRaTeD fOr: ./GO-B-Funcons/GO-B-Funcons.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Golang.GOBFuncons.GOBFuncons where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("channels",DataTypeMemberss "channels" [TPVar "T"] [DataTypeMemberConstructor "channel" [TName "vars",TApp "vars" [TAny],TName "syncs",TName "syncs"] (Just [TPVar "T"])])]

funcons = libFromList
    [("list-snoc",StrictFuncon stepList_snoc),("list-init",StrictFuncon stepList_init),("list-last",StrictFuncon stepList_last),("lists-type",StrictFuncon stepLists_type),("variable-type",StrictFuncon stepVariable_type),("channel",StrictFuncon stepChannel),("channel-create",PartiallyStrictFuncon [NonStrict,Strict] NonStrict stepChannel_create),("channel-type",StrictFuncon stepChannel_type),("channel-send",StrictFuncon stepChannel_send),("channel-send-else-wait",StrictFuncon stepChannel_send_else_wait),("channel-receive",StrictFuncon stepChannel_receive),("channel-receive-else-wait",StrictFuncon stepChannel_receive_else_wait),("channels",StrictFuncon stepChannels)]

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
            env <- vsMatch fargs [VPMetaVar "_X1",VPMetaVar "_X2",VPMetaVar "_X3",VPMetaVar "_X4"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X2") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X3") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X4") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 104)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 108)])])),TVar "_X1",TVar "_X2",TVar "_X3",TVar "_X4"]) env

channel_create_ fargs = FApp "channel-create" (fargs)
stepChannel_create fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "T",PAnnotated (PMetaVar "N") (TName "nats")] env
            rewriteTermTo (TApp "channel" [TApp "alloc-init" [TApp "lists" [TVar "T"],TApp "list" []],TApp "alloc-init" [TName "nats",TVar "N"],TApp "alloc-init" [TName "bools",TName "false"],TName "condition-create",TName "condition-create"]) env

channel_type_ fargs = FApp "channel-type" (fargs)
stepChannel_type fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPAnnotated (VPSeqVar "___" StarOp) (TSortSeq (TName "values") StarOp),VPAnnotated (VPMetaVar "Var") (TName "variables"),VPAnnotated VPWildCard (TName "values")]] env
            rewriteTermTo (TApp "lists-type" [TApp "variable-type" [TVar "Var"]]) env

channel_send_ fargs = FApp "channel-send" (fargs)
stepChannel_send fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPMetaVar "Queue",VPMetaVar "N",VPMetaVar "Closed",VPMetaVar "SendCond",VPMetaVar "ReceiveCond"],VPMetaVar "X"] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "not" [TApp "assigned" [TVar "Closed"]]],TApp "check-true" [TApp "is-greater" [TApp "assigned" [TVar "N"],TFuncon (FValue (Nat 0))]],TApp "sequential" [TApp "assign" [TVar "N",TApp "integer-subtract" [TApp "assigned" [TVar "N"],TFuncon (FValue (Nat 1))]],TApp "assign" [TVar "Queue",TApp "list-snoc" [TApp "assigned" [TVar "Queue"],TVar "X"]],TApp "condition-notify-first" [TVar "ReceiveCond"]]]]) env

channel_send_else_wait_ fargs = FApp "channel-send-else-wait" (fargs)
stepChannel_send_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPMetaVar "Queue",VPMetaVar "N",VPMetaVar "Closed",VPMetaVar "SendCond",VPMetaVar "ReceiveCond"],VPMetaVar "X"] env
            rewriteTermTo (TApp "handle-return" [TApp "while" [TName "true",TApp "thread-atomic" [TApp "else" [TApp "return" [TApp "channel-send" [TApp "channel" [TVar "Queue",TVar "N",TVar "Closed",TVar "SendCond",TVar "ReceiveCond"],TVar "X"]],TApp "sequential" [TApp "check-true" [TApp "not" [TApp "assigned" [TVar "Closed"]]],TApp "condition-wait" [TVar "SendCond"]]]]]]) env

channel_receive_ fargs = FApp "channel-receive" (fargs)
stepChannel_receive fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPMetaVar "Queue",VPMetaVar "N",VPMetaVar "Closed",VPMetaVar "SendCond",VPMetaVar "ReceiveCond"]] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "not" [TApp "assigned" [TVar "Closed"]]],TApp "check-true" [TApp "is-greater" [TApp "list-length" [TApp "assigned" [TVar "Queue"]],TFuncon (FValue (Nat 0))]],TApp "give" [TApp "list-head" [TApp "assigned" [TVar "Queue"]],TApp "sequential" [TApp "assign" [TVar "N",TApp "integer-add" [TVar "N",TFuncon (FValue (Nat 1))]],TApp "assign" [TVar "Queue",TApp "list-tail" [TApp "assigned" [TVar "Queue"]]],TApp "condition-notify-first" [TVar "SendCond"],TName "given"]]]]) env

channel_receive_else_wait_ fargs = FApp "channel-receive-else-wait" (fargs)
stepChannel_receive_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPMetaVar "Queue",VPMetaVar "N",VPMetaVar "Closed",VPMetaVar "SendCond",VPMetaVar "ReceiveCond"]] env
            rewriteTermTo (TApp "handle-return" [TApp "while" [TName "true",TApp "thread-atomic" [TApp "else" [TApp "return" [TApp "channel-receive" [TApp "channel" [TVar "Queue",TVar "N",TVar "Closed",TVar "SendCond",TVar "ReceiveCond"]]],TApp "sequential" [TApp "check-true" [TApp "not" [TApp "assigned" [TVar "Closed"]]],TApp "condition-wait" [TVar "ReceiveCond"]]]]]]) env

channels_ = FApp "channels"
stepChannels ts = rewriteType "channels" ts