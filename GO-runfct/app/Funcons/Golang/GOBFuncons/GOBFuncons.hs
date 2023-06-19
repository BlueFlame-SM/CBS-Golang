-- GeNeRaTeD fOr: ./GO-B-Funcons/GO-B-Funcons.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Golang.GOBFuncons.GOBFuncons where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("channels",DataTypeMemberss "channels" [] [DataTypeMemberConstructor "channel" [TName "natural-numbers",TName "variables",TName "variables",TName "variables"] (Just [])]),("pseudo-threads",DataTypeMemberss "pseudo-threads" [] [DataTypeMemberConstructor "pseudo-thread" [TName "variables",TName "thread-ids"] (Just [])])]

funcons = libFromList
    [("to-strings",StrictFuncon stepTo_strings),("channel",StrictFuncon stepChannel),("channel-create",StrictFuncon stepChannel_create),("channel-print",StrictFuncon stepChannel_print),("channel-send",StrictFuncon stepChannel_send),("channel-receive",StrictFuncon stepChannel_receive),("channel-send-else-wait",StrictFuncon stepChannel_send_else_wait),("channel-receive-else-wait",StrictFuncon stepChannel_receive_else_wait),("channel-send-directly",StrictFuncon stepChannel_send_directly),("channel-recv-directly",StrictFuncon stepChannel_recv_directly),("channel-send-buffer",StrictFuncon stepChannel_send_buffer),("channel-recv-buffer",StrictFuncon stepChannel_recv_buffer),("channel-send-wait",StrictFuncon stepChannel_send_wait),("channel-recv-wait",StrictFuncon stepChannel_recv_wait),("dequeue",StrictFuncon stepDequeue),("enqueue",StrictFuncon stepEnqueue),("pseudo-thread",StrictFuncon stepPseudo_thread),("pseudo-thread-create",StrictFuncon stepPseudo_thread_create),("pseudo-thread-value",StrictFuncon stepPseudo_thread_value),("pseudo-thread-id",StrictFuncon stepPseudo_thread_id),("channels",NullaryFuncon stepChannels),("pseudo-threads",NullaryFuncon stepPseudo_threads)]

to_strings_ fargs = FApp "to-strings" (fargs)
stepTo_strings fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [] env
            rewriteTermTo (TSeq []) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TSeq [TApp "to-string" [TVar "V"],TApp "to-strings" [TVar "V*"]]) env

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
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "Size") (TName "natural-numbers")] env
            rewriteTermTo (TApp "channel" [TVar "Size",TApp "alloc-init" [TApp "lists" [TName "values"],TApp "list" []],TApp "alloc-init" [TApp "lists" [TName "pseudo-threads"],TApp "list" []],TApp "alloc-init" [TApp "lists" [TName "pseudo-threads"],TApp "list" []]]) env

channel_print_ fargs = FApp "channel-print" (fargs)
stepChannel_print fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "C") (TName "channels")] env
            rewriteTermTo (TApp "print" [TApp "structural-assigned" [TVar "C"]]) env

channel_send_ fargs = FApp "channel-send" (fargs)
stepChannel_send fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "C") (TName "channels"),VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "else" [TApp "channel-send-directly" [TVar "C",TVar "V"],TApp "channel-send-buffer" [TVar "C",TVar "V"]]]) env

channel_receive_ fargs = FApp "channel-receive" (fargs)
stepChannel_receive fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "C") (TName "channels")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "else" [TApp "channel-recv-buffer" [TVar "C"],TApp "channel-recv-directly" [TVar "C"]]]) env

channel_send_else_wait_ fargs = FApp "channel-send-else-wait" (fargs)
stepChannel_send_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "C") (TName "channels"),VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "sequential" [TApp "thread-atomic" [TApp "else" [TApp "channel-send" [TVar "C",TVar "V"],TApp "channel-send-wait" [TVar "C",TApp "pseudo-thread-create" [TVar "V"]]]],TApp "postpone" [TName "null"]]) env

channel_receive_else_wait_ fargs = FApp "channel-receive-else-wait" (fargs)
stepChannel_receive_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "C") (TName "channels")] env
            rewriteTermTo (TApp "give" [TApp "pseudo-thread-create" [],TApp "sequential" [TApp "thread-atomic" [TApp "else" [TApp "assign" [TApp "pseudo-thread-value" [TName "given"],TApp "channel-receive" [TVar "C"]],TApp "channel-recv-wait" [TVar "C",TName "given"]]],TApp "postpone" [TName "null"],TApp "assigned" [TApp "pseudo-thread-value" [TName "given"]]]]) env

channel_send_directly_ fargs = FApp "channel-send-directly" (fargs)
stepChannel_send_directly fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPWildCard,VPWildCard,VPMetaVar "RecvQ",VPWildCard],VPMetaVar "V"] env
            rewriteTermTo (TApp "thread-atomic" [TApp "give" [TApp "dequeue" [TVar "RecvQ"],TApp "sequential" [TApp "assign" [TApp "pseudo-thread-value" [TName "given"],TVar "V"],TApp "thread-resume" [TApp "pseudo-thread-id" [TName "given"]]]]]) env

channel_recv_directly_ fargs = FApp "channel-recv-directly" (fargs)
stepChannel_recv_directly fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPWildCard,VPWildCard,VPWildCard,VPMetaVar "SendQ"]] env
            rewriteTermTo (TApp "thread-atomic" [TApp "give" [TApp "dequeue" [TVar "SendQ"],TApp "sequential" [TApp "thread-resume" [TApp "pseudo-thread-id" [TName "given"]],TApp "assigned" [TApp "pseudo-thread-value" [TName "given"]]]]]) env

channel_send_buffer_ fargs = FApp "channel-send-buffer" (fargs)
stepChannel_send_buffer fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPMetaVar "Size",VPMetaVar "Buffer",VPWildCard,VPWildCard],VPMetaVar "V"] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "is-less" [TApp "list-length" [TApp "assigned" [TVar "Buffer"]],TVar "Size"]],TApp "enqueue" [TVar "Buffer",TVar "V"]]]) env

channel_recv_buffer_ fargs = FApp "channel-recv-buffer" (fargs)
stepChannel_recv_buffer fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPWildCard,VPMetaVar "Buffer",VPWildCard,VPMetaVar "SendQ"]] env
            rewriteTermTo (TApp "thread-atomic" [TApp "after-effect" [TApp "dequeue" [TVar "Buffer"],TApp "finalise-failing" [TApp "give" [TApp "dequeue" [TVar "SendQ"],TApp "sequential" [TApp "enqueue" [TVar "Buffer",TApp "pseudo-thread-value" [TName "given"]],TApp "thread-resume" [TApp "pseudo-thread-id" [TName "given"]]]]]]]) env

channel_send_wait_ fargs = FApp "channel-send-wait" (fargs)
stepChannel_send_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPWildCard,VPWildCard,VPWildCard,VPMetaVar "SendQ"],VPMetaVar "P"] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "enqueue" [TVar "SendQ",TVar "P"],TApp "thread-suspend" [TName "current-thread"]]]) env

channel_recv_wait_ fargs = FApp "channel-recv-wait" (fargs)
stepChannel_recv_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "channel" [VPWildCard,VPWildCard,VPMetaVar "RecvQ",VPWildCard],VPMetaVar "P"] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "enqueue" [TVar "RecvQ",TVar "P"],TApp "thread-suspend" [TName "current-thread"]]]) env

dequeue_ fargs = FApp "dequeue" (fargs)
stepDequeue fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "Q") (TName "variables")] env
            rewriteTermTo (TApp "after-effect" [TApp "checked" [TApp "list-head" [TApp "assigned" [TVar "Q"]]],TApp "assign" [TVar "Q",TApp "checked" [TApp "list-tail" [TApp "assigned" [TVar "Q"]]]]]) env

enqueue_ fargs = FApp "enqueue" (fargs)
stepEnqueue fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "Q") (TName "variables"),VPAnnotated (VPSeqVar "V*" StarOp) (TName "values")] env
            rewriteTermTo (TApp "assign" [TVar "Q",TApp "list-append" [TApp "assigned" [TVar "Q"],TApp "list" [TVar "V*"]]]) env

pseudo_thread_ fargs = FApp "pseudo-thread" (fargs)
stepPseudo_thread fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1",VPMetaVar "_X2"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X2") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 112)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 117)]),FValue (ADTVal "unicode-character" [FValue (Int 100)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 45)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 104)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 100)])])),TVar "_X1",TVar "_X2"]) env

pseudo_thread_create_ fargs = FApp "pseudo-thread-create" (fargs)
stepPseudo_thread_create fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [] env
            rewriteTermTo (TApp "pseudo-thread" [TApp "alloc" [TName "values"],TName "current-thread"]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "pseudo-thread" [TApp "alloc-init" [TName "values",TVar "V"],TName "current-thread"]) env

pseudo_thread_value_ fargs = FApp "pseudo-thread-value" (fargs)
stepPseudo_thread_value fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "pseudo-thread" [VPMetaVar "V",VPWildCard]] env
            rewriteTermTo (TVar "V") env

pseudo_thread_id_ fargs = FApp "pseudo-thread-id" (fargs)
stepPseudo_thread_id fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "pseudo-thread" [VPWildCard,VPMetaVar "I"]] env
            rewriteTermTo (TVar "I") env

channels_ = FName "channels"
stepChannels = rewriteType "channels" []

pseudo_threads_ = FName "pseudo-threads"
stepPseudo_threads = rewriteType "pseudo-threads" []