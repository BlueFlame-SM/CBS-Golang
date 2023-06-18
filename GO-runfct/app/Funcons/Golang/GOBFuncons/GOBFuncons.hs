-- GeNeRaTeD fOr: ./GO-B-Funcons/GO-B-Funcons.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Golang.GOBFuncons.GOBFuncons where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("type-constructs",DataTypeMemberss "type-constructs" [] [DataTypeInclusionn (TName "types"),DataTypeMemberConstructor "type-construct" [TName "types",TSortSeq (TName "type-constructs") StarOp] (Just [])]),("pseudo-threads",DataTypeMemberss "pseudo-threads" [] [DataTypeMemberConstructor "pseudo-thread" [TName "pseudo-thread-modes",TName "variables",TName "thread-ids",TName "variables"] (Just [])]),("pseudo-thread-modes",DataTypeMemberss "pseudo-thread-modes" [] [DataTypeMemberConstructor "sending" [] (Just []),DataTypeMemberConstructor "receiving" [] (Just [])])]

funcons = libFromList
    [("after-multiple-effect",NonStrictFuncon stepAfter_multiple_effect),("find-first",StrictFuncon stepFind_first),("drop-nth",StrictFuncon stepDrop_nth),("drop-first-match",StrictFuncon stepDrop_first_match),("type-construct",StrictFuncon stepType_construct),("type-construct-type",StrictFuncon stepType_construct_type),("type-construct-args",StrictFuncon stepType_construct_args),("sync-waiting-list-match-remove",StrictFuncon stepSync_waiting_list_match_remove),("sync-waiting-list-add-head",StrictFuncon stepSync_waiting_list_add_head),("sync-feature-modify",PartiallyStrictFuncon [Strict,Strict,NonStrict] NonStrict stepSync_feature_modify),("sync-count-increase",StrictFuncon stepSync_count_increase),("sync-count-decrease",StrictFuncon stepSync_count_decrease),("exclusive-lock-grant",StrictFuncon stepExclusive_lock_grant),("is-condition-empty",StrictFuncon stepIs_condition_empty),("condition-wait-head",StrictFuncon stepCondition_wait_head),("condition-wait-head-with-lock",StrictFuncon stepCondition_wait_head_with_lock),("channel-create",StrictFuncon stepChannel_create),("channel-print",StrictFuncon stepChannel_print),("channel-send",StrictFuncon stepChannel_send),("channel-receive",StrictFuncon stepChannel_receive),("channel-send-else-wait",StrictFuncon stepChannel_send_else_wait),("channel-receive-else-wait",StrictFuncon stepChannel_receive_else_wait),("pseudo-thread",StrictFuncon stepPseudo_thread),("sending",NullaryFuncon stepSending),("receiving",NullaryFuncon stepReceiving),("pseudo-thread-create",StrictFuncon stepPseudo_thread_create),("pseudo-thread-value",StrictFuncon stepPseudo_thread_value),("pseudo-thread-id",StrictFuncon stepPseudo_thread_id),("pseudo-thread-waiting",StrictFuncon stepPseudo_thread_waiting),("first-pseudo-thread-match",StrictFuncon stepFirst_pseudo_thread_match),("drop-first-pseudo-thread-match",StrictFuncon stepDrop_first_pseudo_thread_match),("type-constructs",NullaryFuncon stepType_constructs),("pseudo-threads",NullaryFuncon stepPseudo_threads),("pseudo-thread-modes",NullaryFuncon stepPseudo_thread_modes)]

after_multiple_effect_ fargs = FApp "after-multiple-effect" (fargs)
stepAfter_multiple_effect fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "Res",PSeqVar "Comp*" StarOp] env
            rewriteTermTo (TApp "give" [TVar "Res",TApp "sequential" [TVar "Comp*",TName "given"]]) env

find_first_ fargs = FApp "find-first" (fargs)
stepFind_first fargs =
    evalRules [rewrite1,rewrite2,rewrite3] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TFuncon (FValue (Nat 1))) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPMetaVar "V'") (TName "values"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            env <- sideCondition (SCEquality (TApp "is-equal" [TVar "V",TVar "V'"]) (TName "true")) env
            rewriteTermTo (TFuncon (FValue (Nat 1))) env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPMetaVar "V'") (TName "values"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            env <- sideCondition (SCEquality (TApp "is-equal" [TVar "V",TVar "V'"]) (TName "false")) env
            rewriteTermTo (TApp "natural-successor" [TApp "find-first" [TVar "V",TVar "V*"]]) env

drop_nth_ fargs = FApp "drop-nth" (fargs)
stepDrop_nth fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPLit (Nat 0),VPAnnotated (VPSeqVar "V*" StarOp) (TVar "T*")] env
            rewriteTermTo (TVar "V*") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "N") (TName "positive-integers"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            env <- sideCondition (SCPatternMatch (TApp "natural-predecessor" [TVar "N"]) [VPMetaVar "N'"]) env
            rewriteTermTo (TSeq [TApp "first-n" [TVar "N'",TVar "V*"],TApp "drop-first-n" [TVar "N",TVar "V*"]]) env

drop_first_match_ fargs = FApp "drop-first-match" (fargs)
stepDrop_first_match fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TApp "drop-nth" [TApp "find-first" [TVar "V",TVar "V*"],TVar "V*"]) env

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

sync_waiting_list_match_remove_ fargs = FApp "sync-waiting-list-match-remove" (fargs)
stepSync_waiting_list_match_remove fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs"),VPAnnotated (VPMetaVar "TID") (TName "thread-ids")] env
            rewriteTermTo (TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"],TApp "list" [TApp "drop-first-match" [TVar "TID",TApp "list-elements" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"]]]]]]) env

sync_waiting_list_add_head_ fargs = FApp "sync-waiting-list-add-head" (fargs)
stepSync_waiting_list_add_head fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs"),VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"],TApp "list-append" [TApp "list" [TVar "V"],TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"]]]]) env

sync_feature_modify_ fargs = FApp "sync-feature-modify" (fargs)
stepSync_feature_modify fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "SY") (TName "syncs"),PAnnotated (PMetaVar "SF") (TName "sync-features"),PMetaVar "V"] env
            rewriteTermTo (TApp "give" [TApp "sync-feature" [TVar "SY",TVar "SF"],TApp "assign" [TName "given",TApp "give" [TApp "assigned" [TName "given"],TVar "V"]]]) env

sync_count_increase_ fargs = FApp "sync-count-increase" (fargs)
stepSync_count_increase fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "sync-feature-modify" [TVar "SY",TName "sync-count",TApp "nat-succ" [TName "given"]]) env

sync_count_decrease_ fargs = FApp "sync-count-decrease" (fargs)
stepSync_count_decrease fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "sync-feature-modify" [TVar "SY",TName "sync-count",TApp "nat-pred" [TName "given"]]) env

exclusive_lock_grant_ fargs = FApp "exclusive-lock-grant" (fargs)
stepExclusive_lock_grant fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs"),VPAnnotated (VPMetaVar "TID") (TName "thread-ids")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "is-exclusive-lock-holder" [TVar "SY"]],TApp "if-true-else" [TApp "is-in" [TVar "TID",TApp "list-elements" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"]]]],TApp "sequential" [TApp "sync-waiting-list-match-remove" [TVar "SY",TVar "TID"],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-holder"],TVar "TID"],TApp "thread-resume" [TVar "TID"]],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-holder"],TVar "TID"]]]]) env

is_condition_empty_ fargs = FApp "is-condition-empty" (fargs)
stepIs_condition_empty fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "is-equal" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"],TApp "list" []]]) env

condition_wait_head_ fargs = FApp "condition-wait-head" (fargs)
stepCondition_wait_head fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "sync-waiting-list-add-head" [TVar "SY",TName "current-thread"],TApp "thread-suspend" [TName "current-thread"]]]) env

condition_wait_head_with_lock_ fargs = FApp "condition-wait-head-with-lock" (fargs)
stepCondition_wait_head_with_lock fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs"),VPAnnotated (VPMetaVar "L") (TName "syncs")] env
            rewriteTermTo (TApp "sequential" [TApp "thread-atomic" [TApp "sequential" [TApp "exclusive-lock-release" [TVar "L"],TApp "sync-waiting-list-add-head" [TVar "SY",TName "current-thread"],TApp "thread-suspend" [TName "current-thread"]]],TApp "exclusive-lock-sync-else-wait" [TVar "L"]]) env

channel_create_ fargs = FApp "channel-create" (fargs)
stepChannel_create fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "N") (TName "nats")] env
            rewriteTermTo (TApp "give" [TApp "sync-create" [TApp "sync-feature-create" [TName "sync-waiting-list"],TApp "sync-feature-create" [TName "sync-count"]],TApp "sequential" [TApp "sync-feature-modify" [TName "given",TName "sync-count",TVar "N"],TName "given"]]) env

channel_print_ fargs = FApp "channel-print" (fargs)
stepChannel_print fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "print" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-count"]],TApp "structural-assigned" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"]]]]) env

channel_send_ fargs = FApp "channel-send" (fargs)
stepChannel_send fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs"),VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "give" [TApp "first-pseudo-thread-match" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"]],TName "receiving"],TApp "sequential" [TApp "assign" [TApp "pseudo-thread-value" [TName "given"],TVar "V"],TApp "sync-feature-modify" [TVar "SY",TName "sync-waiting-list",TApp "drop-first-pseudo-thread-match" [TName "given",TName "receiving"]],TApp "thread-resume" [TApp "pseudo-thread-id" [TName "given"]]]]]) env

channel_receive_ fargs = FApp "channel-receive" (fargs)
stepChannel_receive fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "give" [TApp "first-pseudo-thread-match" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"]],TName "sending"],TApp "sequential" [TApp "print" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 49)])]))],TApp "sync-count-increase" [TVar "SY"],TApp "print" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 50)])]))],TApp "sync-feature-modify" [TVar "SY",TName "sync-waiting-list",TApp "drop-first-pseudo-thread-match" [TName "given",TName "sending"]],TApp "print" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 51)])]))],TApp "if-true-else" [TApp "assigned" [TApp "pseudo-thread-waiting" [TName "given"]],TApp "thread-resume" [TApp "pseudo-thread-id" [TName "given"]],TName "null-value"],TApp "print" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 52)])]))],TApp "assigned" [TApp "pseudo-thread-value" [TName "given"]]]]]) env

channel_send_else_wait_ fargs = FApp "channel-send-else-wait" (fargs)
stepChannel_send_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs"),VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "else" [TApp "channel-send" [TVar "SY",TVar "V"],TApp "if-true-else" [TApp "is-equal" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-count"]],TFuncon (FValue (Nat 0))],TApp "sequential" [TApp "sync-waiting-list-add" [TVar "SY",TApp "pseudo-thread-create" [TName "sending",TVar "V",TName "true"]],TApp "thread-suspend" [TName "current-thread"]],TApp "sequential" [TApp "sync-waiting-list-add" [TVar "SY",TApp "pseudo-thread-create" [TName "sending",TVar "V",TName "false"]],TApp "sync-count-decrease" [TVar "SY"]]]]]) env

channel_receive_else_wait_ fargs = FApp "channel-receive-else-wait" (fargs)
stepChannel_receive_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "give" [TApp "pseudo-thread-create" [TName "receiving"],TApp "sequential" [TApp "thread-atomic" [TApp "else" [TApp "assign" [TApp "pseudo-thread-value" [TName "given"],TApp "channel-receive" [TVar "SY"]],TApp "thread-suspend" [TName "current-thread"]]],TApp "assigned" [TApp "pseudo-thread-value" [TName "given"]]]]) env

pseudo_thread_ fargs = FApp "pseudo-thread" (fargs)
stepPseudo_thread fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1",VPMetaVar "_X2",VPMetaVar "_X3",VPMetaVar "_X4"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X2") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X3") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X4") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 112)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 117)]),FValue (ADTVal "unicode-character" [FValue (Int 100)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 45)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 104)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 100)])])),TVar "_X1",TVar "_X2",TVar "_X3",TVar "_X4"]) env

sending_ = FName "sending"
stepSending = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 100)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 103)])]))]) env

receiving_ = FName "receiving"
stepReceiving = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 118)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 103)])]))]) env

pseudo_thread_create_ fargs = FApp "pseudo-thread-create" (fargs)
stepPseudo_thread_create fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "sending" [],VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPMetaVar "W") (TName "booleans")] env
            rewriteTermTo (TApp "pseudo-thread" [TName "sending",TApp "alloc-init" [TName "values",TVar "V"],TName "current-thread",TApp "alloc-init" [TName "booleans",TVar "W"]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "receiving" []] env
            rewriteTermTo (TApp "pseudo-thread" [TName "receiving",TApp "alloc" [TName "values"],TName "current-thread",TApp "alloc-init" [TName "booleans",TName "true"]]) env

pseudo_thread_value_ fargs = FApp "pseudo-thread-value" (fargs)
stepPseudo_thread_value fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "pseudo-thread" [VPWildCard,VPMetaVar "V",VPWildCard,VPWildCard]] env
            rewriteTermTo (TVar "V") env

pseudo_thread_id_ fargs = FApp "pseudo-thread-id" (fargs)
stepPseudo_thread_id fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "pseudo-thread" [VPWildCard,VPWildCard,VPMetaVar "I",VPWildCard]] env
            rewriteTermTo (TVar "I") env

pseudo_thread_waiting_ fargs = FApp "pseudo-thread-waiting" (fargs)
stepPseudo_thread_waiting fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "pseudo-thread" [VPWildCard,VPWildCard,VPWildCard,VPMetaVar "W"]] env
            rewriteTermTo (TVar "W") env

first_pseudo_thread_match_ fargs = FApp "first-pseudo-thread-match" (fargs)
stepFirst_pseudo_thread_match fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),PADT "pseudo-thread" [VPMetaVar "M",VPMetaVar "V",VPMetaVar "T",VPMetaVar "W"],VPSeqVar "P*" StarOp],VPMetaVar "M'"] env
            rewriteTermTo (TApp "if-true-else" [TApp "is-equal" [TVar "M",TVar "M'"],TApp "pseudo-thread" [TVar "M",TVar "V",TVar "T",TVar "W"],TApp "first-pseudo-thread-match" [TApp "list" [TVar "P*"],TVar "M'"]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])])],VPWildCard] env
            rewriteTermTo (TName "fail") env

drop_first_pseudo_thread_match_ fargs = FApp "drop-first-pseudo-thread-match" (fargs)
stepDrop_first_pseudo_thread_match fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),PADT "pseudo-thread" [VPMetaVar "M",VPMetaVar "V",VPMetaVar "T",VPMetaVar "W"],VPSeqVar "P*" StarOp],VPMetaVar "M'"] env
            rewriteTermTo (TApp "if-true-else" [TApp "is-equal" [TVar "M",TVar "M'"],TApp "list" [TVar "P*"],TApp "cons" [TApp "pseudo-thread" [TVar "M",TVar "V",TVar "T",TVar "W"],TApp "first-pseudo-thread-match" [TApp "list" [TVar "P*"],TVar "M'"]]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])])],VPWildCard] env
            rewriteTermTo (TName "fail") env

type_constructs_ = FName "type-constructs"
stepType_constructs = rewriteType "type-constructs" []

pseudo_threads_ = FName "pseudo-threads"
stepPseudo_threads = rewriteType "pseudo-threads" []

pseudo_thread_modes_ = FName "pseudo-thread-modes"
stepPseudo_thread_modes = rewriteType "pseudo-thread-modes" []