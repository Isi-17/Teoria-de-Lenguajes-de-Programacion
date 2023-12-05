{-# OPTIONS_GHC -w #-}
module WhileParser ( happyParseWhile
                   , parser
                   ) where

import State
import Aexp
import Bexp
import While
import WhileLexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 ((String, [Var], Stm))
	| HappyAbsSyn5 ([Var])
	| HappyAbsSyn7 (Stm)
	| HappyAbsSyn9 (Bexp)
	| HappyAbsSyn10 (Aexp)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72 :: () => Prelude.Int -> ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,138) ([0,0,64,0,32768,8192,0,0,0,0,0,8,0,0,64,0,2,0,32768,0,64,0,0,4096,18944,53,0,0,0,4096,0,0,8,0,0,0,41728,5,0,2886,0,2048,42240,26,16,0,0,0,0,64,54568,0,0,0,256,0,0,0,0,0,0,16,32768,0,0,0,2,8192,256,32768,771,0,0,0,0,0,0,41728,5,0,0,0,0,0,0,11544,0,0,8224,0,1120,0,32768,20480,426,0,0,0,28,0,17920,0,0,5772,0,4096,18944,53,0,0,0,72,0,5888,6,32768,17,0,8960,0,0,70,0,35840,0,0,280,0,8192,37888,106,46176,0,49152,8,0,0,0,0,28,256,0,4,0,0,0,57344,0,0,448,0,0,0,0,1024,0,0,8,0,0,0,0,0,0,0,4096,0,0,0,49152,5,0,64,54568,49152,8,0,3584,2048,0,0,0,1024,21120,13,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_happyParseWhile","Program","Outputs","Var_List","Stm_List","Stm","Bexp","Aexp","NUM","ID","'+'","'-'","'*'","'('","')'","'true'","'false'","'&&'","'!'","'='","'<='","':='","SKIP","';'","'if'","'then'","'else'","'while'","'do'","'repeat'","'until'","'for'","'to'","ABORT","'begin'","'end'","'program'","','","%eof"]
        bit_start = st Prelude.* 41
        bit_end = (st Prelude.+ 1) Prelude.* 41
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..40]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (39) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (39) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (12) = happyShift action_4
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (41) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (16) = happyShift action_6
action_4 (5) = happyGoto action_5
action_4 _ = happyReduce_3

action_5 (26) = happyShift action_9
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (12) = happyShift action_8
action_6 (6) = happyGoto action_7
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (17) = happyShift action_20
action_7 (40) = happyShift action_21
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_4

action_9 (12) = happyShift action_12
action_9 (25) = happyShift action_13
action_9 (27) = happyShift action_14
action_9 (30) = happyShift action_15
action_9 (32) = happyShift action_16
action_9 (34) = happyShift action_17
action_9 (36) = happyShift action_18
action_9 (37) = happyShift action_19
action_9 (7) = happyGoto action_10
action_9 (8) = happyGoto action_11
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_1

action_11 (26) = happyShift action_36
action_11 _ = happyReduce_6

action_12 (24) = happyShift action_35
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_9

action_14 (11) = happyShift action_28
action_14 (12) = happyShift action_29
action_14 (16) = happyShift action_30
action_14 (18) = happyShift action_31
action_14 (19) = happyShift action_32
action_14 (21) = happyShift action_33
action_14 (9) = happyGoto action_34
action_14 (10) = happyGoto action_27
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (11) = happyShift action_28
action_15 (12) = happyShift action_29
action_15 (16) = happyShift action_30
action_15 (18) = happyShift action_31
action_15 (19) = happyShift action_32
action_15 (21) = happyShift action_33
action_15 (9) = happyGoto action_26
action_15 (10) = happyGoto action_27
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (12) = happyShift action_12
action_16 (25) = happyShift action_13
action_16 (27) = happyShift action_14
action_16 (30) = happyShift action_15
action_16 (32) = happyShift action_16
action_16 (34) = happyShift action_17
action_16 (36) = happyShift action_18
action_16 (37) = happyShift action_19
action_16 (7) = happyGoto action_25
action_16 (8) = happyGoto action_11
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (12) = happyShift action_24
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_10

action_19 (12) = happyShift action_12
action_19 (25) = happyShift action_13
action_19 (27) = happyShift action_14
action_19 (30) = happyShift action_15
action_19 (32) = happyShift action_16
action_19 (34) = happyShift action_17
action_19 (36) = happyShift action_18
action_19 (37) = happyShift action_19
action_19 (7) = happyGoto action_23
action_19 (8) = happyGoto action_11
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_2

action_21 (12) = happyShift action_22
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_5

action_23 (38) = happyShift action_53
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (24) = happyShift action_52
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (33) = happyShift action_51
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (20) = happyShift action_40
action_26 (31) = happyShift action_50
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (13) = happyShift action_45
action_27 (14) = happyShift action_46
action_27 (15) = happyShift action_47
action_27 (22) = happyShift action_48
action_27 (23) = happyShift action_49
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_23

action_29 _ = happyReduce_24

action_30 (11) = happyShift action_28
action_30 (12) = happyShift action_29
action_30 (16) = happyShift action_30
action_30 (18) = happyShift action_31
action_30 (19) = happyShift action_32
action_30 (21) = happyShift action_33
action_30 (9) = happyGoto action_43
action_30 (10) = happyGoto action_44
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_16

action_32 _ = happyReduce_17

action_33 (11) = happyShift action_28
action_33 (12) = happyShift action_29
action_33 (16) = happyShift action_30
action_33 (18) = happyShift action_31
action_33 (19) = happyShift action_32
action_33 (21) = happyShift action_33
action_33 (9) = happyGoto action_42
action_33 (10) = happyGoto action_27
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (20) = happyShift action_40
action_34 (28) = happyShift action_41
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (11) = happyShift action_28
action_35 (12) = happyShift action_29
action_35 (16) = happyShift action_39
action_35 (10) = happyGoto action_38
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (12) = happyShift action_12
action_36 (25) = happyShift action_13
action_36 (27) = happyShift action_14
action_36 (30) = happyShift action_15
action_36 (32) = happyShift action_16
action_36 (34) = happyShift action_17
action_36 (36) = happyShift action_18
action_36 (37) = happyShift action_19
action_36 (7) = happyGoto action_37
action_36 (8) = happyGoto action_11
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_7

action_38 (13) = happyShift action_45
action_38 (14) = happyShift action_46
action_38 (15) = happyShift action_47
action_38 _ = happyReduce_8

action_39 (11) = happyShift action_28
action_39 (12) = happyShift action_29
action_39 (16) = happyShift action_39
action_39 (10) = happyGoto action_66
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (11) = happyShift action_28
action_40 (12) = happyShift action_29
action_40 (16) = happyShift action_30
action_40 (18) = happyShift action_31
action_40 (19) = happyShift action_32
action_40 (21) = happyShift action_33
action_40 (9) = happyGoto action_65
action_40 (10) = happyGoto action_27
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (12) = happyShift action_12
action_41 (25) = happyShift action_13
action_41 (27) = happyShift action_14
action_41 (30) = happyShift action_15
action_41 (32) = happyShift action_16
action_41 (34) = happyShift action_17
action_41 (36) = happyShift action_18
action_41 (37) = happyShift action_19
action_41 (8) = happyGoto action_64
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_20

action_43 (17) = happyShift action_63
action_43 (20) = happyShift action_40
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (13) = happyShift action_45
action_44 (14) = happyShift action_46
action_44 (15) = happyShift action_47
action_44 (17) = happyShift action_62
action_44 (22) = happyShift action_48
action_44 (23) = happyShift action_49
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (11) = happyShift action_28
action_45 (12) = happyShift action_29
action_45 (16) = happyShift action_39
action_45 (10) = happyGoto action_61
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (11) = happyShift action_28
action_46 (12) = happyShift action_29
action_46 (16) = happyShift action_39
action_46 (10) = happyGoto action_60
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (11) = happyShift action_28
action_47 (12) = happyShift action_29
action_47 (16) = happyShift action_39
action_47 (10) = happyGoto action_59
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (11) = happyShift action_28
action_48 (12) = happyShift action_29
action_48 (16) = happyShift action_39
action_48 (10) = happyGoto action_58
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (11) = happyShift action_28
action_49 (12) = happyShift action_29
action_49 (16) = happyShift action_39
action_49 (10) = happyGoto action_57
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (12) = happyShift action_12
action_50 (25) = happyShift action_13
action_50 (27) = happyShift action_14
action_50 (30) = happyShift action_15
action_50 (32) = happyShift action_16
action_50 (34) = happyShift action_17
action_50 (36) = happyShift action_18
action_50 (37) = happyShift action_19
action_50 (8) = happyGoto action_56
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (11) = happyShift action_28
action_51 (12) = happyShift action_29
action_51 (16) = happyShift action_30
action_51 (18) = happyShift action_31
action_51 (19) = happyShift action_32
action_51 (21) = happyShift action_33
action_51 (9) = happyGoto action_55
action_51 (10) = happyGoto action_27
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (11) = happyShift action_28
action_52 (12) = happyShift action_29
action_52 (16) = happyShift action_39
action_52 (10) = happyGoto action_54
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_15

action_54 (13) = happyShift action_45
action_54 (14) = happyShift action_46
action_54 (15) = happyShift action_47
action_54 (35) = happyShift action_68
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (20) = happyShift action_40
action_55 _ = happyReduce_13

action_56 _ = happyReduce_12

action_57 (13) = happyShift action_45
action_57 (14) = happyShift action_46
action_57 (15) = happyShift action_47
action_57 _ = happyReduce_19

action_58 (13) = happyShift action_45
action_58 (14) = happyShift action_46
action_58 (15) = happyShift action_47
action_58 _ = happyReduce_18

action_59 _ = happyReduce_27

action_60 (15) = happyShift action_47
action_60 _ = happyReduce_26

action_61 (15) = happyShift action_47
action_61 _ = happyReduce_25

action_62 _ = happyReduce_28

action_63 _ = happyReduce_22

action_64 (29) = happyShift action_67
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_21

action_66 (13) = happyShift action_45
action_66 (14) = happyShift action_46
action_66 (15) = happyShift action_47
action_66 (17) = happyShift action_62
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (12) = happyShift action_12
action_67 (25) = happyShift action_13
action_67 (27) = happyShift action_14
action_67 (30) = happyShift action_15
action_67 (32) = happyShift action_16
action_67 (34) = happyShift action_17
action_67 (36) = happyShift action_18
action_67 (37) = happyShift action_19
action_67 (8) = happyGoto action_70
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (11) = happyShift action_28
action_68 (12) = happyShift action_29
action_68 (16) = happyShift action_39
action_68 (10) = happyGoto action_69
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (13) = happyShift action_45
action_69 (14) = happyShift action_46
action_69 (15) = happyShift action_47
action_69 (31) = happyShift action_71
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_11

action_71 (12) = happyShift action_12
action_71 (25) = happyShift action_13
action_71 (27) = happyShift action_14
action_71 (30) = happyShift action_15
action_71 (32) = happyShift action_16
action_71 (34) = happyShift action_17
action_71 (36) = happyShift action_18
action_71 (37) = happyShift action_19
action_71 (8) = happyGoto action_72
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_14

happyReduce_1 = happyReduce 5 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	(HappyTerminal (IDENTIFIER happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ((happy_var_2, reverse happy_var_3, happy_var_5)
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 ([]
	)

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyTerminal (IDENTIFIER happy_var_1))
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyTerminal (IDENTIFIER happy_var_3))
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_3:happy_var_1
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Comp happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (IDENTIFIER happy_var_1))
	 =  HappyAbsSyn7
		 (Ass happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn7
		 (Skip
	)

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn7
		 (Abort
	)

happyReduce_11 = happyReduce 6 8 happyReduction_11
happyReduction_11 ((HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 4 8 happyReduction_12
happyReduction_12 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 4 8 happyReduction_13
happyReduction_13 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Repeat happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 8 8 happyReduction_14
happyReduction_14 ((HappyAbsSyn7  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (IDENTIFIER happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (For happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_3  8 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  9 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn9
		 (TRUE
	)

happyReduce_17 = happySpecReduce_1  9 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn9
		 (FALSE
	)

happyReduce_18 = happySpecReduce_3  9 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Equ happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  9 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Leq happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  9 happyReduction_20
happyReduction_20 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Neg happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  9 happyReduction_21
happyReduction_21 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (And happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  9 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  10 happyReduction_23
happyReduction_23 (HappyTerminal (LITERAL_INT happy_var_1))
	 =  HappyAbsSyn10
		 (N happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  10 happyReduction_24
happyReduction_24 (HappyTerminal (IDENTIFIER happy_var_1))
	 =  HappyAbsSyn10
		 (V happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  10 happyReduction_25
happyReduction_25 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Add happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  10 happyReduction_26
happyReduction_26 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  10 happyReduction_27
happyReduction_27 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Mult happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  10 happyReduction_28
happyReduction_28 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 41 41 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	LITERAL_INT happy_dollar_dollar -> cont 11;
	IDENTIFIER happy_dollar_dollar -> cont 12;
	PLUS -> cont 13;
	MINUS -> cont 14;
	ASTERISK -> cont 15;
	LPAREN -> cont 16;
	RPAREN -> cont 17;
	LITERAL_TRUE -> cont 18;
	LITERAL_FALSE -> cont 19;
	AMPERSANDS -> cont 20;
	EXCLAMATION -> cont 21;
	EQUALS -> cont 22;
	LESSEQUALS -> cont 23;
	ASSIGN -> cont 24;
	SKIP -> cont 25;
	SEMICOLON -> cont 26;
	IF -> cont 27;
	THEN -> cont 28;
	ELSE -> cont 29;
	WHILE -> cont 30;
	DO -> cont 31;
	REPEAT -> cont 32;
	UNTIL -> cont 33;
	FOR -> cont 34;
	TO -> cont 35;
	ABORT -> cont 36;
	BEGIN -> cont 37;
	END -> cont 38;
	PROGRAM -> cont 39;
	COMMA -> cont 40;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 41 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> happyError tokens)
happyParseWhile tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [Token] -> a
happyError _ = error ("Parse error\n")

parser :: FilePath -> IO((String, [Var], Stm))
parser filename = do
  s <- readFile filename
  return $ (happyParseWhile . alexScanTokens) s
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
