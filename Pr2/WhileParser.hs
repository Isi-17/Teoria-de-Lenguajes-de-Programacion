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
 action_71 :: () => Prelude.Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_27 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,136) ([0,0,32,0,8192,2048,0,0,0,0,32768,0,0,0,2,2048,0,0,256,16384,0,0,0,8,2725,0,0,0,0,2,0,128,0,0,0,35840,22,0,5772,0,2048,42240,10,8,0,2048,42240,10,0,0,2048,0,0,0,0,0,0,16,32768,0,0,0,1,2048,64,28672,96,0,0,0,0,0,0,5772,0,0,0,0,0,0,35840,22,0,2048,8,35840,0,0,8,2725,0,0,0,112,0,35840,0,0,5772,0,2048,42240,10,0,0,0,9,0,24944,0,35840,0,0,140,0,35840,0,0,140,0,35840,0,0,8,2725,35840,22,0,140,0,0,0,0,112,1024,0,8,0,0,0,28672,0,0,112,0,0,0,0,64,0,16384,0,0,0,0,0,0,0,0,16,0,0,0,368,0,2048,42240,10,140,0,28672,16384,0,0,0,2048,42240,10,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_happyParseWhile","Program","Outputs","Var_List","Stm_List","Stm","Bexp","Aexp","NUM","ID","'+'","'-'","'*'","'('","')'","'true'","'false'","'&&'","'!'","'='","'<='","':='","SKIP","';'","'if'","'then'","'else'","'while'","'do'","'repeat'","'until'","'for'","'to'","'begin'","'end'","'program'","','","%eof"]
        bit_start = st Prelude.* 40
        bit_end = (st Prelude.+ 1) Prelude.* 40
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..39]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (38) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (38) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (12) = happyShift action_4
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (40) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (16) = happyShift action_6
action_4 (5) = happyGoto action_5
action_4 _ = happyReduce_3

action_5 (26) = happyShift action_9
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (12) = happyShift action_8
action_6 (6) = happyGoto action_7
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (17) = happyShift action_19
action_7 (39) = happyShift action_20
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_4

action_9 (12) = happyShift action_12
action_9 (25) = happyShift action_13
action_9 (27) = happyShift action_14
action_9 (30) = happyShift action_15
action_9 (32) = happyShift action_16
action_9 (34) = happyShift action_17
action_9 (36) = happyShift action_18
action_9 (7) = happyGoto action_10
action_9 (8) = happyGoto action_11
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_1

action_11 (26) = happyShift action_35
action_11 _ = happyReduce_6

action_12 (24) = happyShift action_34
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_9

action_14 (11) = happyShift action_27
action_14 (12) = happyShift action_28
action_14 (16) = happyShift action_29
action_14 (18) = happyShift action_30
action_14 (19) = happyShift action_31
action_14 (21) = happyShift action_32
action_14 (9) = happyGoto action_33
action_14 (10) = happyGoto action_26
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (11) = happyShift action_27
action_15 (12) = happyShift action_28
action_15 (16) = happyShift action_29
action_15 (18) = happyShift action_30
action_15 (19) = happyShift action_31
action_15 (21) = happyShift action_32
action_15 (9) = happyGoto action_25
action_15 (10) = happyGoto action_26
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (12) = happyShift action_12
action_16 (25) = happyShift action_13
action_16 (27) = happyShift action_14
action_16 (30) = happyShift action_15
action_16 (32) = happyShift action_16
action_16 (34) = happyShift action_17
action_16 (36) = happyShift action_18
action_16 (7) = happyGoto action_24
action_16 (8) = happyGoto action_11
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (12) = happyShift action_23
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (12) = happyShift action_12
action_18 (25) = happyShift action_13
action_18 (27) = happyShift action_14
action_18 (30) = happyShift action_15
action_18 (32) = happyShift action_16
action_18 (34) = happyShift action_17
action_18 (36) = happyShift action_18
action_18 (7) = happyGoto action_22
action_18 (8) = happyGoto action_11
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_2

action_20 (12) = happyShift action_21
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_5

action_22 (37) = happyShift action_52
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (24) = happyShift action_51
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (33) = happyShift action_50
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (20) = happyShift action_39
action_25 (31) = happyShift action_49
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (13) = happyShift action_44
action_26 (14) = happyShift action_45
action_26 (15) = happyShift action_46
action_26 (22) = happyShift action_47
action_26 (23) = happyShift action_48
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_22

action_28 _ = happyReduce_23

action_29 (11) = happyShift action_27
action_29 (12) = happyShift action_28
action_29 (16) = happyShift action_29
action_29 (18) = happyShift action_30
action_29 (19) = happyShift action_31
action_29 (21) = happyShift action_32
action_29 (9) = happyGoto action_42
action_29 (10) = happyGoto action_43
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_15

action_31 _ = happyReduce_16

action_32 (11) = happyShift action_27
action_32 (12) = happyShift action_28
action_32 (16) = happyShift action_29
action_32 (18) = happyShift action_30
action_32 (19) = happyShift action_31
action_32 (21) = happyShift action_32
action_32 (9) = happyGoto action_41
action_32 (10) = happyGoto action_26
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (20) = happyShift action_39
action_33 (28) = happyShift action_40
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (11) = happyShift action_27
action_34 (12) = happyShift action_28
action_34 (16) = happyShift action_38
action_34 (10) = happyGoto action_37
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (12) = happyShift action_12
action_35 (25) = happyShift action_13
action_35 (27) = happyShift action_14
action_35 (30) = happyShift action_15
action_35 (32) = happyShift action_16
action_35 (34) = happyShift action_17
action_35 (36) = happyShift action_18
action_35 (7) = happyGoto action_36
action_35 (8) = happyGoto action_11
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_7

action_37 (13) = happyShift action_44
action_37 (14) = happyShift action_45
action_37 (15) = happyShift action_46
action_37 _ = happyReduce_8

action_38 (11) = happyShift action_27
action_38 (12) = happyShift action_28
action_38 (16) = happyShift action_38
action_38 (10) = happyGoto action_65
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (11) = happyShift action_27
action_39 (12) = happyShift action_28
action_39 (16) = happyShift action_29
action_39 (18) = happyShift action_30
action_39 (19) = happyShift action_31
action_39 (21) = happyShift action_32
action_39 (9) = happyGoto action_64
action_39 (10) = happyGoto action_26
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (12) = happyShift action_12
action_40 (25) = happyShift action_13
action_40 (27) = happyShift action_14
action_40 (30) = happyShift action_15
action_40 (32) = happyShift action_16
action_40 (34) = happyShift action_17
action_40 (36) = happyShift action_18
action_40 (8) = happyGoto action_63
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_19

action_42 (17) = happyShift action_62
action_42 (20) = happyShift action_39
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (13) = happyShift action_44
action_43 (14) = happyShift action_45
action_43 (15) = happyShift action_46
action_43 (17) = happyShift action_61
action_43 (22) = happyShift action_47
action_43 (23) = happyShift action_48
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (11) = happyShift action_27
action_44 (12) = happyShift action_28
action_44 (16) = happyShift action_38
action_44 (10) = happyGoto action_60
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (11) = happyShift action_27
action_45 (12) = happyShift action_28
action_45 (16) = happyShift action_38
action_45 (10) = happyGoto action_59
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (11) = happyShift action_27
action_46 (12) = happyShift action_28
action_46 (16) = happyShift action_38
action_46 (10) = happyGoto action_58
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (11) = happyShift action_27
action_47 (12) = happyShift action_28
action_47 (16) = happyShift action_38
action_47 (10) = happyGoto action_57
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (11) = happyShift action_27
action_48 (12) = happyShift action_28
action_48 (16) = happyShift action_38
action_48 (10) = happyGoto action_56
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (12) = happyShift action_12
action_49 (25) = happyShift action_13
action_49 (27) = happyShift action_14
action_49 (30) = happyShift action_15
action_49 (32) = happyShift action_16
action_49 (34) = happyShift action_17
action_49 (36) = happyShift action_18
action_49 (8) = happyGoto action_55
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (11) = happyShift action_27
action_50 (12) = happyShift action_28
action_50 (16) = happyShift action_29
action_50 (18) = happyShift action_30
action_50 (19) = happyShift action_31
action_50 (21) = happyShift action_32
action_50 (9) = happyGoto action_54
action_50 (10) = happyGoto action_26
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (11) = happyShift action_27
action_51 (12) = happyShift action_28
action_51 (16) = happyShift action_38
action_51 (10) = happyGoto action_53
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_14

action_53 (13) = happyShift action_44
action_53 (14) = happyShift action_45
action_53 (15) = happyShift action_46
action_53 (35) = happyShift action_67
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (20) = happyShift action_39
action_54 _ = happyReduce_12

action_55 _ = happyReduce_11

action_56 (13) = happyShift action_44
action_56 (14) = happyShift action_45
action_56 (15) = happyShift action_46
action_56 _ = happyReduce_18

action_57 (13) = happyShift action_44
action_57 (14) = happyShift action_45
action_57 (15) = happyShift action_46
action_57 _ = happyReduce_17

action_58 _ = happyReduce_26

action_59 (15) = happyShift action_46
action_59 _ = happyReduce_25

action_60 (15) = happyShift action_46
action_60 _ = happyReduce_24

action_61 _ = happyReduce_27

action_62 _ = happyReduce_21

action_63 (29) = happyShift action_66
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_20

action_65 (13) = happyShift action_44
action_65 (14) = happyShift action_45
action_65 (15) = happyShift action_46
action_65 (17) = happyShift action_61
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (12) = happyShift action_12
action_66 (25) = happyShift action_13
action_66 (27) = happyShift action_14
action_66 (30) = happyShift action_15
action_66 (32) = happyShift action_16
action_66 (34) = happyShift action_17
action_66 (36) = happyShift action_18
action_66 (8) = happyGoto action_69
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (11) = happyShift action_27
action_67 (12) = happyShift action_28
action_67 (16) = happyShift action_38
action_67 (10) = happyGoto action_68
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (13) = happyShift action_44
action_68 (14) = happyShift action_45
action_68 (15) = happyShift action_46
action_68 (31) = happyShift action_70
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_10

action_70 (12) = happyShift action_12
action_70 (25) = happyShift action_13
action_70 (27) = happyShift action_14
action_70 (30) = happyShift action_15
action_70 (32) = happyShift action_16
action_70 (34) = happyShift action_17
action_70 (36) = happyShift action_18
action_70 (8) = happyGoto action_71
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_13

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

happyReduce_10 = happyReduce 6 8 happyReduction_10
happyReduction_10 ((HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 4 8 happyReduction_11
happyReduction_11 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 4 8 happyReduction_12
happyReduction_12 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Repeat happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 8 8 happyReduction_13
happyReduction_13 ((HappyAbsSyn7  happy_var_8) `HappyStk`
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

happyReduce_14 = happySpecReduce_3  8 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  9 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn9
		 (TRUE
	)

happyReduce_16 = happySpecReduce_1  9 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn9
		 (FALSE
	)

happyReduce_17 = happySpecReduce_3  9 happyReduction_17
happyReduction_17 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Equ happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  9 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Leq happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  9 happyReduction_19
happyReduction_19 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Neg happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  9 happyReduction_20
happyReduction_20 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (And happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  9 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  10 happyReduction_22
happyReduction_22 (HappyTerminal (LITERAL_INT happy_var_1))
	 =  HappyAbsSyn10
		 (N happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  10 happyReduction_23
happyReduction_23 (HappyTerminal (IDENTIFIER happy_var_1))
	 =  HappyAbsSyn10
		 (V happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  10 happyReduction_24
happyReduction_24 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Add happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  10 happyReduction_25
happyReduction_25 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  10 happyReduction_26
happyReduction_26 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Mult happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  10 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 40 40 notHappyAtAll (HappyState action) sts stk []

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
	BEGIN -> cont 36;
	END -> cont 37;
	PROGRAM -> cont 38;
	COMMA -> cont 39;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 40 tk tks = happyError' (tks, explist)
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
