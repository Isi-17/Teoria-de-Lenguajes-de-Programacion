{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
module While21Parser ( happyParseWhile
                     , parser
                     ) where

import While21Lexer
import While21
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap4 = HappyWrap4 ((String, [Var], Stm))
happyIn4 :: ((String, [Var], Stm)) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap4 x)
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> HappyWrap4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
newtype HappyWrap5 = HappyWrap5 ([Var])
happyIn5 :: ([Var]) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
newtype HappyWrap6 = HappyWrap6 ([Var])
happyIn6 :: ([Var]) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 (Stm)
happyIn7 :: (Stm) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
newtype HappyWrap8 = HappyWrap8 (Stm)
happyIn8 :: (Stm) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 (LabelledStms)
happyIn9 :: (LabelledStms) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 ([Integer])
happyIn10 :: ([Integer]) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 (Bexp)
happyIn11 :: (Bexp) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 (Aexp)
happyIn12 :: (Aexp) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x04\x00\x00\x00\x00\x08\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x02\x00\x00\x08\x00\x00\x00\x00\x00\x04\x00\x04\x00\x00\x00\x00\x00\x00\x40\x00\x50\x22\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x2d\x00\x00\x00\x10\x00\x94\x08\x00\x30\x04\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x02\x00\x00\x00\x00\x00\x00\x02\x00\x00\x80\x80\x00\x00\x00\x0f\x0c\x00\x00\x80\xa1\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x2d\x00\x00\x00\x18\x02\x00\x00\x00\x20\x00\x28\x11\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x04\x00\x00\x00\x78\x61\x00\x00\x00\x0c\x01\x00\x00\x00\x18\x02\x00\x00\x00\x30\x04\x00\x00\x00\x60\x08\x00\x00\x00\xc0\x10\x00\x00\x00\x80\x21\x00\x00\x00\x00\x43\x0b\x00\x00\x00\x04\x00\x25\x02\x00\x00\x00\x00\x00\x00\xe0\x05\x00\x00\x00\x10\x00\x00\x80\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x01\x00\x00\x00\x40\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\xc0\x03\x00\x00\x00\x80\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x94\x08\x00\x20\x00\x28\x11\x00\x20\x00\x00\x00\x00\x80\x00\xa0\x44\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_happyParseWhile","Program","Outputs","Var_List","Stm_List","Stm","Labelled_Stms","Num_List","Bexp","Aexp","NUM","ID","'+'","'-'","'*'","'/'","'('","')'","'true'","'false'","'&&'","'!'","'='","'<='","':='","'skip'","';'","'if'","'then'","'else'","'begin'","'end'","'program'","','","'case'","'of'","':'","'default'","%eof"]
        bit_start = st * 41
        bit_end = (st + 1) * 41
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..40]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xf7\xff\xf7\xff\x10\x00\xfa\xff\x13\x00\x16\x00\x1f\x00\xfb\xff\x00\x00\xff\xff\x00\x00\x22\x00\x21\x00\x00\x00\x28\x00\xff\xff\x46\x00\x00\x00\x40\x00\x00\x00\x02\x00\x00\x00\x00\x00\x46\x00\x2f\x00\xf9\xff\x32\x00\x3a\x00\x00\x00\x00\x00\x3a\x00\x46\x00\x09\x00\x00\x00\x51\x00\x00\x00\x05\x00\x20\x00\x46\x00\x46\x00\x46\x00\x46\x00\x46\x00\x46\x00\x3a\x00\x09\x00\x00\x00\x4b\x00\x01\x00\x3c\x00\x3d\x00\x45\x00\x43\x00\x00\x00\x4c\x00\x00\x00\x51\x00\x51\x00\x00\x00\x00\x00\x04\x00\x04\x00\x00\x00\x09\x00\x09\x00\x5e\x00\x09\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x61\x00\x00\x00\x00\x00\x00\x00\x62\x00\x00\x00\x60\x00\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x36\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5d\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x42\x00\x5f\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x52\x00\x6d\x00\x00\x00\x00\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\x00\x00\x00\x00\x00\x00\xfb\xff\x00\x00\xfe\xff\xf9\xff\x00\x00\xf6\xff\x00\x00\x00\x00\x00\x00\xfd\xff\x00\x00\xfa\xff\x00\x00\xe6\xff\xe5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xed\xff\xec\xff\x00\x00\x00\x00\x00\x00\xf8\xff\xf7\xff\xe9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf4\xff\x00\x00\xf2\xff\x00\x00\x00\x00\xef\xff\x00\x00\xe0\xff\x00\x00\xe8\xff\xea\xff\xeb\xff\xe1\xff\xe2\xff\xe3\xff\xe4\xff\xe7\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf3\xff\xf2\xff\xee\xff\xf1\xff\xf5\xff\xf0\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x01\x00\x08\x00\x0b\x00\x03\x00\x04\x00\x05\x00\x06\x00\x05\x00\x06\x00\x02\x00\x13\x00\x08\x00\x17\x00\x10\x00\x0b\x00\x12\x00\x02\x00\x18\x00\x15\x00\x03\x00\x04\x00\x1d\x00\x19\x00\x10\x00\x07\x00\x12\x00\x1a\x00\x1c\x00\x15\x00\x07\x00\x08\x00\x02\x00\x19\x00\x03\x00\x04\x00\x05\x00\x06\x00\x11\x00\x08\x00\x01\x00\x02\x00\x07\x00\x08\x00\x0d\x00\x0e\x00\x07\x00\x0f\x00\x09\x00\x0a\x00\x11\x00\x0c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x03\x00\x04\x00\x01\x00\x02\x00\x05\x00\x06\x00\x0d\x00\x0e\x00\x07\x00\x02\x00\x09\x00\x0a\x00\x16\x00\x0c\x00\x01\x00\x02\x00\x07\x00\x08\x00\x03\x00\x04\x00\x07\x00\x03\x00\x04\x00\x05\x00\x06\x00\x16\x00\x08\x00\x03\x00\x04\x00\x05\x00\x06\x00\x1b\x00\x07\x00\x08\x00\x05\x00\x06\x00\x18\x00\x1b\x00\x01\x00\x14\x00\x00\x00\x02\x00\x01\x00\x08\x00\x08\x00\xff\xff\x08\x00\xff\xff\xff\xff\xff\xff\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x04\x00\x04\x00\x04\x00\xff\xff\x04\x00\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x0d\x00\x34\x00\x12\x00\x2d\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x29\x00\x2a\x00\x0d\x00\x2e\x00\x3f\x00\x03\x00\x0e\x00\x2d\x00\x0f\x00\x05\x00\x13\x00\x10\x00\x0a\x00\x0b\x00\xff\xff\x11\x00\x0e\x00\x07\x00\x0f\x00\x31\x00\x35\x00\x10\x00\x19\x00\x1a\x00\x09\x00\x11\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x0a\x00\x36\x00\x16\x00\x17\x00\x24\x00\x25\x00\x2b\x00\x2c\x00\x1c\x00\x20\x00\x1d\x00\x1e\x00\x21\x00\x1f\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x18\x00\x0b\x00\x16\x00\x17\x00\x31\x00\x32\x00\x2b\x00\x2c\x00\x1c\x00\x14\x00\x1d\x00\x1e\x00\x2f\x00\x1f\x00\x16\x00\x17\x00\x23\x00\x1a\x00\x21\x00\x0b\x00\x18\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x44\x00\x36\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x43\x00\x37\x00\x1a\x00\x48\x00\x32\x00\x42\x00\x41\x00\x34\x00\x40\x00\x03\x00\x07\x00\x05\x00\x14\x00\x2f\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x3c\x00\x3b\x00\x3a\x00\x39\x00\x38\x00\x36\x00\x47\x00\x46\x00\x00\x00\x44\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 31) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31)
	]

happy_n_terms = 30 :: Int
happy_n_nonterms = 9 :: Int

happyReduce_1 = happyReduce 5# 0# happyReduction_1
happyReduction_1 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (IDENTIFIER happy_var_2) -> 
	case happyOut5 happy_x_3 of { (HappyWrap5 happy_var_3) -> 
	case happyOut7 happy_x_5 of { (HappyWrap7 happy_var_5) -> 
	happyIn4
		 ((happy_var_2, reverse happy_var_3, happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_2 = happySpecReduce_3  1# happyReduction_2
happyReduction_2 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_2 of { (HappyWrap6 happy_var_2) -> 
	happyIn5
		 (happy_var_2
	)}

happyReduce_3 = happySpecReduce_0  1# happyReduction_3
happyReduction_3  =  happyIn5
		 ([]
	)

happyReduce_4 = happySpecReduce_1  2# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { (IDENTIFIER happy_var_1) -> 
	happyIn6
		 ([happy_var_1]
	)}

happyReduce_5 = happySpecReduce_3  2# happyReduction_5
happyReduction_5 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOutTok happy_x_3 of { (IDENTIFIER happy_var_3) -> 
	happyIn6
		 (happy_var_3:happy_var_1
	)}}

happyReduce_6 = happySpecReduce_1  3# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	happyIn7
		 (happy_var_1
	)}

happyReduce_7 = happySpecReduce_3  3# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	case happyOut7 happy_x_3 of { (HappyWrap7 happy_var_3) -> 
	happyIn7
		 (Comp happy_var_1 happy_var_3
	)}}

happyReduce_8 = happySpecReduce_3  4# happyReduction_8
happyReduction_8 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (IDENTIFIER happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn8
		 (Ass happy_var_1 happy_var_3
	)}}

happyReduce_9 = happySpecReduce_1  4# happyReduction_9
happyReduction_9 happy_x_1
	 =  happyIn8
		 (Skip
	)

happyReduce_10 = happyReduce 6# 4# happyReduction_10
happyReduction_10 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut11 happy_x_2 of { (HappyWrap11 happy_var_2) -> 
	case happyOut8 happy_x_4 of { (HappyWrap8 happy_var_4) -> 
	case happyOut8 happy_x_6 of { (HappyWrap8 happy_var_6) -> 
	happyIn8
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_11 = happySpecReduce_3  4# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_2 of { (HappyWrap7 happy_var_2) -> 
	happyIn8
		 (happy_var_2
	)}

happyReduce_12 = happyReduce 5# 4# happyReduction_12
happyReduction_12 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	case happyOut9 happy_x_4 of { (HappyWrap9 happy_var_4) -> 
	happyIn8
		 (Case happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_13 = happySpecReduce_0  5# happyReduction_13
happyReduction_13  =  happyIn9
		 (EndLabelledStms
	)

happyReduce_14 = happySpecReduce_3  5# happyReduction_14
happyReduction_14 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_3 of { (HappyWrap8 happy_var_3) -> 
	happyIn9
		 (Default happy_var_3
	)}

happyReduce_15 = happyReduce 4# 5# happyReduction_15
happyReduction_15 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
	case happyOut8 happy_x_3 of { (HappyWrap8 happy_var_3) -> 
	case happyOut9 happy_x_4 of { (HappyWrap9 happy_var_4) -> 
	happyIn9
		 (LabelledStm happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_16 = happySpecReduce_1  6# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOutTok happy_x_1 of { (LITERAL_INT happy_var_1) -> 
	happyIn10
		 ([happy_var_1]
	)}

happyReduce_17 = happySpecReduce_3  6# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (LITERAL_INT happy_var_1) -> 
	case happyOut10 happy_x_3 of { (HappyWrap10 happy_var_3) -> 
	happyIn10
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_18 = happySpecReduce_1  7# happyReduction_18
happyReduction_18 happy_x_1
	 =  happyIn11
		 (TRUE
	)

happyReduce_19 = happySpecReduce_1  7# happyReduction_19
happyReduction_19 happy_x_1
	 =  happyIn11
		 (FALSE
	)

happyReduce_20 = happySpecReduce_3  7# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn11
		 (Eq happy_var_1 happy_var_3
	)}}

happyReduce_21 = happySpecReduce_3  7# happyReduction_21
happyReduction_21 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn11
		 (Le happy_var_1 happy_var_3
	)}}

happyReduce_22 = happySpecReduce_2  7# happyReduction_22
happyReduction_22 happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_2 of { (HappyWrap11 happy_var_2) -> 
	happyIn11
		 (Neg happy_var_2
	)}

happyReduce_23 = happySpecReduce_3  7# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
	happyIn11
		 (And happy_var_1 happy_var_3
	)}}

happyReduce_24 = happySpecReduce_3  7# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_2 of { (HappyWrap11 happy_var_2) -> 
	happyIn11
		 (happy_var_2
	)}

happyReduce_25 = happySpecReduce_1  8# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOutTok happy_x_1 of { (LITERAL_INT happy_var_1) -> 
	happyIn12
		 (N happy_var_1
	)}

happyReduce_26 = happySpecReduce_1  8# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOutTok happy_x_1 of { (IDENTIFIER happy_var_1) -> 
	happyIn12
		 (V happy_var_1
	)}

happyReduce_27 = happySpecReduce_3  8# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn12
		 (Add happy_var_1 happy_var_3
	)}}

happyReduce_28 = happySpecReduce_3  8# happyReduction_28
happyReduction_28 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn12
		 (Sub happy_var_1 happy_var_3
	)}}

happyReduce_29 = happySpecReduce_3  8# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn12
		 (Mult happy_var_1 happy_var_3
	)}}

happyReduce_30 = happySpecReduce_3  8# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn12
		 (Div happy_var_1 happy_var_3
	)}}

happyReduce_31 = happySpecReduce_3  8# happyReduction_31
happyReduction_31 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	happyIn12
		 (happy_var_2
	)}

happyNewToken action sts stk [] =
	happyDoAction 29# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	LITERAL_INT happy_dollar_dollar -> cont 1#;
	IDENTIFIER happy_dollar_dollar -> cont 2#;
	PLUS -> cont 3#;
	MINUS -> cont 4#;
	ASTERISK -> cont 5#;
	SLASH -> cont 6#;
	LPAREN -> cont 7#;
	RPAREN -> cont 8#;
	LITERAL_TRUE -> cont 9#;
	LITERAL_FALSE -> cont 10#;
	AMPERSANDS -> cont 11#;
	EXCLAMATION -> cont 12#;
	EQUALS -> cont 13#;
	LESSEQUALS -> cont 14#;
	ASSIGN -> cont 15#;
	SKIP -> cont 16#;
	SEMICOLON -> cont 17#;
	IF -> cont 18#;
	THEN -> cont 19#;
	ELSE -> cont 20#;
	BEGIN -> cont 21#;
	END -> cont 22#;
	PROGRAM -> cont 23#;
	COMMA -> cont 24#;
	CASE -> cont 25#;
	OF -> cont 26#;
	COLON -> cont 27#;
	DEFAULT -> cont 28#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 29# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> happyError tokens)
happyParseWhile tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap4 x') = happyOut4 x} in x'))

happySeq = happyDontSeq


happyError :: [Token] -> a
happyError _ = error ("Parse error\n")

parser :: FilePath -> IO((String, [Var], Stm))
parser filename = do
  s <- readFile filename
  return $ (happyParseWhile . alexScanTokens) s
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































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
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
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
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

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
