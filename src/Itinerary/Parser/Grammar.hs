{-# OPTIONS_GHC -w #-}
module Itinerary.Parser.Grammar where
import Itinerary.Parser.Tokens
import Itinerary.Parser.UnexItinerary
import Commons.ISUnits
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28
	= HappyTerminal (T)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,258) ([0,0,10240,0,0,0,0,160,0,0,0,0,0,0,0,0,2560,0,0,0,0,0,0,0,0,1,0,0,0,8192,0,0,0,0,0,0,0,0,0,16,0,0,0,0,8256,0,0,0,0,0,0,0,32768,0,0,0,0,4096,0,0,0,0,2048,0,0,0,0,32,0,0,0,128,0,0,0,0,2,0,0,0,256,257,0,0,0,4608,0,0,0,0,0,0,0,0,1024,0,0,0,0,4096,14338,0,0,0,0,0,0,0,4112,0,0,0,16384,16448,0,0,0,0,0,0,0,0,0,0,0,0,4096,4112,0,0,0,8192,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1025,224,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,432,0,0,0,49152,6,0,0,0,6912,0,0,0,0,0,32224,0,0,0,0,0,0,0,256,257,0,0,0,0,0,0,0,0,0,0,0,0,2048,1,0,0,0,8,0,0,0,8192,0,0,0,0,128,0,0,0,0,2,0,0,0,2048,0,0,0,0,32,0,0,0,32768,0,0,0,0,512,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1728,0,0,0,0,27,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,14336,0,0,0,4160,1792,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,27648,0,0,0,0,432,0,0,0,49152,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,56,0,0,256,904,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,45056,1,0,0,0,1728,0,0,0,0,27,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,31,0,0,1,31744,0,0,1024,3584,0,0,0,16,56,0,0,16384,0,56,0,0,256,7168,0,0,0,4,3072,0,0,4096,49152,1,0,0,64,224,0,0,0,0,0,0,0,32768,503,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6912,0,0,0,0,108,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6912,0,0,0,0,108,0,0,0,45056,1,0,0,0,1728,0,0,0,0,27,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,28,0,0,1024,2,12,0,0,8,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,4096,14336,0,0,0,64,228,0,0,32768,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,192,0,0,256,7169,0,0,0,2,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,1024,3584,0,0,0,16,49156,7,0,8192,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,64,1792,0,0,0,512,0,0,0,512,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,4,61440,1,0,4096,512,1984,0,0,40,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,10240,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,64,0,31,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseCalc","prods","gItinerary","gRoadUnitSequence","gConstCollection","gConst","gAmbientConditions","gRoadUnit","gHeadLessRoadUnit","gRemap","gRemapCollection","gSpeedTerm","gLengthTerm","gSlopeAngleTerm","gCornerRadiusTerm","gTempTerm","gHeadWindTerm","gTailWindTerm","gVapourPressureTerm","gDryAirPressureTerm","gSpeedUnitValue","gLengthUnitValue","gAngleUnitValue","gTempUnitValue","gPressureUnitValue","gDouble","int","double","var","'+'","'-'","':'","'('","')'","'['","']'","'{'","'}'","','","';'","zero","Const","RoadUnit","Itinerary","Halt","Speed","Length","Slope","CornerRadius","Ambient","Temp","HeadWind","TailWind","VapourPressure","DryAirPressure","KPH","MPH","MS","Km","Meter","Miles","Cel","Fah","Kel","Deg","Rad","Pascal","KPascal","PSIA","Bar","Torr","%eof"]
        bit_start = st * 74
        bit_end = (st + 1) * 74
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..73]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (44) = happyShift action_5
action_0 (46) = happyShift action_6
action_0 (4) = happyGoto action_7
action_0 (5) = happyGoto action_2
action_0 (7) = happyGoto action_3
action_0 (8) = happyGoto action_4
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (44) = happyShift action_5
action_1 (46) = happyShift action_6
action_1 (5) = happyGoto action_2
action_1 (7) = happyGoto action_3
action_1 (8) = happyGoto action_4
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (44) = happyShift action_5
action_3 (46) = happyShift action_11
action_3 (8) = happyGoto action_10
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_6

action_5 (31) = happyShift action_9
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (34) = happyShift action_8
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (74) = happyAccept
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (37) = happyShift action_17
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (45) = happyShift action_15
action_9 (52) = happyShift action_16
action_9 (9) = happyGoto action_13
action_9 (10) = happyGoto action_14
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_7

action_11 (34) = happyShift action_12
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (37) = happyShift action_27
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (42) = happyShift action_26
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (42) = happyShift action_25
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (34) = happyShift action_24
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (34) = happyShift action_23
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (31) = happyShift action_20
action_17 (39) = happyShift action_21
action_17 (47) = happyShift action_22
action_17 (6) = happyGoto action_18
action_17 (11) = happyGoto action_19
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (38) = happyShift action_40
action_18 (41) = happyShift action_41
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_4

action_20 (35) = happyShift action_39
action_20 _ = happyReduce_17

action_21 (43) = happyShift action_34
action_21 (48) = happyShift action_35
action_21 (58) = happyShift action_36
action_21 (59) = happyShift action_37
action_21 (60) = happyShift action_38
action_21 (14) = happyGoto action_32
action_21 (23) = happyGoto action_33
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_16

action_23 (31) = happyShift action_30
action_23 (39) = happyShift action_31
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (31) = happyShift action_20
action_24 (39) = happyShift action_21
action_24 (47) = happyShift action_22
action_24 (11) = happyGoto action_29
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_9

action_26 _ = happyReduce_8

action_27 (31) = happyShift action_20
action_27 (39) = happyShift action_21
action_27 (47) = happyShift action_22
action_27 (6) = happyGoto action_28
action_27 (11) = happyGoto action_19
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (38) = happyShift action_70
action_28 (41) = happyShift action_41
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_13

action_30 _ = happyReduce_12

action_31 (43) = happyShift action_65
action_31 (53) = happyShift action_66
action_31 (64) = happyShift action_67
action_31 (65) = happyShift action_68
action_31 (66) = happyShift action_69
action_31 (18) = happyGoto action_63
action_31 (26) = happyGoto action_64
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (42) = happyShift action_62
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_31

action_34 _ = happyReduce_51

action_35 (34) = happyShift action_61
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (29) = happyShift action_55
action_36 (30) = happyShift action_56
action_36 (32) = happyShift action_57
action_36 (33) = happyShift action_58
action_36 (28) = happyGoto action_60
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (29) = happyShift action_55
action_37 (30) = happyShift action_56
action_37 (32) = happyShift action_57
action_37 (33) = happyShift action_58
action_37 (28) = happyGoto action_59
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (29) = happyShift action_55
action_38 (30) = happyShift action_56
action_38 (32) = happyShift action_57
action_38 (33) = happyShift action_58
action_38 (28) = happyGoto action_54
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (48) = happyShift action_45
action_39 (49) = happyShift action_46
action_39 (50) = happyShift action_47
action_39 (51) = happyShift action_48
action_39 (53) = happyShift action_49
action_39 (54) = happyShift action_50
action_39 (55) = happyShift action_51
action_39 (56) = happyShift action_52
action_39 (57) = happyShift action_53
action_39 (12) = happyGoto action_43
action_39 (13) = happyGoto action_44
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_3

action_41 (31) = happyShift action_20
action_41 (39) = happyShift action_21
action_41 (47) = happyShift action_22
action_41 (11) = happyGoto action_42
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_5

action_43 _ = happyReduce_28

action_44 (36) = happyShift action_95
action_44 (41) = happyShift action_96
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (34) = happyShift action_94
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (34) = happyShift action_93
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (34) = happyShift action_92
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (34) = happyShift action_91
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (34) = happyShift action_90
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (34) = happyShift action_89
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (34) = happyShift action_88
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (34) = happyShift action_87
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (34) = happyShift action_86
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_50

action_55 _ = happyReduce_70

action_56 _ = happyReduce_69

action_57 (29) = happyShift action_55
action_57 (30) = happyShift action_56
action_57 (32) = happyShift action_57
action_57 (33) = happyShift action_58
action_57 (28) = happyGoto action_85
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (29) = happyShift action_55
action_58 (30) = happyShift action_56
action_58 (32) = happyShift action_57
action_58 (33) = happyShift action_58
action_58 (28) = happyGoto action_84
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_49

action_60 _ = happyReduce_48

action_61 (43) = happyShift action_34
action_61 (58) = happyShift action_36
action_61 (59) = happyShift action_37
action_61 (60) = happyShift action_38
action_61 (23) = happyGoto action_83
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (43) = happyShift action_78
action_62 (49) = happyShift action_79
action_62 (61) = happyShift action_80
action_62 (62) = happyShift action_81
action_62 (63) = happyShift action_82
action_62 (15) = happyGoto action_76
action_62 (24) = happyGoto action_77
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (42) = happyShift action_75
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_39

action_65 _ = happyReduce_62

action_66 (34) = happyShift action_74
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (29) = happyShift action_55
action_67 (30) = happyShift action_56
action_67 (32) = happyShift action_57
action_67 (33) = happyShift action_58
action_67 (28) = happyGoto action_73
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (29) = happyShift action_55
action_68 (30) = happyShift action_56
action_68 (32) = happyShift action_57
action_68 (33) = happyShift action_58
action_68 (28) = happyGoto action_72
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (29) = happyShift action_55
action_69 (30) = happyShift action_56
action_69 (32) = happyShift action_57
action_69 (33) = happyShift action_58
action_69 (28) = happyGoto action_71
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_2

action_71 _ = happyReduce_61

action_72 _ = happyReduce_60

action_73 _ = happyReduce_59

action_74 (43) = happyShift action_65
action_74 (64) = happyShift action_67
action_74 (65) = happyShift action_68
action_74 (66) = happyShift action_69
action_74 (26) = happyGoto action_124
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (43) = happyShift action_34
action_75 (54) = happyShift action_123
action_75 (58) = happyShift action_36
action_75 (59) = happyShift action_37
action_75 (60) = happyShift action_38
action_75 (19) = happyGoto action_121
action_75 (23) = happyGoto action_122
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (42) = happyShift action_120
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_33

action_78 _ = happyReduce_55

action_79 (34) = happyShift action_119
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (29) = happyShift action_55
action_80 (30) = happyShift action_56
action_80 (32) = happyShift action_57
action_80 (33) = happyShift action_58
action_80 (28) = happyGoto action_118
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (29) = happyShift action_55
action_81 (30) = happyShift action_56
action_81 (32) = happyShift action_57
action_81 (33) = happyShift action_58
action_81 (28) = happyGoto action_117
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (29) = happyShift action_55
action_82 (30) = happyShift action_56
action_82 (32) = happyShift action_57
action_82 (33) = happyShift action_58
action_82 (28) = happyGoto action_116
action_82 _ = happyFail (happyExpListPerState 82)

action_83 _ = happyReduce_30

action_84 _ = happyReduce_71

action_85 _ = happyReduce_72

action_86 (43) = happyShift action_109
action_86 (69) = happyShift action_110
action_86 (70) = happyShift action_111
action_86 (71) = happyShift action_112
action_86 (72) = happyShift action_113
action_86 (73) = happyShift action_114
action_86 (27) = happyGoto action_115
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (43) = happyShift action_109
action_87 (69) = happyShift action_110
action_87 (70) = happyShift action_111
action_87 (71) = happyShift action_112
action_87 (72) = happyShift action_113
action_87 (73) = happyShift action_114
action_87 (27) = happyGoto action_108
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (43) = happyShift action_34
action_88 (58) = happyShift action_36
action_88 (59) = happyShift action_37
action_88 (60) = happyShift action_38
action_88 (23) = happyGoto action_107
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (43) = happyShift action_34
action_89 (58) = happyShift action_36
action_89 (59) = happyShift action_37
action_89 (60) = happyShift action_38
action_89 (23) = happyGoto action_106
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (43) = happyShift action_65
action_90 (64) = happyShift action_67
action_90 (65) = happyShift action_68
action_90 (66) = happyShift action_69
action_90 (26) = happyGoto action_105
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (43) = happyShift action_78
action_91 (61) = happyShift action_80
action_91 (62) = happyShift action_81
action_91 (63) = happyShift action_82
action_91 (24) = happyGoto action_104
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (43) = happyShift action_101
action_92 (67) = happyShift action_102
action_92 (68) = happyShift action_103
action_92 (25) = happyGoto action_100
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (43) = happyShift action_78
action_93 (61) = happyShift action_80
action_93 (62) = happyShift action_81
action_93 (63) = happyShift action_82
action_93 (24) = happyGoto action_99
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (43) = happyShift action_34
action_94 (58) = happyShift action_36
action_94 (59) = happyShift action_37
action_94 (60) = happyShift action_38
action_94 (23) = happyGoto action_98
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_18

action_96 (48) = happyShift action_45
action_96 (49) = happyShift action_46
action_96 (50) = happyShift action_47
action_96 (51) = happyShift action_48
action_96 (53) = happyShift action_49
action_96 (54) = happyShift action_50
action_96 (55) = happyShift action_51
action_96 (56) = happyShift action_52
action_96 (57) = happyShift action_53
action_96 (12) = happyGoto action_97
action_96 _ = happyFail (happyExpListPerState 96)

action_97 _ = happyReduce_29

action_98 _ = happyReduce_19

action_99 _ = happyReduce_20

action_100 _ = happyReduce_21

action_101 _ = happyReduce_58

action_102 (29) = happyShift action_55
action_102 (30) = happyShift action_56
action_102 (32) = happyShift action_57
action_102 (33) = happyShift action_58
action_102 (28) = happyGoto action_137
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (29) = happyShift action_55
action_103 (30) = happyShift action_56
action_103 (32) = happyShift action_57
action_103 (33) = happyShift action_58
action_103 (28) = happyGoto action_136
action_103 _ = happyFail (happyExpListPerState 103)

action_104 _ = happyReduce_22

action_105 _ = happyReduce_23

action_106 _ = happyReduce_24

action_107 _ = happyReduce_25

action_108 _ = happyReduce_27

action_109 _ = happyReduce_68

action_110 (29) = happyShift action_55
action_110 (30) = happyShift action_56
action_110 (32) = happyShift action_57
action_110 (33) = happyShift action_58
action_110 (28) = happyGoto action_135
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (29) = happyShift action_55
action_111 (30) = happyShift action_56
action_111 (32) = happyShift action_57
action_111 (33) = happyShift action_58
action_111 (28) = happyGoto action_134
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (29) = happyShift action_55
action_112 (30) = happyShift action_56
action_112 (32) = happyShift action_57
action_112 (33) = happyShift action_58
action_112 (28) = happyGoto action_133
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (29) = happyShift action_55
action_113 (30) = happyShift action_56
action_113 (32) = happyShift action_57
action_113 (33) = happyShift action_58
action_113 (28) = happyGoto action_132
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (29) = happyShift action_55
action_114 (30) = happyShift action_56
action_114 (32) = happyShift action_57
action_114 (33) = happyShift action_58
action_114 (28) = happyGoto action_131
action_114 _ = happyFail (happyExpListPerState 114)

action_115 _ = happyReduce_26

action_116 _ = happyReduce_54

action_117 _ = happyReduce_53

action_118 _ = happyReduce_52

action_119 (43) = happyShift action_78
action_119 (61) = happyShift action_80
action_119 (62) = happyShift action_81
action_119 (63) = happyShift action_82
action_119 (24) = happyGoto action_130
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (43) = happyShift action_101
action_120 (50) = happyShift action_129
action_120 (67) = happyShift action_102
action_120 (68) = happyShift action_103
action_120 (16) = happyGoto action_127
action_120 (25) = happyGoto action_128
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (42) = happyShift action_126
action_121 _ = happyFail (happyExpListPerState 121)

action_122 _ = happyReduce_41

action_123 (34) = happyShift action_125
action_123 _ = happyFail (happyExpListPerState 123)

action_124 _ = happyReduce_38

action_125 (43) = happyShift action_34
action_125 (58) = happyShift action_36
action_125 (59) = happyShift action_37
action_125 (60) = happyShift action_38
action_125 (23) = happyGoto action_143
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (43) = happyShift action_34
action_126 (55) = happyShift action_142
action_126 (58) = happyShift action_36
action_126 (59) = happyShift action_37
action_126 (60) = happyShift action_38
action_126 (20) = happyGoto action_140
action_126 (23) = happyGoto action_141
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (42) = happyShift action_139
action_127 _ = happyFail (happyExpListPerState 127)

action_128 _ = happyReduce_35

action_129 (34) = happyShift action_138
action_129 _ = happyFail (happyExpListPerState 129)

action_130 _ = happyReduce_32

action_131 _ = happyReduce_67

action_132 _ = happyReduce_66

action_133 _ = happyReduce_65

action_134 _ = happyReduce_64

action_135 _ = happyReduce_63

action_136 _ = happyReduce_57

action_137 _ = happyReduce_56

action_138 (43) = happyShift action_101
action_138 (67) = happyShift action_102
action_138 (68) = happyShift action_103
action_138 (25) = happyGoto action_149
action_138 _ = happyFail (happyExpListPerState 138)

action_139 (43) = happyShift action_78
action_139 (51) = happyShift action_148
action_139 (61) = happyShift action_80
action_139 (62) = happyShift action_81
action_139 (63) = happyShift action_82
action_139 (17) = happyGoto action_146
action_139 (24) = happyGoto action_147
action_139 _ = happyFail (happyExpListPerState 139)

action_140 (42) = happyShift action_145
action_140 _ = happyFail (happyExpListPerState 140)

action_141 _ = happyReduce_43

action_142 (34) = happyShift action_144
action_142 _ = happyFail (happyExpListPerState 142)

action_143 _ = happyReduce_40

action_144 (43) = happyShift action_34
action_144 (58) = happyShift action_36
action_144 (59) = happyShift action_37
action_144 (60) = happyShift action_38
action_144 (23) = happyGoto action_155
action_144 _ = happyFail (happyExpListPerState 144)

action_145 (43) = happyShift action_109
action_145 (57) = happyShift action_154
action_145 (69) = happyShift action_110
action_145 (70) = happyShift action_111
action_145 (71) = happyShift action_112
action_145 (72) = happyShift action_113
action_145 (73) = happyShift action_114
action_145 (22) = happyGoto action_152
action_145 (27) = happyGoto action_153
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (42) = happyShift action_151
action_146 _ = happyFail (happyExpListPerState 146)

action_147 _ = happyReduce_37

action_148 (34) = happyShift action_150
action_148 _ = happyFail (happyExpListPerState 148)

action_149 _ = happyReduce_34

action_150 (43) = happyShift action_78
action_150 (61) = happyShift action_80
action_150 (62) = happyShift action_81
action_150 (63) = happyShift action_82
action_150 (24) = happyGoto action_159
action_150 _ = happyFail (happyExpListPerState 150)

action_151 (52) = happyShift action_16
action_151 (9) = happyGoto action_158
action_151 _ = happyFail (happyExpListPerState 151)

action_152 (42) = happyShift action_157
action_152 _ = happyFail (happyExpListPerState 152)

action_153 _ = happyReduce_47

action_154 (34) = happyShift action_156
action_154 _ = happyFail (happyExpListPerState 154)

action_155 _ = happyReduce_42

action_156 (43) = happyShift action_109
action_156 (69) = happyShift action_110
action_156 (70) = happyShift action_111
action_156 (71) = happyShift action_112
action_156 (72) = happyShift action_113
action_156 (73) = happyShift action_114
action_156 (27) = happyGoto action_165
action_156 _ = happyFail (happyExpListPerState 156)

action_157 (43) = happyShift action_109
action_157 (56) = happyShift action_164
action_157 (69) = happyShift action_110
action_157 (70) = happyShift action_111
action_157 (71) = happyShift action_112
action_157 (72) = happyShift action_113
action_157 (73) = happyShift action_114
action_157 (21) = happyGoto action_162
action_157 (27) = happyGoto action_163
action_157 _ = happyFail (happyExpListPerState 157)

action_158 (40) = happyShift action_160
action_158 (42) = happyShift action_161
action_158 _ = happyFail (happyExpListPerState 158)

action_159 _ = happyReduce_36

action_160 _ = happyReduce_15

action_161 (40) = happyShift action_169
action_161 _ = happyFail (happyExpListPerState 161)

action_162 (40) = happyShift action_167
action_162 (42) = happyShift action_168
action_162 _ = happyFail (happyExpListPerState 162)

action_163 _ = happyReduce_45

action_164 (34) = happyShift action_166
action_164 _ = happyFail (happyExpListPerState 164)

action_165 _ = happyReduce_46

action_166 (43) = happyShift action_109
action_166 (69) = happyShift action_110
action_166 (70) = happyShift action_111
action_166 (71) = happyShift action_112
action_166 (72) = happyShift action_113
action_166 (73) = happyShift action_114
action_166 (27) = happyGoto action_171
action_166 _ = happyFail (happyExpListPerState 166)

action_167 _ = happyReduce_11

action_168 (40) = happyShift action_170
action_168 _ = happyFail (happyExpListPerState 168)

action_169 _ = happyReduce_14

action_170 _ = happyReduce_10

action_171 _ = happyReduce_44

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happyReduce 6 5 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (UnexItinerary happy_var_1 happy_var_5
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 5 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (UnexItinerary [] happy_var_4
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  7 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_2 : happy_var_1
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 4 8 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	(HappyTerminal (TVarName _ happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (ConstAC happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 4 8 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	(HappyTerminal (TVarName _ happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (ConstRU happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 14 9 happyReduction_10
happyReduction_10 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_12) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (UnexAmbientCondition happy_var_4 happy_var_6 happy_var_8 happy_var_10 happy_var_12
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 13 9 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_12) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (UnexAmbientCondition happy_var_4 happy_var_6 happy_var_8 happy_var_10 happy_var_12
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 (HappyTerminal (TVarName _ happy_var_3))
	_
	_
	 =  HappyAbsSyn9
		 (UnexAmbientConditionVar happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  10 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_3)
	_
	_
	 =  HappyAbsSyn10
		 (happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 12 11 happyReduction_14
happyReduction_14 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (UnexRoadUnit happy_var_2 happy_var_4 happy_var_6 happy_var_8 happy_var_10
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 11 11 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (UnexRoadUnit happy_var_2 happy_var_4 happy_var_6 happy_var_8 happy_var_10
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_1  11 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn11
		 (UnexHalt
	)

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyTerminal (TVarName _ happy_var_1))
	 =  HappyAbsSyn11
		 (UnexRoadUnitVar happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 11 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVarName _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (UnexRoadUnitVarEdit happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_3  12 happyReduction_19
happyReduction_19 (HappyAbsSyn23  happy_var_3)
	_
	_
	 =  HappyAbsSyn12
		 (UnexSpeed happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  12 happyReduction_20
happyReduction_20 (HappyAbsSyn24  happy_var_3)
	_
	_
	 =  HappyAbsSyn12
		 (UnexLength happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  12 happyReduction_21
happyReduction_21 (HappyAbsSyn25  happy_var_3)
	_
	_
	 =  HappyAbsSyn12
		 (UnexAngle happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  12 happyReduction_22
happyReduction_22 (HappyAbsSyn24  happy_var_3)
	_
	_
	 =  HappyAbsSyn12
		 (UnexCornerRadius happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  12 happyReduction_23
happyReduction_23 (HappyAbsSyn26  happy_var_3)
	_
	_
	 =  HappyAbsSyn12
		 (UnexTemp happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  12 happyReduction_24
happyReduction_24 (HappyAbsSyn23  happy_var_3)
	_
	_
	 =  HappyAbsSyn12
		 (UnexHeadWind happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  12 happyReduction_25
happyReduction_25 (HappyAbsSyn23  happy_var_3)
	_
	_
	 =  HappyAbsSyn12
		 (UnexTailWind happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  12 happyReduction_26
happyReduction_26 (HappyAbsSyn27  happy_var_3)
	_
	_
	 =  HappyAbsSyn12
		 (UnexDryAirPressure happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  12 happyReduction_27
happyReduction_27 (HappyAbsSyn27  happy_var_3)
	_
	_
	 =  HappyAbsSyn12
		 (UnexVapourPressure happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  13 happyReduction_28
happyReduction_28 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  13 happyReduction_29
happyReduction_29 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  14 happyReduction_30
happyReduction_30 (HappyAbsSyn23  happy_var_3)
	_
	_
	 =  HappyAbsSyn14
		 (happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  14 happyReduction_31
happyReduction_31 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  15 happyReduction_32
happyReduction_32 (HappyAbsSyn24  happy_var_3)
	_
	_
	 =  HappyAbsSyn15
		 (happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  15 happyReduction_33
happyReduction_33 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  16 happyReduction_34
happyReduction_34 (HappyAbsSyn25  happy_var_3)
	_
	_
	 =  HappyAbsSyn16
		 (happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  16 happyReduction_35
happyReduction_35 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  17 happyReduction_36
happyReduction_36 (HappyAbsSyn24  happy_var_3)
	_
	_
	 =  HappyAbsSyn17
		 (happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  17 happyReduction_37
happyReduction_37 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  18 happyReduction_38
happyReduction_38 (HappyAbsSyn26  happy_var_3)
	_
	_
	 =  HappyAbsSyn18
		 (happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  18 happyReduction_39
happyReduction_39 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  19 happyReduction_40
happyReduction_40 (HappyAbsSyn23  happy_var_3)
	_
	_
	 =  HappyAbsSyn19
		 (happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  19 happyReduction_41
happyReduction_41 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  20 happyReduction_42
happyReduction_42 (HappyAbsSyn23  happy_var_3)
	_
	_
	 =  HappyAbsSyn20
		 (happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  20 happyReduction_43
happyReduction_43 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  21 happyReduction_44
happyReduction_44 (HappyAbsSyn27  happy_var_3)
	_
	_
	 =  HappyAbsSyn21
		 (happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  21 happyReduction_45
happyReduction_45 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  22 happyReduction_46
happyReduction_46 (HappyAbsSyn27  happy_var_3)
	_
	_
	 =  HappyAbsSyn22
		 (happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  22 happyReduction_47
happyReduction_47 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  23 happyReduction_48
happyReduction_48 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (KmHour happy_var_2
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  23 happyReduction_49
happyReduction_49 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (MileHour happy_var_2
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_2  23 happyReduction_50
happyReduction_50 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (MeterSecond happy_var_2
	)
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  23 happyReduction_51
happyReduction_51 _
	 =  HappyAbsSyn23
		 (KmHour 0.0
	)

happyReduce_52 = happySpecReduce_2  24 happyReduction_52
happyReduction_52 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (Kmeter happy_var_2
	)
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  24 happyReduction_53
happyReduction_53 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (Meter happy_var_2
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_2  24 happyReduction_54
happyReduction_54 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (Mile happy_var_2
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  24 happyReduction_55
happyReduction_55 _
	 =  HappyAbsSyn24
		 (Meter 0.0
	)

happyReduce_56 = happySpecReduce_2  25 happyReduction_56
happyReduction_56 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (Degrees happy_var_2
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_2  25 happyReduction_57
happyReduction_57 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (Radians happy_var_2
	)
happyReduction_57 _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  25 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn25
		 (Radians 0.0
	)

happyReduce_59 = happySpecReduce_2  26 happyReduction_59
happyReduction_59 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Celsius happy_var_2
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_2  26 happyReduction_60
happyReduction_60 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Fahrenheit happy_var_2
	)
happyReduction_60 _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_2  26 happyReduction_61
happyReduction_61 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Kelvin happy_var_2
	)
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  26 happyReduction_62
happyReduction_62 _
	 =  HappyAbsSyn26
		 (Kelvin 0.0
	)

happyReduce_63 = happySpecReduce_2  27 happyReduction_63
happyReduction_63 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (Pascal happy_var_2
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_2  27 happyReduction_64
happyReduction_64 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (KPascal happy_var_2
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_2  27 happyReduction_65
happyReduction_65 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (PSIA happy_var_2
	)
happyReduction_65 _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_2  27 happyReduction_66
happyReduction_66 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (Bar happy_var_2
	)
happyReduction_66 _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_2  27 happyReduction_67
happyReduction_67 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (Torr happy_var_2
	)
happyReduction_67 _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  27 happyReduction_68
happyReduction_68 _
	 =  HappyAbsSyn27
		 (Pascal 0.0
	)

happyReduce_69 = happySpecReduce_1  28 happyReduction_69
happyReduction_69 (HappyTerminal (TDoubleValue _ happy_var_1))
	 =  HappyAbsSyn28
		 (happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  28 happyReduction_70
happyReduction_70 (HappyTerminal (TIntValue _ happy_var_1))
	 =  HappyAbsSyn28
		 (fromIntegral happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_2  28 happyReduction_71
happyReduction_71 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (-happy_var_2
	)
happyReduction_71 _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_2  28 happyReduction_72
happyReduction_72 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (happy_var_2
	)
happyReduction_72 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 74 74 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TIntValue _ happy_dollar_dollar -> cont 29;
	TDoubleValue _ happy_dollar_dollar -> cont 30;
	TVarName _ happy_dollar_dollar -> cont 31;
	TPlus _ -> cont 32;
	TMinus _ -> cont 33;
	TColon _ -> cont 34;
	TLBracket _ -> cont 35;
	TRBracket _ -> cont 36;
	TLSqrBracket _ -> cont 37;
	TRSqrBracket _ -> cont 38;
	TLCurBracket _ -> cont 39;
	TRCurBracket _ -> cont 40;
	TComma _ -> cont 41;
	TSemiColon _ -> cont 42;
	TZeroLit _ -> cont 43;
	TKeyConst _ -> cont 44;
	TKeyTypeRoadUnit _ -> cont 45;
	TKeyTypeItinerary _ -> cont 46;
	TKeyTypeHalt _ -> cont 47;
	TKeySpeed _ -> cont 48;
	TKeyLength _ -> cont 49;
	TKeySlope _ -> cont 50;
	TKeyCornerRadius _ -> cont 51;
	TKeyAmbient _ -> cont 52;
	TKeyTemp _ -> cont 53;
	TKeyHeadWind _ -> cont 54;
	TKeyTailWind _ -> cont 55;
	TKeyVapourPressure _ -> cont 56;
	TKeyDryAirPressure _ -> cont 57;
	TKeyTypeKPH _ -> cont 58;
	TKeyTypeMPH _ -> cont 59;
	TKeyTypeMS _ -> cont 60;
	TKeyTypeKm _ -> cont 61;
	TKeyTypeMeter _ -> cont 62;
	TKeyTypeMiles _ -> cont 63;
	TKeyTypeCel _ -> cont 64;
	TKeyTypeFah _ -> cont 65;
	TKeyTypeKel _ -> cont 66;
	TKeyTypeDeg _ -> cont 67;
	TKeyTypeRad _ -> cont 68;
	TKeyTypePascal _ -> cont 69;
	TKeyTypeKPascal _ -> cont 70;
	TKeyTypePSIA _ -> cont 71;
	TKeyTypeBar _ -> cont 72;
	TKeyTypeTorr _ -> cont 73;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 74 tk tks = happyError' (tks, explist)
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
happyError' :: () => ([(T)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> gError tokens)
parseCalc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


gError :: [T] -> a
gError [] = error "[END ERRORS]" 
gError (t:ts) = error ("[GRAMMAR ERROR]: [" ++ (tokenPosn t) ++ "]")
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































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
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
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
     = case happyDrop (k - ((1) :: Int)) sts of
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





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

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
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







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
