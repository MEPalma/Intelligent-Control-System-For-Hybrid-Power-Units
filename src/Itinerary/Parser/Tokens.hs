{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
{-# LINE 1 "Tokens.x" #-}

module Itinerary.Parser.Tokens where

#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Array.Base (unsafeAt)
#else
import Array
#endif
{-# LINE 1 "templates/wrappers.hs" #-}
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.






import Data.Word (Word8)
















import Data.Char (ord)
import qualified Data.Bits

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]



type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type


type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p,c,_bs,_s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))





























































-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.


data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+alex_tab_size-1) `div` alex_tab_size)*alex_tab_size+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)


-- -----------------------------------------------------------------------------
-- Default monad
















































































































-- -----------------------------------------------------------------------------
-- Monad (with ByteString input)







































































































-- -----------------------------------------------------------------------------
-- Basic wrapper

























-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version
































-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.


--alexScanTokens :: String -> [token]
alexScanTokens str0 = go (alexStartPos,'\n',[],str0)
  where go inp__@(pos,_,_,str) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
                AlexSkip  inp__' _ln     -> go inp__'
                AlexToken inp__' len act -> act pos (take len str) : go inp__'



-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version














-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.














alex_tab_size :: Int
alex_tab_size = 8
alex_base :: Array Int Int
alex_base = listArray (0 :: Int, 181)
  [ -8
  , -41
  , -114
  , -93
  , -106
  , -91
  , -90
  , -67
  , -28
  , -103
  , -89
  , -50
  , -136
  , -10
  , -97
  , -86
  , -81
  , -96
  , -87
  , -78
  , -76
  , -48
  , -71
  , -35
  , -27
  , -70
  , -69
  , -44
  , -66
  , 118
  , -43
  , -15
  , -46
  , -73
  , -55
  , -34
  , -40
  , 277
  , -25
  , -18
  , -29
  , -1
  , -22
  , 276
  , 293
  , 292
  , 274
  , 294
  , 295
  , 265
  , 268
  , 269
  , 270
  , 285
  , 286
  , 296
  , 279
  , 280
  , 299
  , 282
  , 300
  , 284
  , 291
  , 297
  , 298
  , 301
  , 283
  , 302
  , 288
  , 336
  , 287
  , 307
  , 303
  , 304
  , 305
  , 306
  , 325
  , 326
  , 308
  , 309
  , 311
  , 312
  , 327
  , 310
  , 313
  , 314
  , 315
  , 317
  , 322
  , 318
  , 319
  , 316
  , 321
  , 320
  , 323
  , 324
  , 0
  , 328
  , 329
  , 330
  , 331
  , 332
  , 333
  , 337
  , 338
  , 339
  , 334
  , 341
  , 340
  , 0
  , 393
  , 342
  , 344
  , 345
  , 347
  , 335
  , 351
  , 348
  , 352
  , 349
  , 360
  , 365
  , 361
  , 364
  , 353
  , 350
  , 363
  , 368
  , 354
  , 533
  , 534
  , 603
  , 661
  , 597
  , 0
  , 844
  , 848
  , 811
  , 829
  , 841
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 1056
  ]

alex_table :: Array Int Int
alex_table = listArray (0 :: Int, 1311)
  [ 0
  , 135
  , 135
  , 135
  , 135
  , 135
  , 136
  , 153
  , 157
  , 158
  , 163
  , 164
  , 170
  , 120
  , 174
  , 178
  , 167
  , 181
  , 130
  , 129
  , 127
  , 4
  , 124
  , 119
  , 135
  , 9
  , 117
  , 114
  , 113
  , 11
  , 106
  , 112
  , 143
  , 144
  , 93
  , 140
  , 149
  , 141
  , 92
  , 1
  , 138
  , 139
  , 139
  , 139
  , 139
  , 139
  , 139
  , 139
  , 139
  , 139
  , 142
  , 150
  , 121
  , 118
  , 108
  , 16
  , 105
  , 59
  , 28
  , 21
  , 23
  , 104
  , 22
  , 95
  , 31
  , 78
  , 10
  , 8
  , 54
  , 7
  , 115
  , 17
  , 44
  , 116
  , 24
  , 90
  , 37
  , 25
  , 71
  , 70
  , 103
  , 168
  , 107
  , 145
  , 58
  , 146
  , 60
  , 102
  , 101
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 147
  , 100
  , 148
  , 132
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 13
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 61
  , 26
  , 27
  , 94
  , 18
  , 36
  , 40
  , 38
  , 39
  , 41
  , 42
  , 43
  , 91
  , 89
  , 99
  , 84
  , 51
  , 86
  , 56
  , 88
  , 49
  , 50
  , 87
  , 85
  , 57
  , 66
  , 74
  , 67
  , 81
  , 83
  , 72
  , 79
  , 80
  , 73
  , 69
  , 55
  , 0
  , 0
  , 0
  , 46
  , 47
  , 68
  , 65
  , 45
  , 76
  , 77
  , 53
  , 48
  , 35
  , 63
  , 64
  , 62
  , 82
  , 34
  , 33
  , 20
  , 30
  , 32
  , 166
  , 15
  , 14
  , 175
  , 19
  , 165
  , 75
  , 0
  , 0
  , 0
  , 0
  , 122
  , 111
  , 6
  , 5
  , 123
  , 125
  , 128
  , 126
  , 3
  , 172
  , 177
  , 2
  , 176
  , 180
  , 12
  , 179
  , 173
  , 171
  , 162
  , 52
  , 169
  , 161
  , 160
  , 159
  , 156
  , 155
  , 0
  , 154
  , 110
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 133
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 152
  , 151
  , 137
  , 137
  , 137
  , 137
  , 137
  , 137
  , 137
  , 137
  , 137
  , 137
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 135
  , 135
  , 135
  , 135
  , 135
  , -1
  , 137
  , 137
  , 137
  , 137
  , 137
  , 137
  , 137
  , 137
  , 137
  , 137
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 131
  , 135
  , 139
  , 139
  , 139
  , 139
  , 139
  , 139
  , 139
  , 139
  , 139
  , 139
  , 131
  , 0
  , 139
  , 139
  , 139
  , 139
  , 139
  , 139
  , 139
  , 139
  , 139
  , 139
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 132
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 134
  , 133
  , 13
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 109
  , 110
  , 29
  , 96
  , 96
  , 96
  , 97
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 0
  , 0
  , 0
  , 0
  , 181
  , 0
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 181
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 98
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  ]

alex_check :: Array Int Int
alex_check = listArray (0 :: Int, 1311)
  [ -1
  , 9
  , 10
  , 11
  , 12
  , 13
  , 47
  , 121
  , 101
  , 115
  , 101
  , 101
  , 115
  , 80
  , 103
  , 65
  , 83
  , 153
  , 115
  , 105
  , 101
  , 117
  , 109
  , 101
  , 32
  , 101
  , 97
  , 97
  , 97
  , 73
  , 103
  , 97
  , 40
  , 41
  , 101
  , 43
  , 44
  , 45
  , 105
  , 47
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 58
  , 59
  , 80
  , 101
  , 97
  , 101
  , 111
  , 65
  , 66
  , 67
  , 68
  , 101
  , 70
  , 111
  , 72
  , 73
  , 101
  , 75
  , 76
  , 77
  , 97
  , 105
  , 80
  , 101
  , 82
  , 83
  , 84
  , 99
  , 86
  , 114
  , 105
  , 109
  , 97
  , 91
  , 111
  , 93
  , 101
  , 105
  , 117
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 123
  , 117
  , 125
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 97
  , 99
  , 83
  , 85
  , 101
  , 105
  , 115
  , 87
  , 87
  , 115
  , 115
  , 115
  , 101
  , 101
  , 111
  , 105
  , 97
  , 109
  , 101
  , 97
  , 115
  , 115
  , 97
  , 97
  , 101
  , 111
  , 117
  , 65
  , 101
  , 105
  , 97
  , 80
  , 80
  , 105
  , 121
  , 82
  , -1
  , -1
  , -1
  , 98
  , 100
  , 112
  , 110
  , 100
  , 114
  , 114
  , 110
  , 108
  , 100
  , 114
  , 116
  , 114
  , 114
  , 108
  , 110
  , 108
  , 114
  , 112
  , 72
  , 110
  , 110
  , 100
  , 116
  , 72
  , 114
  , -1
  , -1
  , -1
  , -1
  , 110
  , 114
  , 114
  , 114
  , 110
  , 110
  , 108
  , 116
  , 112
  , 104
  , 108
  , 114
  , 108
  , 114
  , 128
  , 114
  , 108
  , 108
  , 100
  , 97
  , 114
  , 100
  , 112
  , 116
  , 104
  , 100
  , -1
  , 116
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 116
  , 116
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , 32
  , 33
  , 34
  , 35
  , 36
  , 37
  , 38
  , 39
  , 40
  , 41
  , 42
  , 43
  , 44
  , 45
  , 46
  , 47
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 58
  , 59
  , 60
  , 61
  , 62
  , 63
  , 64
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , 91
  , 92
  , 93
  , 94
  , 95
  , 96
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 123
  , 124
  , 125
  , 126
  , 127
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 9
  , 10
  , 11
  , 12
  , 13
  , 10
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 46
  , 32
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 46
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 226
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_deflt :: Array Int Int
alex_deflt = listArray (0 :: Int, 181)
  [ -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 109
  , 109
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 134
  , 134
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 136
  , 136
  , 136
  , -1
  , 136
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_accept = listArray (0 :: Int, 181)
  [ AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccSkip
  , AlexAccSkip
  , AlexAcc 44
  , AlexAcc 43
  , AlexAcc 42
  , AlexAcc 41
  , AlexAcc 40
  , AlexAcc 39
  , AlexAcc 38
  , AlexAcc 37
  , AlexAcc 36
  , AlexAcc 35
  , AlexAcc 34
  , AlexAcc 33
  , AlexAcc 32
  , AlexAcc 31
  , AlexAcc 30
  , AlexAcc 29
  , AlexAcc 28
  , AlexAcc 27
  , AlexAcc 26
  , AlexAcc 25
  , AlexAcc 24
  , AlexAcc 23
  , AlexAcc 22
  , AlexAcc 21
  , AlexAcc 20
  , AlexAcc 19
  , AlexAcc 18
  , AlexAcc 17
  , AlexAcc 16
  , AlexAcc 15
  , AlexAcc 14
  , AlexAcc 13
  , AlexAcc 12
  , AlexAcc 11
  , AlexAcc 10
  , AlexAcc 9
  , AlexAcc 8
  , AlexAcc 7
  , AlexAcc 6
  , AlexAcc 5
  , AlexAcc 4
  , AlexAcc 3
  , AlexAcc 2
  , AlexAcc 1
  , AlexAcc 0
  ]

alex_actions = array (0 :: Int, 45)
  [ (44,alex_action_2)
  , (43,alex_action_3)
  , (42,alex_action_4)
  , (41,alex_action_5)
  , (40,alex_action_6)
  , (39,alex_action_7)
  , (38,alex_action_8)
  , (37,alex_action_9)
  , (36,alex_action_10)
  , (35,alex_action_11)
  , (34,alex_action_12)
  , (33,alex_action_13)
  , (32,alex_action_14)
  , (31,alex_action_15)
  , (30,alex_action_16)
  , (29,alex_action_17)
  , (28,alex_action_18)
  , (27,alex_action_19)
  , (26,alex_action_20)
  , (25,alex_action_21)
  , (24,alex_action_22)
  , (23,alex_action_23)
  , (22,alex_action_24)
  , (21,alex_action_25)
  , (20,alex_action_26)
  , (19,alex_action_27)
  , (18,alex_action_28)
  , (17,alex_action_29)
  , (16,alex_action_30)
  , (15,alex_action_31)
  , (14,alex_action_32)
  , (13,alex_action_33)
  , (12,alex_action_34)
  , (11,alex_action_35)
  , (10,alex_action_36)
  , (9,alex_action_37)
  , (8,alex_action_38)
  , (7,alex_action_39)
  , (6,alex_action_40)
  , (5,alex_action_41)
  , (4,alex_action_42)
  , (3,alex_action_43)
  , (2,alex_action_44)
  , (1,alex_action_45)
  , (0,alex_action_46)
  ]

{-# LINE 69 "Tokens.x" #-}

tok f p s = f p s

data T =
  TVarName AlexPosn String |
  TIntValue AlexPosn Int   |
  TDoubleValue AlexPosn Double |

  TPlus AlexPosn       |
  TMinus AlexPosn      |

  TLBracket AlexPosn     |
  TRBracket AlexPosn     |
  TRSqrBracket  AlexPosn |
  TLSqrBracket AlexPosn  |
  TRCurBracket AlexPosn  |
  TLCurBracket AlexPosn  |

  TColon AlexPosn          |
  TComma AlexPosn           |
  TSemiColon AlexPosn       |
  TZeroLit AlexPosn      |

  TKeyConst AlexPosn |
  TKeyTypeTemp AlexPosn |
  TKeyTypeRoadUnit AlexPosn |
  TKeyTypeItinerary AlexPosn |
  TKeyTypeHalt AlexPosn |

  TKeySpeed AlexPosn |
  TKeyLength AlexPosn |
  TKeySlope AlexPosn |
  TKeyCornerRadius AlexPosn |
  TKeyAmbient AlexPosn |

  TKeyTemp AlexPosn |
  TKeyHeadWind AlexPosn |
  TKeyTailWind AlexPosn |
  TKeyVapourPressure AlexPosn |
  TKeyDryAirPressure AlexPosn |

  TKeyTypeKPH AlexPosn |
  TKeyTypeMPH AlexPosn |
  TKeyTypeMS AlexPosn |

  TKeyTypeKm AlexPosn |
  TKeyTypeMeter AlexPosn |
  TKeyTypeMiles AlexPosn |

  TKeyTypeCel AlexPosn |
  TKeyTypeFah AlexPosn |
  TKeyTypeKel AlexPosn |

  TKeyTypeDeg AlexPosn |
  TKeyTypeRad AlexPosn |

  TKeyTypePascal AlexPosn |
  TKeyTypeKPascal AlexPosn |
  TKeyTypePSIA AlexPosn |
  TKeyTypeBar AlexPosn |
  TKeyTypeTorr AlexPosn

  deriving (Eq,Show)



showPosn (AlexPn a l c) = "WITH:(" ++ show(a) ++ ") AT:(" ++ show(l) ++ ":" ++ show(c) ++ ")"

tokenPosn :: T -> String

tokenPosn (TVarName p x) = showPosn p
tokenPosn (TIntValue p n) = showPosn p
tokenPosn (TDoubleValue p n) = showPosn p

tokenPosn (TPlus p) = showPosn p
tokenPosn (TMinus p) = showPosn p

tokenPosn (TLBracket p) = showPosn p
tokenPosn (TRBracket p) = showPosn p
tokenPosn (TRSqrBracket p) = showPosn p
tokenPosn (TLSqrBracket p) = showPosn p
tokenPosn (TRCurBracket p) = showPosn p
tokenPosn (TLCurBracket p) = showPosn p

tokenPosn (TColon p) = showPosn p
tokenPosn (TComma p) = showPosn p
tokenPosn (TSemiColon p) = showPosn p
tokenPosn (TZeroLit p) = showPosn p

tokenPosn (TKeyConst p) = showPosn p
tokenPosn (TKeyTypeTemp p) = showPosn p
tokenPosn (TKeyTypeRoadUnit p) = showPosn p
tokenPosn (TKeyTypeItinerary p) = showPosn p
tokenPosn (TKeyTypeHalt p) = showPosn p

tokenPosn (TKeySpeed p) = showPosn p
tokenPosn (TKeyLength p) = showPosn p
tokenPosn (TKeySlope p) = showPosn p
tokenPosn (TKeyCornerRadius p) = showPosn p
tokenPosn (TKeyAmbient p) = showPosn p

tokenPosn (TKeyTemp p) = showPosn p
tokenPosn (TKeyHeadWind p) = showPosn p
tokenPosn (TKeyTailWind p) = showPosn p
tokenPosn (TKeyVapourPressure p) = showPosn p
tokenPosn (TKeyDryAirPressure p) = showPosn p

tokenPosn (TKeyTypeKPH p) = showPosn p
tokenPosn (TKeyTypeMPH p) = showPosn p
tokenPosn (TKeyTypeMS p) = showPosn p

tokenPosn (TKeyTypeKm p) = showPosn p
tokenPosn (TKeyTypeMeter p) = showPosn p
tokenPosn (TKeyTypeMiles p) = showPosn p

tokenPosn (TKeyTypeCel p) = showPosn p
tokenPosn (TKeyTypeFah p) = showPosn p
tokenPosn (TKeyTypeKel p) = showPosn p

tokenPosn (TKeyTypeDeg p) = showPosn p
tokenPosn (TKeyTypeRad p) = showPosn p

tokenPosn (TKeyTypePascal p) = showPosn p
tokenPosn (TKeyTypeKPascal p) = showPosn p
tokenPosn (TKeyTypePSIA p) = showPosn p
tokenPosn (TKeyTypeBar p) = showPosn p
tokenPosn (TKeyTypeTorr p) = showPosn p

alex_action_2 =  tok (\p s -> TDoubleValue p (read s)) 
alex_action_3 =  tok (\p s -> TZeroLit p ) 
alex_action_4 =  tok (\p s -> TIntValue p (read s)) 
alex_action_5 =  tok (\p s -> TPlus p) 
alex_action_6 =  tok (\p s -> TMinus p) 
alex_action_7 =  tok (\p s -> TColon p) 
alex_action_8 =  tok (\p s -> TLBracket p) 
alex_action_9 =  tok (\p s -> TRBracket p) 
alex_action_10 =  tok (\p s -> TLSqrBracket p) 
alex_action_11 =  tok (\p s -> TRSqrBracket p) 
alex_action_12 =  tok (\p s -> TLCurBracket p) 
alex_action_13 =  tok (\p s -> TRCurBracket p) 
alex_action_14 =  tok (\p s -> TComma p) 
alex_action_15 =  tok (\p s -> TSemiColon p) 
alex_action_16 =  tok (\p s -> TKeyConst p) 
alex_action_17 =  tok (\p s -> TKeyTypeRoadUnit p) 
alex_action_18 =  tok (\p s -> TKeyTypeItinerary p) 
alex_action_19 =  tok (\p s -> TKeyTypeHalt p) 
alex_action_20 =  tok (\p s -> TKeySpeed p) 
alex_action_21 =  tok (\p s -> TKeyLength p) 
alex_action_22 =  tok (\p s -> TKeySlope p) 
alex_action_23 =  tok (\p s -> TKeyCornerRadius p) 
alex_action_24 =  tok (\p s -> TKeyAmbient p) 
alex_action_25 =  tok (\p s -> TKeyTemp p) 
alex_action_26 =  tok (\p s -> TKeyHeadWind p) 
alex_action_27 =  tok (\p s -> TKeyTailWind p) 
alex_action_28 =  tok (\p s -> TKeyVapourPressure p) 
alex_action_29 =  tok (\p s -> TKeyDryAirPressure p) 
alex_action_30 =  tok (\p s -> TKeyTypeKPH p) 
alex_action_31 =  tok (\p s -> TKeyTypeMPH p) 
alex_action_32 =  tok (\p s -> TKeyTypeMS p) 
alex_action_33 =  tok (\p s -> TKeyTypeKm p) 
alex_action_34 =  tok (\p s -> TKeyTypeMeter p) 
alex_action_35 =  tok (\p s -> TKeyTypeMiles p) 
alex_action_36 =  tok (\p s -> TKeyTypeCel p) 
alex_action_37 =  tok (\p s -> TKeyTypeFah p) 
alex_action_38 =  tok (\p s -> TKeyTypeKel p) 
alex_action_39 =  tok (\p s -> TKeyTypeDeg p) 
alex_action_40 =  tok (\p s -> TKeyTypeRad p) 
alex_action_41 =  tok (\p s -> TKeyTypePascal p) 
alex_action_42 =  tok (\p s -> TKeyTypeKPascal p) 
alex_action_43 =  tok (\p s -> TKeyTypePSIA p) 
alex_action_44 =  tok (\p s -> TKeyTypeBar p) 
alex_action_45 =  tok (\p s -> TKeyTypeTorr p) 
alex_action_46 =  tok (\p s -> TVarName p s) 
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine































































alexIndexInt16OffAddr arr off = arr ! off




















alexIndexInt32OffAddr arr off = arr ! off











quickIndex arr i = arr ! i


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input__ (sc)
  = alexScanUser undefined input__ (sc)

alexScanUser user__ input__ (sc)
  = case alex_scan_tkn user__ input__ (0) input__ sc AlexNone of
  (AlexNone, input__') ->
    case alexGetByte input__ of
      Nothing ->



                                   AlexEOF
      Just _ ->



                                   AlexError input__'

  (AlexLastSkip input__'' len, _) ->



    AlexSkip input__'' len

  (AlexLastAcc k input__''' len, _) ->



    AlexToken input__''' len (alex_actions ! k)


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user__ orig_input len input__ s last_acc =
  input__ `seq` -- strict in the input
  let
  new_acc = (check_accs (alex_accept `quickIndex` (s)))
  in
  new_acc `seq`
  case alexGetByte input__ of
     Nothing -> (new_acc, input__)
     Just (c, new_input) ->



      case fromIntegral c of { (ord_c) ->
        let
                base   = alexIndexInt32OffAddr alex_base s
                offset = (base + ord_c)
                check  = alexIndexInt16OffAddr alex_check offset

                new_s = if (offset >= (0)) && (check == ord_c)
                          then alexIndexInt16OffAddr alex_table offset
                          else alexIndexInt16OffAddr alex_deflt s
        in
        case new_s of
            (-1) -> (new_acc, input__)
                -- on an error, we want to keep the input *before* the
                -- character that failed, not after.
            _ -> alex_scan_tkn user__ orig_input (if c < 0x80 || c >= 0xC0 then (len + (1)) else len)
                                                -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
                        new_input new_s new_acc
      }
  where
        check_accs (AlexAccNone) = last_acc
        check_accs (AlexAcc a  ) = AlexLastAcc a input__ (len)
        check_accs (AlexAccSkip) = AlexLastSkip  input__ (len)

        check_accs (AlexAccPred a predx rest)
           | predx user__ orig_input (len) input__
           = AlexLastAcc a input__ (len)
           | otherwise
           = check_accs rest
        check_accs (AlexAccSkipPred predx rest)
           | predx user__ orig_input (len) input__
           = AlexLastSkip input__ (len)
           | otherwise
           = check_accs rest


data AlexLastAcc
  = AlexNone
  | AlexLastAcc !Int !AlexInput !Int
  | AlexLastSkip     !AlexInput !Int

data AlexAcc user
  = AlexAccNone
  | AlexAcc Int
  | AlexAccSkip

  | AlexAccPred Int (AlexAccPred user) (AlexAcc user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user__ in1 len in2
  = p1 user__ in1 len in2 && p2 user__ in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _
alexPrevCharIs c _ input__ _ _ = c == alexInputPrevChar input__

alexPrevCharMatches f _ input__ _ _ = f (alexInputPrevChar input__)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _
alexPrevCharIsOneOf arr _ input__ _ _ = arr ! alexInputPrevChar input__

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user__ _ _ input__ =
     case alex_scan_tkn user__ input__ (0) input__ sc AlexNone of
          (AlexNone, _) -> False
          _ -> True
        -- TODO: there's no need to find the longest
        -- match when checking the right context, just
        -- the first match will do.

