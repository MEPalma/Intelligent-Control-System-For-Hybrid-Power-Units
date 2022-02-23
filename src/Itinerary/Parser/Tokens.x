{
module Itinerary.Parser.Tokens where
}

%wrapper "posn"
$digit = 0-9
$alpha = [a-zA-Z]
$alphaSmall = [a-z]

tokens :-
$white+       ;
  "//".*        ; 
  $digit+\.$digit+       { tok (\p s -> TDoubleValue p (read s)) }
  0             { tok (\p s -> TZeroLit p ) }
  $digit+       { tok (\p s -> TIntValue p (read s)) }
  \+            { tok (\p s -> TPlus p) }
  \-            { tok (\p s -> TMinus p) }
  \:            { tok (\p s -> TColon p) }
  \(            { tok (\p s -> TLBracket p) }
  \)            { tok (\p s -> TRBracket p) }
  \[            { tok (\p s -> TLSqrBracket p) }
  \]            { tok (\p s -> TRSqrBracket p) }
  \{            { tok (\p s -> TLCurBracket p) }
  \}            { tok (\p s -> TRCurBracket p) }
  \,            { tok (\p s -> TComma p) }
  \;            { tok (\p s -> TSemiColon p) }

  Const         { tok (\p s -> TKeyConst p) }
  RoadUnit      { tok (\p s -> TKeyTypeRoadUnit p) }
  Itinerary     { tok (\p s -> TKeyTypeItinerary p) }
  Halt          { tok (\p s -> TKeyTypeHalt p) }

  Speed         { tok (\p s -> TKeySpeed p) }
  Length        { tok (\p s -> TKeyLength p) }
  Slope         { tok (\p s -> TKeySlope p) }
  CornerRadius  { tok (\p s -> TKeyCornerRadius p) }
  Ambient       { tok (\p s -> TKeyAmbient p) }

  Temp          { tok (\p s -> TKeyTemp p) }
  HeadWind      { tok (\p s -> TKeyHeadWind p) }
  TailWind      { tok (\p s -> TKeyTailWind p) }
  VapourPressure { tok (\p s -> TKeyVapourPressure p) }
  DryAirPressure { tok (\p s -> TKeyDryAirPressure p) }

  KPH { tok (\p s -> TKeyTypeKPH p) }
  MPH { tok (\p s -> TKeyTypeMPH p) }
  MS { tok (\p s -> TKeyTypeMS p) }

  Km { tok (\p s -> TKeyTypeKm p) }
  Meter { tok (\p s -> TKeyTypeMeter p) }
  Miles { tok (\p s -> TKeyTypeMiles p) }

  Cel { tok (\p s -> TKeyTypeCel p) }
  Fah { tok (\p s -> TKeyTypeFah p) }
  Kel { tok (\p s -> TKeyTypeKel p) }

  Deg { tok (\p s -> TKeyTypeDeg p) }
  Rad { tok (\p s -> TKeyTypeRad p) }

  Pascal  { tok (\p s -> TKeyTypePascal p) }
  KPascal { tok (\p s -> TKeyTypeKPascal p) }
  PSIA    { tok (\p s -> TKeyTypePSIA p) }
  Bar     { tok (\p s -> TKeyTypeBar p) }
  Torr    { tok (\p s -> TKeyTypeTorr p) }

  $alphaSmall [$alpha $digit \_ \’]* { tok (\p s -> TVarName p s) }


{
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
}