{
module Itinerary.Parser.Grammar where
import Itinerary.Parser.Tokens
import Itinerary.Parser.UnexItinerary
import Commons.ISUnits
}

%name parseCalc
%tokentype { T }
%error { gError }
%token

    int             { TIntValue _ $$ }
    double          { TDoubleValue _ $$ }
    var             { TVarName _ $$ }

    '+'             { TPlus _ }
    '-'             { TMinus _ }
    ':'             { TColon _ }

    '('             { TLBracket _ }
    ')'             { TRBracket _ }
    '['             { TLSqrBracket _ }
    ']'             { TRSqrBracket _ }
    '{'             { TLCurBracket _ }
    '}'             { TRCurBracket _ }

    ','             { TComma _ }
    ';'             { TSemiColon _ }
    zero            { TZeroLit _ }


    Const           { TKeyConst _ }
    RoadUnit        { TKeyTypeRoadUnit _ }
    Itinerary       { TKeyTypeItinerary _ }
    Halt            { TKeyTypeHalt _ }

    Speed           { TKeySpeed _ }
    Length          { TKeyLength _ }
    Slope           { TKeySlope _ }
    CornerRadius    { TKeyCornerRadius _ }
    Ambient         { TKeyAmbient _ }

    Temp            { TKeyTemp _ }
    HeadWind        { TKeyHeadWind _ }
    TailWind        { TKeyTailWind _ }
    VapourPressure  { TKeyVapourPressure _ }
    DryAirPressure   { TKeyDryAirPressure _ }


    KPH             { TKeyTypeKPH _ }
    MPH             { TKeyTypeMPH _ }
    MS              { TKeyTypeMS _ }

    Km              { TKeyTypeKm _ }
    Meter           { TKeyTypeMeter _ }
    Miles           { TKeyTypeMiles _ }

    Cel             { TKeyTypeCel _ }
    Fah             { TKeyTypeFah _ }
    Kel             { TKeyTypeKel _ }

    Deg             { TKeyTypeDeg _ }
    Rad             { TKeyTypeRad _ }

    Pascal          { TKeyTypePascal _ }
    KPascal         { TKeyTypeKPascal _ }
    PSIA            { TKeyTypePSIA _ }
    Bar             { TKeyTypeBar _ }
    Torr            { TKeyTypeTorr _ }


%right ';'
%left '+' '-'


%%

prods : gItinerary { $1 }


gItinerary : gConstCollection Itinerary ':' '[' gRoadUnitSequence ']'  { UnexItinerary $1 $5 }
           | Itinerary ':' '[' gRoadUnitSequence ']'                   { UnexItinerary [] $4 }


gRoadUnitSequence : gHeadLessRoadUnit { [$1] }
                  | gRoadUnitSequence ',' gHeadLessRoadUnit { $1 ++ [$3] }


gConstCollection : gConst { [$1] }
                 | gConstCollection gConst { $2 : $1 }

gConst : Const var gAmbientConditions ';' { ConstAC $2 $3 }
       | Const var gRoadUnit ';'          { ConstRU $2 $3 }


gAmbientConditions : Ambient ':' '{' gTempTerm ';' gHeadWindTerm ';' gTailWindTerm ';' gDryAirPressureTerm ';' gVapourPressureTerm ';' '}'   { UnexAmbientCondition $4 $6 $8 $10 $12 }
                   | Ambient ':' '{' gTempTerm ';' gHeadWindTerm ';' gTailWindTerm ';' gDryAirPressureTerm ';' gVapourPressureTerm '}'       { UnexAmbientCondition $4 $6 $8 $10 $12 }
                   | Ambient ':' var  { UnexAmbientConditionVar $3 }

gRoadUnit : RoadUnit ':' gHeadLessRoadUnit { $3 }

gHeadLessRoadUnit : '{' gSpeedTerm ';' gLengthTerm ';' gSlopeAngleTerm ';' gCornerRadiusTerm ';' gAmbientConditions ';' '}'    { UnexRoadUnit $2 $4 $6 $8 $10 }
                  | '{' gSpeedTerm ';' gLengthTerm ';' gSlopeAngleTerm ';' gCornerRadiusTerm ';' gAmbientConditions '}'        { UnexRoadUnit $2 $4 $6 $8 $10 }
                  | Halt { UnexHalt }
                  | var { UnexRoadUnitVar $1 }
                  | var '(' gRemapCollection ')' { UnexRoadUnitVarEdit $1 $3 }


gRemap : Speed ':' gSpeedUnitValue            { UnexSpeed $3 }
       | Length ':' gLengthUnitValue          { UnexLength $3 }
       | Slope ':' gAngleUnitValue            { UnexAngle $3 }
       | CornerRadius ':' gLengthUnitValue    { UnexCornerRadius $3 }
       | Temp ':' gTempUnitValue              { UnexTemp $3 }
       | HeadWind ':' gSpeedUnitValue         { UnexHeadWind $3 }
       | TailWind ':' gSpeedUnitValue         { UnexTailWind $3 }
       | DryAirPressure ':' gPressureUnitValue { UnexDryAirPressure $3 }
       | VapourPressure ':' gPressureUnitValue { UnexVapourPressure $3 }

gRemapCollection : gRemap { [$1] }
                 | gRemapCollection ',' gRemap { $1 ++ [$3] }



gSpeedTerm : Speed ':' gSpeedUnitValue { $3 }
           | gSpeedUnitValue { $1 }

gLengthTerm : Length ':' gLengthUnitValue { $3 }
            | gLengthUnitValue { $1 }

gSlopeAngleTerm : Slope ':' gAngleUnitValue { $3 }
                | gAngleUnitValue { $1 }

gCornerRadiusTerm : CornerRadius ':' gLengthUnitValue { $3 }
                  | gLengthUnitValue { $1 }

gTempTerm : Temp ':' gTempUnitValue { $3 }
          | gTempUnitValue { $1 }

gHeadWindTerm : HeadWind ':' gSpeedUnitValue { $3 }
              | gSpeedUnitValue { $1 }

gTailWindTerm : TailWind ':' gSpeedUnitValue { $3 }
              | gSpeedUnitValue { $1 }

gVapourPressureTerm : VapourPressure ':' gPressureUnitValue { $3 }
                    | gPressureUnitValue { $1 }

gDryAirPressureTerm : DryAirPressure ':' gPressureUnitValue { $3 }
                   | gPressureUnitValue { $1 }

gSpeedUnitValue : KPH gDouble { KmHour $2 }
                | MPH gDouble { MileHour $2 }
                | MS  gDouble { MeterSecond $2 }
                | zero { KmHour 0.0 }

gLengthUnitValue : Km gDouble { Kmeter $2 }
                 | Meter gDouble { Meter $2 }
                 | Miles gDouble { Mile $2 }
                 | zero { Meter 0.0 }

gAngleUnitValue : Deg gDouble { Degrees $2 }
                | Rad gDouble { Radians $2 }
                | zero { Radians 0.0 }

gTempUnitValue : Cel gDouble { Celsius $2 }
               | Fah gDouble { Fahrenheit $2 }
               | Kel gDouble { Kelvin $2 }
               | zero { Kelvin 0.0 }

gPressureUnitValue : Pascal gDouble { Pascal $2 }
                   | KPascal gDouble { KPascal $2 }
                   | PSIA gDouble { PSIA $2 }
                   | Bar gDouble { Bar $2 }
                   | Torr gDouble { Torr $2 }
                   | zero { Pascal 0.0 }

gDouble : double { $1 }
        | int { fromIntegral $1 }
        | '-' gDouble { -$2 }
        | '+' gDouble { $2 }

{
gError :: [T] -> a
gError [] = error "[END ERRORS]" 
gError (t:ts) = error ("[GRAMMAR ERROR]: [" ++ (tokenPosn t) ++ "]")
}