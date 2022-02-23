{-
    Data types of unexpanded road units. These are road units
    which have been constructed straight from grammar derivations
    and required a remapping of their values from environment to
    obtain their type safe RoadUnit variant.
-}
module Itinerary.Parser.UnexItinerary where
    --
import Itinerary.Itinerary
import Commons.ISUnits

{-
    Data type of a remapping of a road unit's values.
-}
data UnexRemap =
          UnexSpeed Speed
        | UnexLength Length
        | UnexAngle Angle
        | UnexCornerRadius Length
        | UnexTemp Temp
        | UnexHeadWind Speed
        | UnexTailWind Speed
        | UnexDryAirPressure Pressure
        | UnexVapourPressure Pressure
        deriving (Show, Eq)

{-
    Data type for an unexpanded ambient condition.
-}
data UnexAmbientCondition =
          UnexAmbientCondition Temp Speed Speed Pressure Pressure
        | UnexAmbientConditionVar String
        deriving (Show, Eq)

{-
    Data type for an unexpanded road unit.
-}
data UnexRoadUnit = UnexRoadUnit Speed Length Angle Length UnexAmbientCondition
              | UnexHalt
              | UnexRoadUnitVar String
              | UnexRoadUnitVarEdit String [UnexRemap]
              deriving (Show, Eq)

{-
    Data type for a constant definition, a mapping of a variable name
    to an unexpanded ambient condition of road unit.
-}
data Const = ConstAC String UnexAmbientCondition
           | ConstRU String UnexRoadUnit
           deriving (Show, Eq)

{-
    Data type for an unexpanded itinerary: the environment for the
    construction of Itinerary.
-}
data UnexItinerary = UnexItinerary [Const] [UnexRoadUnit]
          deriving (Show, Eq)

{-
    Converts an unexpanded itinerary to a true itinerary.
-}
toItinerary :: UnexItinerary -> Itinerary
toItinerary (UnexItinerary consts unexrus) = map (recomposeRoadUnit consts) unexrus

{-
    Converts an unexpanded road unit to a true RoadUnit.
-}
recomposeRoadUnit :: [Const] -> UnexRoadUnit -> RoadUnit
recomposeRoadUnit consts r =
    case r of
        -- Return cases:
        UnexHalt -> getHaltRoadUnit
        --
        (UnexRoadUnit s l a cr (UnexAmbientCondition t hw tw wp vp)) -> RoadUnit s l a cr (AmbientCondition t hw tw wp vp)
        --
        -- Reduction cases:
        --
        -- Retrieve ambient conditions from variable name.
        (UnexRoadUnit s l a cr (UnexAmbientConditionVar varName)) ->
            let res = getConstWithVarName consts varName
             in case res of
                    (ConstAC _ unexac) -> recomposeRoadUnit consts (UnexRoadUnit s l a cr unexac)
                    _ -> error $ "[EXPECTED AmbientCondition DEFINITION ON VARIABLE NAME \"" ++ varName ++ "\"]"
        --
        -- Retrieve road unit from variable name.
        (UnexRoadUnitVar varName) ->
            let res = getConstWithVarName consts varName
             in case res of
                    (ConstRU _ unexr) -> recomposeRoadUnit consts unexr
                    _ -> error $ "[EXPECTED RoadUnit DEFINITION ON VARIABLE NAME \"" ++ varName ++ "\"]"

        -- Retrieve road unit from variable name and apply remaps.
        (UnexRoadUnitVarEdit varName remaps) ->
            let res = getConstWithVarName consts varName
             in case res of
                    (ConstRU _ unexru) ->
                        case unexru of
                            -- Base case, fully expanded RoadUnit: apply remap and recompose.
                            (UnexRoadUnit s l a cr (UnexAmbientCondition t hw tw wp vp)) ->
                                recomposeRoadUnit consts (applyRemaps unexru remaps)
                            -- Road Unit has referenced temperature: rebuild UnexRoadUnit with temperature, then remap and recompose.
                            (UnexRoadUnit s l a cr (UnexAmbientConditionVar varName)) ->
                                let tempVal = getConstWithVarName consts varName
                                 in case tempVal of
                                        (ConstAC _ acvalue) ->
                                            recomposeRoadUnit consts (applyRemaps (UnexRoadUnit s l a cr acvalue) remaps)
                                        --
                                        _ -> error $ "[EXPECTED AmbientCondition DEFINITION ON VARIABLE NAME \"" ++ varName ++ "\"]"
                            --
                            _ -> error $ "[EXPECTED RoadUnit REFERENCE TO REMAP ON VARIABLE NAME \"" ++ varName ++ "\"]"
                    --
                    _ -> error $ "[EXPECTED RoadUnit DEFINITION ON VARIABLE NAME \"" ++ varName ++ "\"]"

{-
    Returns the first constant in the environment with the given name.
-}
getConstWithVarName :: [Const] -> String -> Const
getConstWithVarName [] varName' = error $ "[UNRESOLVABLE VARIABLE REFERENCE \"" ++ varName' ++ "\"]"
getConstWithVarName (c:cs) varName' =
    case c of
        (ConstAC varName obj) -> if varName == varName' then ConstAC varName obj
                                    else getConstWithVarName cs varName'
        (ConstRU varName obj) -> if varName == varName' then ConstRU varName obj
                                    else getConstWithVarName cs varName'

{-
    Applies all remaps attached to an unexpanded road unit.
-}
applyRemaps :: UnexRoadUnit -> [UnexRemap] -> UnexRoadUnit
applyRemaps UnexHalt _ = error "[CANNOT REMAP ON Halt TYPE]"
applyRemaps ru [] = ru
applyRemaps (UnexRoadUnit s l a cr (UnexAmbientCondition t hw tw ap vp)) (rm:rms) = 
    case rm of
        (UnexSpeed s')           -> applyRemaps (UnexRoadUnit s' l a cr (UnexAmbientCondition t hw tw ap vp)) rms
        (UnexLength l')          -> applyRemaps (UnexRoadUnit s l' a cr (UnexAmbientCondition t hw tw ap vp)) rms
        (UnexAngle a')           -> applyRemaps (UnexRoadUnit s l a' cr (UnexAmbientCondition t hw tw ap vp)) rms
        (UnexCornerRadius cr')   -> applyRemaps (UnexRoadUnit s l a cr' (UnexAmbientCondition t hw tw ap vp)) rms
        (UnexTemp t')            -> applyRemaps (UnexRoadUnit s l a cr (UnexAmbientCondition t' hw tw ap vp)) rms
        (UnexHeadWind hw')       -> applyRemaps (UnexRoadUnit s l a cr (UnexAmbientCondition t hw' tw ap vp)) rms
        (UnexTailWind tw')       -> applyRemaps (UnexRoadUnit s l a cr (UnexAmbientCondition t hw tw' ap vp)) rms
        (UnexDryAirPressure ap') -> applyRemaps (UnexRoadUnit s l a cr (UnexAmbientCondition t hw tw ap' vp)) rms
        (UnexVapourPressure vp') -> applyRemaps (UnexRoadUnit s l a cr (UnexAmbientCondition t hw tw ap vp')) rms
