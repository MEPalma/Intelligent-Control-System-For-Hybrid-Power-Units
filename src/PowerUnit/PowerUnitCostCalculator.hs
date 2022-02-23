{-
    Encapsulates functions for the computation of the running costs
    of a power unit.
-}
module PowerUnit.PowerUnitCostCalculator
    (
        SubUnitConfiguration,
        subUnitConfigToPowerUnitConfig,
        --
        propulsionCostPerSecondOfConfig
    ) where
--
import Commons.ISUnits
import Commons.Utilities
import Commons.Currency
--
import Vehicle.Vehicle
import GearboxDynamics.Gearbox
--
import PowerUnit.PowerUnitTypes
import PowerUnit.PredictionStrategy
--
import PowerUnit.ICPowerUnit.ICPowerUnit
import PowerUnit.ICPowerUnit.ICPowerUnitType
import PowerUnit.ICPowerUnit.ICEngine
import PowerUnit.ICPowerUnit.Fuel.Fuel
--
import PowerUnit.EPowerUnit.EEngine
import PowerUnit.EPowerUnit.EPowerUnitType
import PowerUnit.EPowerUnit.Electricity
--
import PowerUnit.ICPowerUnit.ICTelInputNode
import qualified PowerUnit.ICPowerUnit.ICPowerUnitEfficiencyModeller as ICPUEM
import qualified PowerUnit.ICPowerUnit.ICPowerUnitEfficiencyRetrieval as ICPUKNN
--
import PowerUnit.EPowerUnit.ETelInputNode
import qualified PowerUnit.EPowerUnit.EPowerUnitEfficiencyModeller as EPUEM
import qualified PowerUnit.EPowerUnit.EPowerUnitEfficiencyRetrieval as EPUKNN
--
import Debug.Trace

{-# ANN module "HLint: ignore Reduce duplication" #-}


{-
    Data type for an intermediate definition of a power unit
    configuration given a power unit mode.
-}
data SubUnitConfiguration =
          SUCPureICEngine ICPowerUnitConfig
        | SUCPureEEngine EPowerUnitConfig
        | SUCCombined Integer ICPowerUnitConfig EPowerUnitConfig
        | SUCFullECompensation ICPowerUnitConfig EPowerUnitConfig
        deriving (Show, Eq)

{-
    Converts a partial power unit configuration to a power unit configuration.
-}
subUnitConfigToPowerUnitConfig :: SubUnitConfiguration -> GearRatio -> PowerUnitConfig
subUnitConfigToPowerUnitConfig (SUCPureICEngine icpuConf) gr = PUCPureICEngine gr icpuConf
subUnitConfigToPowerUnitConfig (SUCPureEEngine epuConf) gr = PUCPureEEngine gr epuConf
subUnitConfigToPowerUnitConfig (SUCCombined r icpuConf epuConf) gr = PUCCombined r gr icpuConf epuConf
subUnitConfigToPowerUnitConfig (SUCFullECompensation icpuConf epuConf) gr = PUCFullECompensation gr icpuConf epuConf

{-
    Of a given power unit mode, computes its required power unit
    configuration, and continues to compute its running costs per
    unit time.
-}
propulsionCostPerSecondOfConfig ::
       Vehicle
    -> Speed
    -> PowerUnitMode
    -> PowerUnitDeliveryRequirement
    -> (PropulsionCostPerSecond, SubUnitConfiguration)
propulsionCostPerSecondOfConfig v vSpeed puMode puDelReq =
    let icPuPredMode = icpuPredictionStrategy $ vehicleICPowerUnit v
        ePuPredMode  = epuPredictionStrategy $ vehicleEPowerUnit v
     in case puMode of
        PUMPureICEngine ->
            case icPuPredMode of
                KNN -> propulsionCostPerSecondOfConfigWithKNN        v vSpeed puMode puDelReq
                REG -> propulsionCostPerSecondOfConfigWithRegression v vSpeed puMode puDelReq
        --
        PUMPureEEngine ->
            case ePuPredMode of
                KNN -> propulsionCostPerSecondOfConfigWithKNN        v vSpeed puMode puDelReq
                REG -> propulsionCostPerSecondOfConfigWithRegression v vSpeed puMode puDelReq
        --
        _ -> -- Combined | FullElectricCompensation
            let Nm reqTorque = pudrTorque puDelReq
                --
                (icTorque, eTorque) =
                    case puMode of
                        PUMCombined icPercent ->
                            let icLoadRatio = fromIntegral icPercent / 100
                                eLoadRatio = 1 - icLoadRatio
                                --
                                icTorque = Nm $ icLoadRatio * reqTorque
                                eTorque = Nm $ eLoadRatio * reqTorque
                             in (icTorque, eTorque)
                        --
                        PUMFullECompensation ->
                            fullECompensationToSplit (vehiclePowerUnit v) (Nm reqTorque)
                --
                icPuDelReq = puDelReq{pudrTorque = icTorque}
                (PropulsionCostPerSecond icCost, SUCPureICEngine icpuConfig)
                    = propulsionCostPerSecondOfConfig v vSpeed PUMPureICEngine icPuDelReq
                --
                ePuDelReq = puDelReq{pudrTorque = eTorque}
                (PropulsionCostPerSecond eCost, SUCPureEEngine epuConfig)
                    = propulsionCostPerSecondOfConfig v vSpeed PUMPureEEngine ePuDelReq
                --
                totalCost = PropulsionCostPerSecond $ icCost + eCost
                --
             in case puMode of
                    PUMCombined icPercent ->
                        (totalCost, SUCCombined icPercent icpuConfig epuConfig)
                    --
                    PUMFullECompensation ->
                        (totalCost, SUCFullECompensation icpuConfig epuConfig)


{-
    Calculates propulsion costs using the KNN regression modelling
    of the power unit's components.
-}
propulsionCostPerSecondOfConfigWithKNN :: Vehicle -> Speed -> PowerUnitMode -> PowerUnitDeliveryRequirement -> (PropulsionCostPerSecond, SubUnitConfiguration)
-- PUCPureICEngine.
propulsionCostPerSecondOfConfigWithKNN v vSpeed PUMPureICEngine (PowerUnitDeliveryRequirement eTorque eSpeed ambient) =
    if eTorque == Nm 0 -- Engine off.
    then (PropulsionCostPerSecond 0, SUCPureICEngine $ ICPowerUnitConfig eTorque eSpeed _NO_MIXTURE_FLOW)
    else
    let -- Get the vehicle's ICEngine.
        vICPowerUnit = vehiclePowerUnit v
        icvICPowerUnit = puICPowerUnit vICPowerUnit
        -- Compose the query for the prediction model.
        query = ICTelQuery eTorque eSpeed ambient
        eff = ICPUKNN.effOf (icpuKnnConfig icvICPowerUnit) query (icpuTelPoints icvICPowerUnit)
        -- Predict the required flow.
        -- Mix.
        afMix = fOttoCycleAFMForTorque (icpuCompRatio icvICPowerUnit) ambient eTorque
        -- Flow
        predictedFlow = mixtureToFlowCorrected afMix (icpuNumCylinders icvICPowerUnit) eSpeed eff
        --
        icEngineFuelConsumption = fFuelFlowToFuelConsumption predictedFlow vSpeed
        --
        -- And here is the propulsion cost.
        proCost = fICEngineFuelConsumptionToPropulsionCostPerSecond icEngineFuelConsumption vSpeed (vehicleFuelUnitCostPerLiter v)
        --
        icpuConfig = ICPowerUnitConfig eTorque eSpeed predictedFlow
        --
     in (proCost, SUCPureICEngine icpuConfig)
--
propulsionCostPerSecondOfConfigWithKNN v _vSpeed PUMPureEEngine (PowerUnitDeliveryRequirement eTorque eSpeed ambient) =
    if eTorque == Nm 0 -- Motor off.
    then (PropulsionCostPerSecond 0, SUCPureEEngine $ EPowerUnitConfig eTorque eSpeed (Ampere 0) (epuVoltage (vehicleEPowerUnit v)))
    else
    let -- Get the vehicle's EEngine.
        evEPowerUnit = vehicleEPowerUnit v
        --
        eVoltage = epuVoltage evEPowerUnit
        --
        -- Get expected eff.
        eff = EPUKNN.effOf2 (epuKnnConfigPower evEPowerUnit) (eTorque, eSpeed, ambient) (epuTelPoints evEPowerUnit)
        -- Predict expected current.
        expCurr = getCurrentWithEff eTorque eSpeed (epuVoltage evEPowerUnit) eff
        --
        -- AmpereHour for one second.
        expAmpHour = ampsToAmpHour expCurr (Seconds 1)
       
        KWattHour expKWh = currentEnergyToKWattHour $ ampHourToWattHour expAmpHour eVoltage
        --
        --
        -- Cost per second.
        --
        ElectricityCostPerKWattHour eCostPerKWh = vehicleElectricityCostPerKWattHour v -- Cost/KWh
        --
        eCostPerSecond = PropulsionCostPerSecond $ eCostPerKWh *  fromRational (toRational expKWh)
        --
        epuConfig = EPowerUnitConfig eTorque eSpeed expCurr eVoltage
        --
     in (eCostPerSecond, SUCPureEEngine epuConfig)

{-
    Calculates propulsion costs using the multivariate regression modelling
    of the power unit's components.
-}
propulsionCostPerSecondOfConfigWithRegression :: Vehicle -> Speed -> PowerUnitMode -> PowerUnitDeliveryRequirement -> (PropulsionCostPerSecond, SubUnitConfiguration)
-- PUCPureICEngine.
propulsionCostPerSecondOfConfigWithRegression v vSpeed PUMPureICEngine (PowerUnitDeliveryRequirement eTorque eSpeed ambient) =
    if eTorque == Nm 0 -- Engine off.
    then (PropulsionCostPerSecond 0, SUCPureICEngine $ ICPowerUnitConfig eTorque eSpeed _NO_MIXTURE_FLOW)
    else
    let -- Get the vehicle's ICEngine.
        vICPowerUnit = vehiclePowerUnit v
        icvICPowerUnit = puICPowerUnit vICPowerUnit
        -- Compose the query for the prediction model.
        icpueQuery = ICPUEM.ICQuery (icpuNumCylinders icvICPowerUnit) (icpuCompRatio icvICPowerUnit) eTorque eSpeed ambient
        -- Predict the required flow.
        predictedFlow =
            let x = ICPUEM.predict (icpuEffConfig icvICPowerUnit) icpueQuery
             in case x of
                (Just f) -> f
                Nothing  -> MixtureFlow (GramPerSecond 0) (GramPerSecond 0)
        --
        icEngineFuelConsumption = fFuelFlowToFuelConsumption predictedFlow vSpeed
        --
        -- And here is the propulsion cost.
        proCost = fICEngineFuelConsumptionToPropulsionCostPerSecond icEngineFuelConsumption vSpeed (vehicleFuelUnitCostPerLiter v)
        --
        icpuConfig = ICPowerUnitConfig eTorque eSpeed predictedFlow
        --
     in (proCost, SUCPureICEngine icpuConfig)
propulsionCostPerSecondOfConfigWithRegression v _vSpeed PUMPureEEngine (PowerUnitDeliveryRequirement eTorque eSpeed ambient) =
    if eTorque == Nm 0 -- Motor off.
    then (PropulsionCostPerSecond 0, SUCPureEEngine $ EPowerUnitConfig eTorque eSpeed (Ampere 0) (epuVoltage (vehicleEPowerUnit v)))
    else
    let -- Get the vehicle's EEngine.
        vPowerUnit = vehiclePowerUnit v
        evEPowerUnit = puEPowerUnit vPowerUnit
        --
        eVoltage = epuVoltage evEPowerUnit
        --
        --
        -- Get expected current.
        epueQuery = EPUEM.EQuery eTorque eSpeed ambient
        expCurr = EPUEM.predict (epuEffConfig evEPowerUnit) epueQuery eVoltage
        --
        -- AmpereHour for one second.
        expAmpHour = ampsToAmpHour expCurr (Seconds 1)
        --
        -- Time to cover 1 meter.
        KWattHour expKWh = currentEnergyToKWattHour $ ampHourToWattHour expAmpHour eVoltage
        --
        -- Cost per second.
        ElectricityCostPerKWattHour eCostPerKWh = vehicleElectricityCostPerKWattHour v -- Cost/KWh
        --
        eCostPerSecond = PropulsionCostPerSecond $ eCostPerKWh *  fromRational (toRational expKWh)
        --
        epuConfig = EPowerUnitConfig eTorque eSpeed expCurr eVoltage
        --
     in (eCostPerSecond, SUCPureEEngine epuConfig)