module Vehicle.VehicleConsts.TestVehicles.TestVehicle where

import Commons.ISUnits

import Vehicle.Vehicle
import Vehicle.VehicleState
--
import PowerUnit.PowerUnitTypes
import PowerUnit.ICPowerUnit.Fuel.Fuel
import PowerUnit.EPowerUnit.Electricity
--
import qualified Vehicle.VehicleConsts.Fiat500Consts as Fiat500
import qualified Vehicle.VehicleConsts.FordFusionV6Consts as FordFusionV6
import qualified Vehicle.VehicleConsts.BMWi3Consts as BMWi3
import qualified Vehicle.VehicleConsts.NissanLeafConsts as NissanLeaf

testVehicle =
    Vehicle
        { vehicleNakedMass       = Kg 1300
        , vehicleGearbox         = Fiat500.gearbox
        , vehicleGearboxNaming   = genericGearNaming Fiat500.gearbox
        , vehicleDiff            = Fiat500.diff
        , vehicleWheelRadius     = Fiat500.wheelRadius
        , vehicleDragCoeff       = 0.350
        , vehicleFrontalArea     = Meter2 2.54
        , vehicleRollingCoeff    = 0.02
        --
        , vehiclePowerUnit =
            PowerUnit 
                { puICPowerUnit = Fiat500.icPowerUnit
                , puEPowerUnit  = BMWi3.ePowerUnit
                }
        , vehicleFuelUnitCostPerLiter = FuelUnitCostPerLiter 1.55
        , vehicleElectricityCostPerKWattHour = ElectricityCostPerKWattHour 0.13
        --
        , vehicleState =
            VehicleState
                { vehicleStateSpeed = MeterSecond 0
                --
                , vehicleStateFuelMass = Kg 30
                --
                , vehicleStateChargeMax = KWattHour 8
                , vehicleStateChargeNow = KWattHour 5
                }
        }