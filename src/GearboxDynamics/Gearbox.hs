{-
    Models the type of a gear and gearbox, as well as
    encapsulating conversion functions for torque and
    angular speed.
-}
module GearboxDynamics.Gearbox where
import Commons.ISUnits

newtype GearRatio =
            GRImp Double -- output / input
            deriving (Show, Eq)
--
instance Ord GearRatio where
    compare (GRImp a) (GRImp b) = compare b a -- A is a higher gear if a has a lower ratio.


type Gearbox = [GearRatio]

{-|
    Takes an explicit declaration of a GearRatio (GRImp input output)
    and converts it into an implicit declaration.

    Eg: GRExp 1 2 => GRImp (1 / 2)
-}
fGearRatioToGRImp :: GearRatio -> GearRatio
fGearRatioToGRImp (GRImp r)   = GRImp r

fCompOut :: GearRatio -> Double -> Double
fCompOut (GRImp gr) n = n / gr

fCompIn :: GearRatio -> Double -> Double
fCompIn (GRImp gr) n = n * gr


{-|
    Given input revs returns the revs outputted by the gear ratio.
-}
fRevsOut :: Rev -> GearRatio -> Rev
fRevsOut (RPM x) gr = RPM $ round $ fCompOut gr (fromIntegral x)
fRevsOut (HZ x)  gr = HZ $ fCompOut gr x

{-|
    Given input revs returns the revs outputted by the gear ratio system.
    Order is first to last element given.

    Eg: fRevsOutSeq (HZ 10.5) [(3, 1), (3.5, 1)] = 1 HZ
        = (10.5 HZ outputs 1 HZ at the end of the system)
-}
fRevsOutSeq :: Foldable t => Rev -> t GearRatio -> Rev
fRevsOutSeq = foldl fRevsOut


{-|
    Given the target output revs returns the required input revs.
-}
fRevsIn :: GearRatio -> Rev -> Rev
fRevsIn gr (RPM x) = RPM $ round $ fCompIn gr (fromIntegral x)
fRevsIn gr (HZ x)  = HZ $ fCompIn gr x

{-|
    Given the target output revs returns the required input revs.
    Order is first to last element given.

    Eg: fRevsReqSeq (HZ 1) [(3, 1), (3.5, 1)] = 10.5 HZ
        = (for the system to output 1 HZ requires 10.5 HZ at the start of the system)
-}
fRevsInSeq :: Foldable t => Rev -> t GearRatio -> Rev
fRevsInSeq = foldr fRevsIn


{-|
    Computes in Newton Meters (Nm) the output torque of the given
    torque when put through a gear ratio.

    TorqueOutput = TorqueInput * GearRatio
                 = TorqueInput * (radiusSndGear / radiusFstGear)
-}
fTorqueOut :: Torque -> GearRatio -> Torque
fTorqueOut (Nm t) gr = Nm $ fCompIn gr t

{-|
    Computes in Newton Meters (Nm) the output torque of the given
    torque when put into a system gear ratios.

    Folds left the fTorqueOut function on the ordered gear ratios
    of the gearbox.
-}
fTorqueOutSeq :: Torque -> Gearbox -> Torque
fTorqueOutSeq = foldl fTorqueOut

{-|
    Computes in Newton Meters (Nm) the input torque of the given
    output torque when put through a gear ratio.

    TorqueInput = TorqueOutput / GearRatio
                = TorqueOutput * (radiusFstGear / radiusSndGear)
                = TorqueOutput / (radiusSndGear / radiusFstGear)
-}
fTorqueIn :: GearRatio -> Torque -> Torque
fTorqueIn gr (Nm t) = Nm $ fCompOut gr t

{-|
    Computes in Newton Meters (Nm) the input torque of the given
    output torque when put through a system gear ratios.

    Folds left the fTorqueIn function on the ordered gear ratios
    of the gearbox.
-}
fTorqueInSeq :: Foldable t => Torque ->  t GearRatio -> Torque
fTorqueInSeq = foldr fTorqueIn