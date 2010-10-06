{-# LANGUAGE NoMonomorphismRestriction,DeriveDataTypeable,TemplateHaskell #-}
-- +Require ModML-Units
-- +Require ModML-Reactions
-- +Require typehash
module ReactionModel
where
import qualified ModML.Units.UnitsDAEModel as U
import qualified ModML.Core.BasicDAEModel as B
import ModML.Units.UnitsDAEOpAliases
import qualified ModML.Reactions.Reactions as R
import qualified Data.Data as D
import qualified Data.TypeHash as D
import ModML.Units.SIUnits

gasConstant = U.realConstant (uJoule $*$ (uMole $*$ uKelvin) $**$ (-1)) 8.31447215
uConcentration = uMole $*$ uLitre $**$ (-1)
uFlux = uConcentration $*$ uSecond $**$ (-1)
uNthOrderPerConcentration n = uConcentration $**$ (-n) $*$ uSecond $**$ (-1)

uConcentrationR = U.liftUnits uConcentration
R.declareNamedTaggedEntity [e|uConcentrationR|] "H2" "hydrogen2"
R.declareNamedTaggedEntity [e|uConcentrationR|] "O2" "oxygen2"
R.declareNamedTaggedEntity [e|uConcentrationR|] "O." "oxygenRadical"
R.declareNamedTaggedEntity [e|uConcentrationR|] "Water" "water"
R.declareNamedTaggedCompartment "Reactor Vessel" "vessel"


U.declareRealVariable [e|uKelvin|] "temperature" "temperature"
--data TemperatureTag = TemperatureTag deriving (D.Typeable, D.Data)
--temperatureTag = D.typeCode TemperatureTag
-- temperature :: Monad m => U.ModelBuilderT m U.RealVariable
--temperature = U.newNamedTaggedRealVariable uKelvin temperatureTag "temperature"

temperatureM = temperature >>= return . U.RealVariableE

-- Karach, S.P.; Osherov, V.I.
-- Ab Initio Analysis of the Transition States on the Lowest Triplet H2O2 Potential Surface
-- J. Chem. Phys. 110 (1999)
-- http://kinetics.nist.gov/kinetics/Detail?id=1999KAR/OSH11918-11927:16
h2_o2_React compartment =
    do
      h2var <- R.addEntity R.EssentialForProcess R.CantBeCreatedByProcess R.ModifiedByProcess (-1)
                 (hydrogen2 `R.withCompartment` compartment)
      o2var <- R.addEntity R.EssentialForProcess R.CantBeCreatedByProcess R.ModifiedByProcess (-1)
                 (oxygen2 `R.withCompartment` compartment)
      watervar <- R.addEntity R.NotEssentialForProcess R.CanBeCreatedByProcess R.ModifiedByProcess 1
                    (water `R.withCompartment` compartment)
      oradvar <- R.addEntity R.NotEssentialForProcess R.CanBeCreatedByProcess R.ModifiedByProcess 1
                    (oxygenRadical `R.withCompartment` compartment)
      R.rateEquation $ h2var .*. o2var .*.
                       (U.realConstant (uNthOrderPerConcentration 2) 1.37E9) .*.
                       (U.expX $ U.realConstant (uJoule $*$ uMole $**$ (-1)) (-295496) ./.
                        (gasConstant .*. temperatureM))

-- Tsang, W.; Hampson, R.F.
-- Chemical kinetic data base for combustion chemistry. Part I. Methane and related compounds
-- J. Phys. Chem. Ref. Data 15 (1986)
-- http://kinetics.nist.gov/kinetics/Detail?id=1986TSA/HAM1087:264
o_o_radical_Combine compartment =
    do
      oradvar <- R.addEntity R.EssentialForProcess R.CantBeCreatedByProcess R.ModifiedByProcess (-2) (oxygenRadical `R.inCompartment` compartment)
      o2var <- R.addEntity R.NotEssentialForProcess R.CanBeCreatedByProcess R.ModifiedByProcess 1 (oxygen2 `R.inCompartment` compartment)
      R.rateEquation $ oradvar.**.U.realConstant U.dimensionless 3 .*.
                        (U.realConstant (uNthOrderPerConcentration 3) 1.89E7) .*.
                        (U.expX $ U.realConstant (uJoule $*$ uMole $**$ (-1)) 7483 ./.
                         (gasConstant .*. temperatureM))

reactionModel = do
  R.newAllCompartmentProcess h2_o2_React
  R.newAllCompartmentProcess h2_o2_React
  -- Declare the initial presence of oxygen in vessel
  R.addEntityInstance (oxygen2 `R.inCompartment` vessel) (R.entityFromProcesses (U.realConstant uConcentration 1) (U.realConstant uFlux 0))
  -- And hydrogen...
  R.addEntityInstance (hydrogen2 `R.inCompartment` vessel) (R.entityFromProcesses (U.realConstant uConcentration 1) (U.realConstant uFlux 0))

unitsModel :: Monad m => U.ModelBuilderT m ()
unitsModel = do
  R.runReactionBuilderInUnitBuilder reactionModel
  temperatureM `U.newEq` (U.realConstant uKelvin 1000)

model = B.buildModel $ do
  U.unitsToCore uSecond unitsModel
