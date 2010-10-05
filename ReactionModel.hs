{-# LANGUAGE DeriveDataTypeable,TemplateHaskell #-}
-- +Require ModML-Units
-- +Require ModML-Reactions
module ReactionModel
where
import qualified ModML.Units.UnitsDAEModel as U
import ModML.Units.UnitsDAEOpAliases
import qualified ModML.Reactions.Reactions as R
import qualified Data.Data as D
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

-- Karach, S.P.; Osherov, V.I.
-- Ab Initio Analysis of the Transition States on the Lowest Triplet H2O2 Potential Surface
-- J. Chem. Phys. 110 (1999)
-- http://kinetics.nist.gov/kinetics/Detail?id=1999KAR/OSH11918-11927:16
h2_o2_React compartment =
    do
      h2var <- R.addEntity R.EssentialForProcess R.CantBeCreatedByProcess R.ModifiedByProcess (-1)
                 (hydrogen2 `R.inCompartment` compartment)
      o2var <- R.addEntity R.EssentialForProcess R.CantBeCreatedByProcess R.ModifiedByProcess (-1)
                 (oxygen2 `R.inCompartment` compartment)
      watervar <- R.addEntity R.NotEssentialForProcess R.CanBeCreatedByProcess R.ModifiedByProcess 1
                    (water `R.inCompartment` compartment)
      oradvar <- R.addEntity R.NotEssentialForProcess R.CanBeCreatedByProcess R.ModifiedByProcess 1
                    (oxygenRadical `R.inCompartment` compartment)
      R.rateEquation $ h2var .*. o2var .*.
                       (U.realConstant (uNthOrderPerConcentration 2) 1.37E9) .*.
                       (U.expX $ U.realConstant (uJoule $*$ uMole $**$ (-1)) (-295496) ./.
                        (gasConstant .*. temperature))

-- Tsang, W.; Hampson, R.F.
-- Chemical kinetic data base for combustion chemistry. Part I. Methane and related compounds
-- J. Phys. Chem. Ref. Data 15 (1986)
-- http://kinetics.nist.gov/kinetics/Detail?id=1986TSA/HAM1087:264
o_o_radical_Combine compartment =
    do
      oradvar <- R.addEntity R.EssentialForProcess R.CantBeCreatedByProcess R.ModifiedByProcess (-2) (oxygenRadical, compartment)
      o2var <- R.addEntity R.NotEssentialForProcess R.CanBeCreatedByProcess R.ModifiedByProcess 1 (oxygen2, compartment)
      R.rateEquation $ oradvar.**.3 .*.
                        (U.realConstant (uNthOrderPerConcentration 3) 1.89E7) .*.
                        (U.expX $ U.realConstant (uJoule $*$ uMole $**$ (-1)) 7483 ./.
                         (gasConstant .*. temperature))

reactionModel = do
  R.newAllCompartmentProcess h2_o2_React
  R.newAllCompartmentProcess h2_o2_React
  -- Declare the initial presence of oxygen in vessel
  R.addEntityInstance (oxygen2, vessel) (R.EntityFromProcesses (U.realConstant uConcentration 1) (U.realConstant uFlux 0))
  -- And hydrogen...
  R.addEntityInstance (hydrogen2, vessel) (R.EntityFromProcesses (U.realConstant uConcentration 1) (U.realConstant uFlux 0))

unitsModel = do
  R.runReactionBuilderInUnitBuilder reactionModel
  temperature `U.newEq` (U.realConstant uKelvin 1000)

model = do
  U.unitsToCore unitsModel
