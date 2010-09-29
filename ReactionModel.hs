--Package ModML-Units
--Package ModML-Reactions
module ReactionModel
where
import qualified ModML.Units.UnitsDAEModel as U
import qualified ModML.Reactions.Reactions as R

reactionModel = do
  -- TODO: Add Process.

unitsModel = do
  R.runReactionBuilderInUnitBuilder reactionModel

model = do
  U.unitsToCore unitsModel
