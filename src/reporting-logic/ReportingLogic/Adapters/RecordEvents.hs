module ReportingLogic.Adapters.RecordEvents
  ( RecordEvents,
    Event (..),
    run,
  )
where

import Base.Prelude
import ReportingLogic.Algebra

-- | Collects events into a list.
newtype RecordEvents a
  = RecordEvents
      ([Event] -> (a, [Event]))
  deriving
    (Functor, Applicative, Monad)
    via (State [Event])

instance Reports RecordEvents where
  reportStageEnter stageName =
    RecordEvents \events -> ((), StageEnter stageName : events)
  reportStageExit stageName localProgress =
    RecordEvents \events ->
      ( (),
        StageExit stageName localProgress : events
      )
  mapReports f (RecordEvents action) =
    RecordEvents \events ->
      case action [] of
        (result, newEvents) ->
          ( result,
            map
              ( \case
                  StageEnter name -> StageEnter name
                  StageExit name progress -> StageExit name (f progress)
              )
              newEvents
              ++ events
          )

data Event
  = StageEnter Text
  | StageExit Text Double
  deriving (Show, Eq)

run :: RecordEvents a -> ([Event], a)
run (RecordEvents action) =
  let (result, events) = action []
   in (reverse events, result)
