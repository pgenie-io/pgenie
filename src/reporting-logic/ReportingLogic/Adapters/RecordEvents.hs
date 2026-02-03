module ReportingLogic.Adapters.RecordEvents
  ( RecordEvents,
    run,
    Event (..),
  )
where

import Base.Prelude
import ReportingLogic.Algebra

data Event
  = StageEnter Text
  | StageExit
      Text
      -- | Global progress.
      Double
  deriving (Show, Eq)

data Scope = Scope
  { -- | Progress step determined for this scope.
    progressPerStage :: Double
  }

data Memory = Memory
  { substagesExited :: Int,
    events :: [Event]
  }

-- | Collects events into a list.
newtype RecordEvents a
  = RecordEvents (Scope -> Memory -> (a, Memory))
  deriving
    (Functor, Applicative, Monad)
    via (ReaderT Scope (State Memory))

instance Reports RecordEvents where
  stage name size (RecordEvents runInner) =
    RecordEvents \scope memory ->
      let eventsAfterEnter = StageEnter name : memory.events
          progressPerSubstage = scope.progressPerStage / fromIntegral size
          (result, Memory innerSubstagesExited eventsAfterInner) =
            runInner
              Scope
                { progressPerStage = progressPerSubstage
                }
              Memory
                { substagesExited = 0,
                  events = eventsAfterEnter
                }
          totalInnerProgress = fromIntegral innerSubstagesExited * progressPerSubstage
          exitProgress = scope.progressPerStage - totalInnerProgress
          exitEvent = StageExit name exitProgress
          eventsAfterExit = exitEvent : eventsAfterInner
       in (result, Memory {substagesExited = memory.substagesExited + 1, events = eventsAfterExit})

run :: RecordEvents a -> ([Event], a)
run (RecordEvents action) =
  let (result, memory) =
        action
          Scope
            { progressPerStage = 1.0
            }
          Memory
            { substagesExited = 0,
              events = []
            }
   in (memory.events, result)
