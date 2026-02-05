module StagingAlgebra.Runtimes.RecordEvents
  ( RecordEvents,
    run,
    Event (..),
  )
where

import Base.Prelude
import StagingAlgebra.Algebra

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

instance Stages RecordEvents where
  stage name substagesCount (RecordEvents runInner) =
    RecordEvents \scope memory ->
      if substagesCount > 0
        then
          let eventsAfterEnter = StageEnter name : memory.events
              progressPerSubstage = scope.progressPerStage / fromIntegral substagesCount
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
        else
          let eventsAfterEnter = StageEnter name : memory.events
              (result, Memory _innerSubstagesExited eventsAfterInner) =
                runInner
                  Scope
                    { progressPerStage = 0
                    }
                  Memory
                    { substagesExited = 0,
                      events = eventsAfterEnter
                    }
              exitEvent = StageExit name scope.progressPerStage
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
