module Scenario.Scratch where

import           BasicPrelude
import           Control.Distributed.Process
import           Data.Binary
import           Data.Map.Strict             ((!?))
import           GHC.Generics

data Options = Options
  { blocking :: !Bool -- ^ Does it block instructions to other agents
  }

type Instruction = Text

type LineId = Int

type AgentId = Text

data Line = Line
  { options     :: !Options
  , instruction :: !Instruction
  , agentId     :: !AgentId
  , lineId      :: !LineId
  }

instance Eq Line where
  x == y = lineId x == lineId y

instance Ord Line where
  x `compare` y = lineId x `compare` lineId y

data InstructionRequest = InstructionRequest
  { instruction :: !Instruction
  , replyTo     :: !(SendPort Effect)
  } deriving (Generic, Show, Typeable)

instance Binary InstructionRequest

data Effect
  = Success !Text
  | Failure !Text
  deriving (Generic, Show, Typeable)

instance Binary Effect

type Program = [Line]

data AgentHandle m = AgentHandle
  { process :: Instruction -> m Effect
  }

agent :: AgentHandle IO -> Process ()
agent h@AgentHandle {process} = do
  InstructionRequest {instruction, replyTo} <- expect
  sendChan replyTo =<< (liftIO $ process instruction)
  agent h

data CoordinatorState = CoordinatorState
  { agents  :: Map AgentId ProcessId
  , pending :: [(LineId, ReceivePort Effect)]
  }

coordinator :: Program -> CoordinatorState -> Process ()
coordinator [] _ = pure ()
coordinator (line:rest) CoordinatorState {agents, pending} = do
  undefined
  -- agents' <- updateAgents
  -- send (agents' ! agentId line) (instruction 
  -- where updateAgents = undefined
