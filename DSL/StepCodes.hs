{- | Step Codes for Human Actor (Keeley Abbott -- 2016)
     Description: A programmer provides a set of protocols for the human actor
         that describe goal-directed tasks for the human actor to complete as
         well as some additional information, and potentially how to recover
         from exceptions.
-}

module StepCodes where

{- | categorization of protocol steps.
-}
data Code = Task Kind (Maybe [Feature]) String
          | Mult Kind Code
          | Seq Kind Code
          | Alt Kind Code
          deriving(Show)


{- | describes the primary action of the step for the human actor.
     physical: manipulate a physical object
     virtual: manipulate a virtual object
     cognitive: evaluate or analyze
     measurement: take a measurement
     selection: make a selection decision
     monitoring: watch or monitor
     branching: perform parallel actions
     looping: repeat a process or step
-}
data Kind = Physical
          | Virtual
          | Cognitive
          | Measurement
          | Selection
          | Monitoring
          | Branching Int
          | Looping Int
          deriving(Show)


{- | one or more optional additional features that give some additional
         information to the human actor about completing the step.
     advice: suggested method or process
     constraint: avoid this method or process
     expectation: expected result or outcome
     optional: not a required process or method
     wiggle: alternative method or process
     contingency: in-line troubleshooting
     information: background information
     reference: outside source
     mapping: map instruction over a set
     visual: human visual patterm matching processing
-}
data Feature = Advice
             | Constraint
             | Expectation
             | Optional
             | Wiggle
             | Contingency
             | Information
             | Reference Link
             | Mapping
             | Visual
             deriving(Show)

type Link = String
