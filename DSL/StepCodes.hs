module StepCodes where

data Code = Task Kind (Maybe [Feature])
          | Mult Code Code
          | Seq Code Code
          | Alt Code Code
          deriving(Show)

data Kind = Physical
          | Virtual
          | Cognitive
          | Measurement
          | NonHuman
          | Selection
          | Monitoring
          | Branching Int
          | Looping Int
          deriving(Show)

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
