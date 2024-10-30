{-
Module      : Tactics
Description : Module for tactics acting on proofs.
Maintainer  : argh atch esss culling@pm.com
Stability   : Experimental
License     : MIT

    This project has two primary data types:

        Prop(ositions)
        T(yped)terms
    
    They are packaged into two higher order
    data types for structuring proofs: 

        ProofState  - Single step in proof
        Proof       - List of ProofStates

    Here we define tactics as functions that 
    act, conditionally, on proofs. If they 
    apply to that step, then they prepend 
    the next step in the proof to the head 
    of the underlying list. Otherwise they 
    return the proof unchanged. 

-}

module Tactic.Tactics where
    
    import Data.Propterms
    import Data.Proofstate

    type Tactic = Proof -> Proof

    -- Intro applies the deduction theorem.
    -- introState generates the new state. 
    introState :: Char -> Prop -> ProofState -> ProofState
    introState t (Imp a c) previous = State newCtx newGoal newInference
        where
            currentCtx          = getContext previous
            currentGoal         = getGoal previous
            currentInference    = getInference previous
            newTerm             = Vart t a
            newCtx              = newTerm : currentCtx
            newGoal             = c
            newInference        = ImpIntro t
    -- If goal not an implication, then intro won't change anything.
    -- Should probably tell user this.
    introState _ _ previous = previous
   
   -- Intro wraps the previous function up to work on proofs. 
    intro :: Char -> Tactic
    intro c pf = if isImp goal
                    then introState c goal current : pf
                    else pf
        where
            current = head pf
            goal    = getGoal current

    



