{-|
Module      : Proofstate
Description : Module containing basic data types.
Maintainer  : argh atch esss culling@pm.com
Stability   : Experimental
License     : MIT

    This project has two primary data types:

        Prop(ositions)
        T(yped)terms
    
    Defined in src/Data/Propterms.hs
    
    They are packaged into two higher order
    data types for structuring proofs: 

        ProofState  - Single step in proof
        Proof       - List of ProofStates

    We employ two type synonyms for readability:

        Goal        - Single prop
        Context     - List of Tterms

-}

module Data.Proofstate where

    import Data.Propterms

    type Goal    = Prop
    type Context = [Tterm]

    prettyContext :: Context -> String
    prettyContext []        = "\n"
    prettyContext (t : ts) = show t ++ "\n" ++ prettyContext ts

    -- This data is to label steps in a proof. 
    -- To keep a record of the moves in a proof. 
    data Inference =
        Start
        | ImpElim
        | ImpIntro Char
        | QED

    -- Steps in proofs are wrapped up as follows:
    data ProofState =
        State Context Goal Inference
    
    -- This data structure needs destructors
    getContext :: ProofState -> Context
    getContext (State c _ _) = c

    getGoal :: ProofState -> Goal
    getGoal (State _ g _) = g

    getInference :: ProofState -> Inference
    getInference (State _ _ i) = i

    -- Pretty printing required for Show class. 
    prettyProofState :: ProofState -> String
    prettyProofState (State c g i) = 
        cstr ++ gstr 
        where
            cstr = prettyContext c
            gstr = "|-  " ++ show g
    
    instance Show ProofState where
        show :: ProofState -> String
        show = prettyProofState

    -- Proofs are lists of these states. 
    type Proof = [ProofState]
    -- Proofs are to be developed by updating 
    -- the state at the head of the list
    prettyProof :: Proof -> String
    prettyProof (x : xs) = prettyProofState x    

    displayProof :: Proof -> IO ()
    displayProof pf =
        do
            putStr $ prettyProof pf
            putStr "\n"

    -- Given a context and a goal one can 
    -- begin a proof!
    initium :: Context -> Goal -> Proof
    initium ctx gl = [begin]
        where
            begin :: ProofState
            begin = State ctx gl Start

