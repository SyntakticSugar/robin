{-
Module      : Robin
Description : Proof Assistant for STLC=IPL
Maintainer  : argh atch esss culling@pm.com
Stability   : Experimental
License     : MIT

    Robin is a proof assistant for the simply 
    typed lambda calculus. Equivalently, Robin
    is a proof assistant for propositional 
    logic with the single connective -> 

    Haskell B. Curry first observed this 
    equivalence in the 1930s in terms of 
    Hilbert style proofs and combinator 
    calculus. 

    William Howard later extended this 
    observation using Gentzen's natural 
    deductions and more expressive types. 
    Robin is modeled more closely on the 
    Gentzen style, where rules of inference 
    have be translated into tactics. 

    ----------------------
    -- How to use Robin --
    ----------------------

    [ ] :: Specify the context
    [ ] :: Specify the goal
    [ ] :: Write your proof!

    As this logic has only one connective
    there are only two tactics: 

        Intro
        Apply
    
    Intro :: this moves the outer antecedent
             in the goal to the hypotheses

        [!] Will fail if goal not an -> 
    
    Apply :: this combines two hypotheses 
             to generate a new hypothesis
        
        [!] Will fail if types of the 
            combined terms don't match

-}

module Robin where

    import Data.Propterms
    import Tactic.Tactics
    import Data.Proofstate
    import Example.Examples
    import Interface.Parser
    import Interface.Display

    ctxt :: Context
    ctxt = []

    goal :: Goal
    goal = swap

    proof :: Proof
    proof = initium ctxt goal

    main :: Proof -> IO ()
    main pf =
        do
            display pf
            input <- getLine
            main $ inputTactic input pf