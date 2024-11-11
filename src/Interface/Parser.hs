{-
Module      : Parser
Description : Code for parsing user instructions.
Maintainer  : argh atch esss culling@pm.com
Stability   : Experimental
License     : MIT

    Users will only have a small number 
    of valid input options. Made up of 
    tactics and a few other commands. 

    Tactics

        Intro
        Apply
    
    Extra commands

        :H for a help window
        :T for a tactic window
        :Q for closing the program
    
    -------
    TACTICS
    -------

    [ ] :: Intro

        This will be called by the user

            Intro <<string>>
    
    [ ] :: Apply

        This will be called by the user
            
            Apply <<string>> <<string>>

-}

module Interface.Parser where

    import Data.Propterms
    import Tactic.Tactics
    import Data.Proofstate
    import Example.Examples
    import Interface.Display

    inputToTactic :: String -> Tactic
    inputToTactic input 
        | cmd == "Intro" = intro term
        | cmd == "Apply" = identity
        | cmd == "QED"   = identity
        | otherwise      = identity
        where
            inputWords  = words input
            cmd         = head inputWords
            term        = tail inputWords
