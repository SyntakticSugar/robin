{-|
Module      : Display
Description : User interface code.
Maintainer  : argh atch esss culling@pm.com
Stability   : Experimental
License     : MIT

    The user interface layout is very similar 
    that of  Coq and Agda.

    A proof in progress should look similar
    to the example below. 

    [  Robin :: Proof Assistant for STLC  ]
    [                                     ]
    [   Theorem: Context |- Goal n        ]
    [                                     ]
    [   h1 : T1                           ]
    [   h1 : T2                           ]
    [   h1 : T3                           ]
    [      :                              ]
    [   hi : Ti                           ]
    [                                     ]
    [-------------------------------------]
    [   |- P-> (Q -> P)                   ]
    [                                     ]
    [ [:H]elp [:Q]uit                     ]
    [                                     ]
    [    >> Input Instructions            ]


    Display components
    *   Title
    *   Original context and goal
    *   Live hypotheses [displayProof]
    *   Dividing line   [displayProof]
    *   Current goal    [displayProof]
    *   Help line
    -   User input
   
-}

module Interface.Display where

    import Data.Proofstate
    import Data.Propterms
    import Example.Examples
    import Tactic.Tactics

    title :: String
    title = "\nRobin :: Proof Assistant for the Simply Typed Lambda Calculus \n\n"

    prettySequent :: ProofState -> String 
    prettySequent (State c g i) =
        context ++ " |- " ++ show g
        where
            context = prettyContextLine c
    
    displayProof :: Proof -> IO ()
    displayProof pf =
        do
            putStr $ prettyProof pf
            putStr "\n"
            
    -- Buttons for the menu on display.
    type Button = String
    
    help :: Button
    help    = "[:H]elp "
    
    tactics :: Button
    tactics = "[:T]actics "
    
    quit :: Button
    quit     = "[:Q]uit "

    menu :: String
    menu = help ++ tactics ++ quit

    -- Display the entire current proofstate.
    display :: Proof -> IO ()
    display pf =
        do
            -- Reset terminal.
            putStr "\ESC[2J" 
            putStr "\ESC[H"

            -- Construct the display.
            putStr title
            putStr $ "Theorem: " ++ prettySequent (last pf)
            putStr "\n\n\n"
            displayProof pf
            putStr menu
            putStr "\n"

    