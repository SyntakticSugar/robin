{-
Module      : Propositions and Typed Terms.
Description : Module containing basic data types.
Maintainer  : argh atch esss culling@pm.com
Stability   : Experimental
License     : MIT


    This project has two primary data types:

        Prop(ositions)
        T(yped)terms
    
    Propositions are for goals.
    Typed terms are for the context.


-}

module Data.Propterms where

    data Prop = 
        Falsum
        | Varp Char
        | Imp Prop Prop
        deriving Eq

    -- Negation is just syntactic sugar.
    neg :: Prop -> Prop
    neg p = Imp p Falsum

    prettyProp :: Prop -> String
    prettyProp Falsum = "#f"
    prettyProp (Varp c) = [c]
    prettyProp (Imp a c) = "(" ++ prettyProp a ++ " -> " 
                               ++ prettyProp c ++ ")"

    instance Show Prop where
        show :: Prop -> String
        show = prettyProp

    isImp :: Prop -> Bool
    isImp (Imp _ _) = True
    isImp _         = False

    data Tterm =
        Vart String Prop
        | Abs String Prop Tterm
        | App Tterm Tterm
        deriving Eq
    
    prettyTterm :: Tterm -> String
    prettyTterm (Vart c p) = c ++ " : " ++ show p
    prettyTterm (Abs c p t) = "lam " ++ c ++ show p ++ prettyTterm t
    prettyTterm (App t1 t2) = "(" ++ prettyTterm t1 ++") (" ++ prettyTterm t2 ++ ")"

    instance Show Tterm where
        show :: Tterm -> String
        show = prettyTterm
    
    -- Determine whether application is sound i.e. whether
    -- the antecedent matches the input type. 
    applySound :: Prop -> Prop -> Bool
    applySound (Imp ant con) p  = ant == p
    applySound _ _              = False
