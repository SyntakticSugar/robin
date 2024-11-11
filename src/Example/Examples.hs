{-
Module      : Example library
Description : Examples for testing the application
Maintainer  : argh atch esss culling@pm.com
Stability   : Experimental
License     : MIT

    Simple library of propositional varibales 
    and some familiar combinators. Here for 
    the purposes of testing the parts of the 
    application.  

-}

module Example.Examples where

    import Data.Proofstate
    import Data.Propterms
    import Tactic.Tactics
    
    p :: Prop
    p = Varp 'P'

    np :: Prop
    np = neg p 

    q :: Prop
    q = Varp 'Q'

    nq :: Prop
    nq = neg q

    r :: Prop
    r = Varp 'R'

    nr :: Prop
    nr = neg r

    i :: Prop
    i = Imp p p

    k :: Prop
    k = Imp p (Imp q p)

    b :: Prop
    b = Imp (Imp p q)
            (Imp (Imp q r)
                 (Imp p r))
    
    swap :: Prop
    swap = Imp (Imp p (Imp q r))
               (Imp q (Imp p r))

    example :: Proof
    example = initium [] swap

    hp :: Tterm
    hp = Vart "hp" p

    hq :: Tterm 
    hq = Vart "hq" q

    hr :: Tterm 
    hr = Vart "hr" r

    hi :: Tterm
    hi = Vart "hi" i

    hk :: Tterm
    hk = Vart "hk" k

    comp :: Tterm
    comp = Vart "comp" b

    contest :: Context
    contest = [hp, hq, hr, hi, hk]