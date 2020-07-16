-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Examples
import Control.Lens

--
-- Examples of interactions available with these Lenses
--

Ex1 : Maybe Int
Ex1 = over mapped (+1) (Just 2)

Ex1Proof : Ex1 = Just 3
Ex1Proof = Refl

Ex2 : (Int, Int)
Ex2 = (2,3) |> over both (+1)

Ex2Proof : Ex2 = (3, 4)
Ex2Proof = Refl

Ex3 : (String, String)
Ex3 = ("Hello",2) |> snd_ .~ "World!"

Ex3Proof : Ex3 = ("Hello","World!")
Ex3Proof = Refl

Ex4 : Either String Int
Ex4 = f (Left "hi")
  where f : Either String Int -> Either String Int
        f = over right_ (+1)

Ex4Proof : Ex4 = (Left "hi")
Ex4Proof = Refl

Ex5 : Either String Int
Ex5 = f (Right 4)
  where f : Either String Int -> Either String Int
        f = over right_ (+1)

Ex5Proof : Ex5 = (Right 5)
Ex5Proof = Refl

Ex6 : Either (String, Int) Int
Ex6 = f (Left ("hi", 2))
  where f : Either (String,Int) Int -> Either (String,Int) Int
        f = over (left_ . snd_) (+1)

Ex6Proof : Ex6 = Left ("hi", 3)
Ex6Proof = Refl

-- Ex7 and Ex8 need to use the PlusNatMonoid named instance and I don't know how to do this yet
[PlusNatSemi] Semigroup Nat where
  (<+>) x y = x + y

[PlusNatMonoid] Monoid Nat using PlusNatSemi where
  neutral = 0

[PlusNatApplicative] Applicative (Const Nat) using PlusNatMonoid where
  pure _                      = MkConst neutral
  (MkConst f) <*> (MkConst v) = MkConst (f <+> v)

-- f : Either (String, Nat) Int -> Nat
-- f = view @{PlusNatApplicative} (left_ . snd_)

-- Ex7 : Nat
-- Ex7 = f (Left ("hi", 2)) where

-- Ex7Proof : Ex7 = 2
-- Ex7Proof = Refl

-- Ex8 : Nat
-- Ex8 = f (Right 2) where
--   f : Either (String, Additive) Int -> Additive
--   f = view (left_ . snd_)

-- Ex8Proof : Ex8 = 0
-- Ex8Proof = Refl

Ex9 : Maybe String
Ex9 = g ^? right_ . fst_ where
 g : Either String (String, Int)
 g = Right ("x",2)

Ex9Proof : Ex9 = Just "x"
Ex9Proof = Refl

Ex10 : Maybe ()
Ex10 = Just "x" ^? nothing_

Ex10Proof : Ex10 = Nothing
Ex10Proof = Refl

Ex11 : Maybe ()
Ex11 = n ^? nothing_ where
  n : Maybe String
  n = Nothing

Ex11Proof : Ex11 = Just ()
Ex11Proof = Refl

Ex12 : Int
Ex12 = view (to fst) (1,2)

Ex12Proof : Ex12 = 1
Ex12Proof = Refl

Ex13 : Either Int String
Ex13 = the (Either Int String) $ left_ # 4

Ex13Proof : Ex13 = Left 4
Ex13Proof = Refl

Ex14 : Either Int String
Ex14 = the (Either Int String) $ 5 ^.re left_

Ex14Proof : Ex14 = Left 5
Ex14Proof = Refl

sansExample : Maybe Int
sansExample = sans () (Just 4)

fusingExample : Maybe (Int, Int)
fusingExample = Just (2,3) |> over ( fusing (just_ . both)) (+1)

confusingExample : Maybe (Int, Int)
confusingExample = Just (2,3) |> over ( confusing (just_ . both)) (+1)

-- --------------------------------------------------------------------- [ EOF ]
