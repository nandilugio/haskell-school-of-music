import Euterpea

t251 :: Music Pitch
t251 =
  let
    dMinor = d 4 qn :=: f 4 qn :=: a 4 qn
    gMajor = g 4 qn :=: b 4 qn :=: d 4 qn
    cMajor = c 4 qn :=: e 4 qn :=: g 4 qn
  in
    dMinor :+: gMajor :+: cMajor

--m = t251

-- Excercise 2.1 ---------------------------------------------------------------

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne pit dur =
  let
    one  = transpose 0 (note dur pit) :=: transpose 4  (note dur pit) :=: transpose 7  (note dur pit)
    two  = transpose 2 (note dur pit) :=: transpose 6  (note dur pit) :=: transpose 9  (note dur pit)
    five = transpose 7 (note dur pit) :=: transpose 11 (note dur pit) :=: transpose 14 (note dur pit)
  in
    two :+: five :+: one

--m = twoFiveOne (G, 2) qn

-- Excercise 2.2 ---------------------------------------------------------------

data BluesPitchClass = Ro | MT | Fo | Fi | MS
type BluesPitch = (BluesPitchClass, Octave)

pitchClassFromBlues :: BluesPitchClass -> PitchClass
pitchClassFromBlues Ro = C
pitchClassFromBlues MT = Ef
pitchClassFromBlues Fo = F
pitchClassFromBlues Fi = G
pitchClassFromBlues MS = Bf

fromBlues :: Music BluesPitch -> Music Pitch
fromBlues (Prim (Note d (bpc, o))) = (Prim (Note d (pitchClassFromBlues bpc, o)))
fromBlues (Prim (Rest d)) = (Prim (Rest d))
fromBlues (m1 :+: m2) = (fromBlues m1 :+: fromBlues m2)
fromBlues (m1 :=: m2) = (fromBlues m1 :=: fromBlues m2)
fromBlues (Modify c m) = (Modify c (fromBlues m))

bluesMelody = 
  let
    riff = note en (Ro, 4) :+: note en (MT, 4) :+: note en (Fo, 4) :+: note en (Fi, 4)
       :+: note qn (MT, 4)                     :+: note en (MS, 3) :+: note en (Ro, 4)
    bass = note hn (Ro, 2) :+: note dqn (MS, 1) :+: note en (Ro, 2)
  in
    bass :=: riff

--m = fromBlues(bluesMelody)

-- Exercise 2.5 ----------------------------------------------------------------

transM :: AbsPitch -> Music Pitch -> Music Pitch
transM tp (Prim (Note d p)) = (Prim (Note d (trans tp p)))
transM tp (Prim (Rest d)) = (Prim (Rest d))
transM tp (m1 :+: m2) = (transM tp m1 :+: transM tp m2)
transM tp (m1 :=: m2) = (transM tp m1 :=: transM tp m2)
transM tp (Modify c m) = (Modify c (transM tp m))

music = fromBlues(bluesMelody)
--m = music :+: (transM 3 music)

-- Exercise 3.1.3

f3Staccato :: [Music Pitch] -> [Music Pitch]
f3Staccato notes =
  let staccateNote (Prim (Note d p)) = note (d/2) p :+: rest (d/2)
  in map staccateNote notes

c7Notes = [c 4 en, e 4 en, g 4 en, bf 4 en]
exerciseResult = f3Staccato c7Notes

-- folds haven't been introduced but.. to test ;p
c7ArpeggioStaccato = foldr (:+:) (rest 0) exerciseResult
c7Arpeggio         = foldr (:+:) (rest 0) c7Notes
m = c7Arpeggio :+: c7ArpeggioStaccato

-- TODO:
-- `f3Staccato` assumes it's 1st arg elements are of one particular case of
-- `Music Pitch`, a leaf node `(Prim (Note Dur Pitch))`.
-- - Doesn't GHC check that the rest of the cases aren't being handled (like Elm)?
-- - Can we restrict to that case the input type? Tried
--     `f3Staccato :: [Primitive Pitch] -> [Music Pitch]`
--   and some variants but couldn't make it work.

main =
  playDev 5 m
