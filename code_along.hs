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

-- Excercise 2.1
twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne pit dur =
  let
    one  = transpose 0 (note dur pit) :=: transpose 4  (note dur pit) :=: transpose 7  (note dur pit)
    two  = transpose 2 (note dur pit) :=: transpose 6  (note dur pit) :=: transpose 9  (note dur pit)
    five = transpose 7 (note dur pit) :=: transpose 11 (note dur pit) :=: transpose 14 (note dur pit)
  in
    two :+: five :+: one
--m = twoFiveOne (G, 2) qn

-- Excercise 2.2
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

m = fromBlues(bluesMelody)

main =
  playDev 5 m
