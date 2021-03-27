import Euterpea

t251 :: Music Pitch
t251 =
  let
    dMinor = d 4 qn :=: f 4 qn :=: a 4 qn
    gMajor = g 4 qn :=: b 4 qn :=: d 4 qn
    cMajor = c 4 qn :=: e 4 qn :=: g 4 qn
  in
    dMinor :+: gMajor :+: cMajor

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne pit dur =
  let
    one  = transpose 0 (note dur pit) :=: transpose 4  (note dur pit) :=: transpose 7  (note dur pit)
    two  = transpose 2 (note dur pit) :=: transpose 6  (note dur pit) :=: transpose 9  (note dur pit)
    five = transpose 7 (note dur pit) :=: transpose 11 (note dur pit) :=: transpose 14 (note dur pit)
  in
    two :+: five :+: one
  


main =
  playDev 5 (twoFiveOne (C, 4) qn)
