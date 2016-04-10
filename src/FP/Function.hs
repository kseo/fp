module FP.Function where

import Data.List (transpose)
import FP.Value

tl :: Object -> Object
tl (SequenceObject [_]) = emptySeq
tl (SequenceObject (_:os)) = SequenceObject os
tl _ = Bottom

tlr :: Object -> Object
tlr (SequenceObject [_]) = emptySeq
tlr (SequenceObject os) = SequenceObject $ init os
tlr _ = Bottom

atom :: Object -> Object
atom (AtomObject _) = makeBool True
atom _ = makeBool False

eq :: Object -> Object
eq (SequenceObject [x,y]) = makeBool $ x == y
eq _ = makeBool False

null :: Object -> Object
null = makeBool . (emptySeq==)

reverse :: Object -> Object
reverse (SequenceObject os) = SequenceObject $ Prelude.reverse os
reverse _ = Bottom

distl :: Object -> Object
distl (SequenceObject [x, SequenceObject ys]) = SequenceObject $ zipWith (\x y -> SequenceObject [x, y]) (repeat x) ys
distl _ = Bottom

distr :: Object -> Object
distr (SequenceObject [SequenceObject xs, y]) = SequenceObject $ zipWith (\x y -> SequenceObject [x, y]) xs (repeat y)
distr _ = Bottom

length :: Object -> Object
length (SequenceObject os) = makeNumber $ fromIntegral $ Prelude.length os
length _ = Bottom

trans :: Object -> Object
trans (SequenceObject os) =
  let oss = fmap unwrap os
      oss' = transpose oss
   in wrap $ fmap wrap oss'
  where
    unwrap (SequenceObject os) = os
    wrap = SequenceObject
trans _ = Bottom

and :: Object -> Object
and (SequenceObject [AtomObject (BoolAtom x), AtomObject (BoolAtom y)]) = makeBool $ x && y
and _ = Bottom

or :: Object -> Object
or (SequenceObject [AtomObject (BoolAtom x), AtomObject (BoolAtom y)]) = makeBool $ x || y
or _ = Bottom

not :: Object -> Object
not (AtomObject (BoolAtom x)) = makeBool $ Prelude.not x
not _ = Bottom

apndl :: Object -> Object
apndl (SequenceObject [x, SequenceObject os]) = SequenceObject $ x:os
apndl _ = Bottom

apndr :: Object -> Object
apndr (SequenceObject [SequenceObject os, x]) = SequenceObject $ os ++ [x]
apndr _ = Bottom

rotl :: Object -> Object
rotl (SequenceObject (o:os)) = SequenceObject $ os ++ [o]
rotl _ = Bottom

rotr :: Object -> Object
rotr (SequenceObject os) =
    let lastIndex = Prelude.length os - 1
        (xs, ys) = splitAt lastIndex os
    in SequenceObject $ ys ++ xs
rotr _ = Bottom

select :: Object -> Object -> Object
select (AtomObject (NumberAtom index)) (SequenceObject os) =
  let seqLength = Prelude.length os
      intIndex = fromIntegral index
   in  if intIndex <= seqLength
          then os !! (intIndex - 1)
          else Bottom
s _ _ = Bottom

selectr :: Object -> Object -> Object
selectr (AtomObject (NumberAtom index)) (SequenceObject os) =
  let seqLength = Prelude.length os
      intIndex = fromIntegral index
   in  if intIndex <= seqLength
          then os !! (seqLength - intIndex)
          else Bottom
sr _ _ = Bottom

add :: Object -> Object
add (SequenceObject [AtomObject (NumberAtom x), AtomObject (NumberAtom y)]) = makeNumber $ x + y
add _ = Bottom

subtract :: Object -> Object
subtract (SequenceObject [AtomObject (NumberAtom x), AtomObject (NumberAtom y)]) = makeNumber $ x - y
subtract _ = Bottom

multiply :: Object -> Object
multiply (SequenceObject [AtomObject (NumberAtom x), AtomObject (NumberAtom y)]) = makeNumber $ x * y
multiply _ = Bottom

divide :: Object -> Object
divide (SequenceObject [AtomObject (NumberAtom x), AtomObject (NumberAtom y)]) =
    if y == 0
       then Bottom
       else makeNumber $ x `div` y
divide _ = Bottom
