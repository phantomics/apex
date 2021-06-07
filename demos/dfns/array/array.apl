⍝ Ported from http://dfns.dyalog.com/n_contents.htm into April APL

⍝ From http://dfns.dyalog.com/n_alists.htm

alpush ← {                                  ⍝ Association list ⍺ prefixed with (key value) pair ⍵.
  ⍺,⍨∘⊂¨⍵                                   ⍝ :: list ← list ∇ key val
}

alpop ← {                                   ⍝ Leftmost value for key ⍵ from list ⍺
  keys vals←⍺                               ⍝ keys vector and corresponding values
  indx←keys⍳⊂⍵                              ⍝ index of first key ⍵ in list ⍺
  val←indx⊃vals                             ⍝ value for key ⍵
  list←(⊂indx≠⍳⍴keys)/¨⍺                    ⍝ reduced list
  val list                                  ⍝ :: val list ← list ∇ key
}

alget ← {                                   ⍝ Value for key ⍵ in association list ⍺.
  keys vals←⍺                               ⍝ keys vector and corresponding values
  (keys⍳⊂⍵)⊃vals                            ⍝ :: val ← list ∇ key
}

alset ← {                                   ⍝ Assoc list ⍺ with (key value) pair ⍵ replaced.
  key val←⍵                                 ⍝ key and new value
  {(⊂val)@(⍺⍳⊂key)⊢⍵}\⍺                     ⍝ :: list ← list ∇ key value
}

⍝ acc ← {                                   ⍝ Accumulating reduction.
⍝   ⊃⍺⍺{(⊂⍺ ⍺⍺⊃⍬⍴⍵),⍵}/1↓{⍵,⊂⍬⍴⍵}¯1⌽⍵
⍝ }

⍝ From http://dfns.dyalog.com/c_enlist.htm

enlist ← {           ⍝ List ⍺-leaves of nested array.
  ⍺←0                 ⍝ default: list 0-leaves.
  $[⍺≥¯1+|≡⍵;,⍵;         ⍝ all shallow leaves: finished.
    1↓↑,/(⊂⊂⊃⊃⍵),⍺ ∇¨,⍵ ⍝ otherwise: concatenate sublists.
   ]
}

⍝ From http://dfns.dyalog.com/n_foldl.htm

foldl← {             ⍝ Fold (reduce) from the left.
  ↑⍺⍺⍨/(⌽⍵),⊂⍺
}
