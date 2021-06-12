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

⍝ From http://dfns.dyalog.com/c_acc.htm

acc ← {                                     ⍝ Accumulating reduction.
  op←⍺⍺
  ⊃op{(⊂⍺ op⊃⍬⍴⍵),⍵}/1↓{⍵,⊂⍬⍴⍵}¯1⌽⍵
}

⍝ From http://dfns.dyalog.com/c_enlist.htm

enlist ← {                                  ⍝ List ⍺-leaves of nested array.
  $[⍺≥¯1+|≡⍵;,⍵;                            ⍝ all shallow leaves: finished.
    1↓↑,/(⊂⊂⊃⊃⍵),⍺ ∇¨,⍵                     ⍝ otherwise: concatenate sublists.
   ]
}

⍝ From http://dfns.dyalog.com/n_foldl.htm

foldl ← { ↑⍺⍺⍨/(⌽⍵),⊂⍺ }                    ⍝ Fold (reduce) from the left.

⍝ From http://dfns.dyalog.com/n_nlines.htm

nlines ← {                                  ⍝ Number of display lines for simple array.
  {                       
    (×/⍵)+{                                 ⍝ # of lines of real data +
      +/+\0⌈⍵-1,¯1↓⍵                        ⍝ # of blank lines separating different planes
    }×\¯1↓⍵                                 ⍝ of the array
  }¯1↓⍴⍵                                    ⍝ last axis affects only width of display.
}

⍝ From http://dfns.dyalog.com/s_perv.htm

perv ← {                                    ⍝ Scalar pervasion
  ⍺←⊢                                       ⍝ (⍺ and) ⍵ depth 0: operand fn application
  $[1=≡⍺ ⍵ ⍵;⍺ ⍺⍺ ⍵;⍺ ∇¨⍵]                  ⍝ (⍺ or) ⍵ deeper: recursive traversal.
}

⍝ From http://dfns.dyalog.com/c_pmat.htm

pmat ← {                                    ⍝ Permutation matrix of ⍳⍵.
  {$[1≥⍴⍵;↑,↓⍵;↑⍪/⍵,∘∇¨⍵∘~¨⍵]}⍳⍵            ⍝ short vector: done, else items prefix sub-perms of remainder.
}                                           ⍝ permutations of identity perm.

⍝ From http://dfns.dyalog.com/c_pred.htm

pred ← { ↑⍺⍺/¨(⍺/⍳⍴⍺)⊆⍵ }                   ⍝ Partitioned reduction.

