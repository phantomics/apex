⍝ Ported from http://dfns.dyalog.com/n_contents.htm into April APL

⍝ From http://dfns.dyalog.com/n_alists.htm

alpush ← {                                   ⍝ Association list ⍺ prefixed with (key value) pair ⍵.
  ⍺,⍨∘⊂¨⍵                                    ⍝ :: list ← list ∇ key val
}

alpop ← {                                    ⍝ Leftmost value for key ⍵ from list ⍺
  keys vals←⍺                                ⍝ keys vector and corresponding values
  indx←keys⍳⊂⍵                               ⍝ index of first key ⍵ in list ⍺
  val←indx⊃vals                              ⍝ value for key ⍵
  list←(⊂indx≠⍳⍴keys)/¨⍺                     ⍝ reduced list
  val list                                   ⍝ :: val list ← list ∇ key
}

alget ← {                                    ⍝ Value for key ⍵ in association list ⍺.
  keys vals←⍺                                ⍝ keys vector and corresponding values
  (keys⍳⊂⍵)⊃vals                             ⍝ :: val ← list ∇ key
}

alset ← {                                    ⍝ Assoc list ⍺ with (key value) pair ⍵ replaced.
  key val←⍵                                  ⍝ key and new value
  {(⊂val)@(⍺⍳⊂key)⊢⍵}\⍺                      ⍝ :: list ← list ∇ key value
}

⍝ From http://dfns.dyalog.com/c_acc.htm

acc ← { ⊃⍺⍺{(⊂⍺ ⍺⍺⊃⍬⍴⍵),⍵}/1↓{⍵,⊂⍬⍴⍵}¯1⌽⍵ }  ⍝ Accumulating reduction.

⍝ From http://dfns.dyalog.com/c_display.htm

display ← { ⎕IO←0                            ⍝ Boxed display of array.
  box←{                                      ⍝ box with type and axes
    vrt hrz←(¯1+⍴⍵)⍴¨'│─'                    ⍝ vert. and horiz. lines
    top←'─⊖→'[¯1↑⍺],hrz                      ⍝ upper border with axis
    bot←(⊃⍺),hrz                             ⍝ lower border with type
    rgt←'┐│',vrt,'┘'                         ⍝ right side with corners
    lax←'│⌽↓'[¯1↓1↓⍺],¨⊂vrt                  ⍝ left side(s) with axes,
    lft←⍉'┌',(↑lax),'└'                      ⍝ ... and corners
    lft,(top⍪⍵⍪bot),rgt                      ⍝ fully boxed array
  }

  open←{(1⌈⍴⍵)⍴⍵}                            ⍝ exposure of null axes
  trim←{(~1 1⍷∧⌿⍵=' ')/⍵}                    ⍝ removal of extra blank cols
  char←{$[⍬≡⍴⍵;'─';(⊃⍵∊'¯',⎕D)⊃'#~']}∘⍕      ⍝ simple scalar type
  type←{{(1=⍴⍵)⊃'+'⍵}∪,char¨⍵}               ⍝ simple array type
  axes←{(-2⌈⍴⍴⍵)↑1+×⍴⍵}                      ⍝ array axis types
  deco←{⍺←type open ⍵ ⋄ ⍺,axes ⍵}            ⍝ type and axes vector
  ⍝ line←{(6≠10|⎕DR' '⍵)⊃' -'}               ⍝ underline for atom
  line←{(49≠⎕T 1⍴⍵)⊃' -'}                    ⍝ underline for atom

  {                                          ⍝ recursive boxing of arrays:
    $[0=≡⍵;' '⍪(open ⎕FMT ⍵)⍪line ⍵;         ⍝ simple scalar
      1=≡⍵;(deco ⍵)box open ⎕FMT open ⍵;     ⍝ simple array
      ('∊'deco ⍵)box trim ⎕FMT ∇¨open ⍵      ⍝ nested array
     ]
  }⍵
}

⍝ From http://dfns.dyalog.com/c_enlist.htm

enlist ← {                                   ⍝ List ⍺-leaves of nested array.
  $[⍺≥¯1+|≡⍵;,⍵;                             ⍝ all shallow leaves: finished.
    1↓↑,/(⊂⊂⊃⊃⍵),⍺ ∇¨,⍵                      ⍝ otherwise: concatenate sublists.
   ]
}

⍝ From http://dfns.dyalog.com/n_foldl.htm

foldl ← { ↑⍺⍺⍨/(⌽⍵),⊂⍺ }                     ⍝ Fold (reduce) from the left.

⍝ From http://dfns.dyalog.com/n_nlines.htm

nlines ← {                                   ⍝ Number of display lines for simple array.
  {                       
    (×/⍵)+{                                  ⍝ # of lines of real data +
      +/+\0⌈⍵-1,¯1↓⍵                         ⍝ # of blank lines separating different planes
    }×\¯1↓⍵                                  ⍝ of the array
  }¯1↓⍴⍵                                     ⍝ last axis affects only width of display.
}

⍝ From http://dfns.dyalog.com/s_perv.htm

perv ← {                                     ⍝ Scalar pervasion
  ⍝ ⍺←⊢                                      ⍝ (⍺ and) ⍵ depth 0: operand fn application
  $[1=≡⍺ ⍵ ⍵;⍺ ⍺⍺ ⍵;⍺ ∇¨⍵]                   ⍝ (⍺ or) ⍵ deeper: recursive traversal.
}

⍝ From http://dfns.dyalog.com/c_pmat.htm

pmat ← {                                     ⍝ Permutation matrix of ⍳⍵.
  {$[1≥⍴⍵;↑,↓⍵;↑⍪/⍵,∘∇¨⍵∘~¨⍵]}⍳⍵             ⍝ short vector: done, else items prefix sub-perms of remainder.
}                                            ⍝ permutations of identity perm.

⍝ From http://dfns.dyalog.com/c_pred.htm

pred ← { ↑⍺⍺/¨(⍺/⍳⍴⍺)⊆⍵ }                    ⍝ Partitioned reduction.

⍝ From http://dfns.dyalog.com/c_mscan.htm

mscan ← {                                    ⍝ Minus scan.
  ⍺←⍬⍴⌽⍳⍴⍴⍵                                  ⍝ ⍺ is axis (default last).
  +\[⍺]⍵×[⍺](⍺⊃⍴⍵)⍴1,-1
}

⍝ From http://dfns.dyalog.com/c_dscan.htm

dscan ← {                                    ⍝ Divide scan
  ⍺←⍬⍴⌽⍳⍴⍴⍵                                  ⍝ ⍺ is axis (default last).
  ×\[⍺]⍵*[⍺](⍺⊃⍴⍵)⍴1,-1
}

⍝ From http://dfns.dyalog.com/c_ascan.htm

ascan ← {                                    ⍝ Associative scan.
  $[2>0⊥⍴⍵;⍵;                                ⍝ few items: done.
    ⌽↑⍺⍺{(⊂(⊃⍵)⍺⍺ ⍺),⍵}/⌽(⊂∘⊃¨↓⍵),↑1↓¨↓⍵
   ]
}

⍝ From http://dfns.dyalog.com/c_ascana.htm

ascana ← {                                   ⍝ Higher rank asscociative scan.
  ⍺←⍬⍴⌽⍳⍴⍴⍵                                  ⍝ default last axis.
  ↑[⍺-0.1](⍺⍺ ascan)¨↓[⍺]⍵
}

⍝ From http://dfns.dyalog.com/c_select.htm

select ← { ⍺⊃¨↑,¨/⊂¨¨⍵ }                     ⍝ ⍺-selection of items of vector ⍵.

⍝ From http://dfns.dyalog.com/c_shannon.htm

shannon ← { -+/(2∘⍟×⊣)¨({≢⍵}⌸÷≢)⍵ }          ⍝ Shannon entropy of message ⍵.

⍝ From http://dfns.dyalog.com/c_subvec.htm

subvec ← {                                   ⍝ Is ⍺ a subvector of ⍵?
  $[0∊⍴⍺;1;0∊⍴⍵;0;(1↓⍺)∇(⍵⍳1↑⍺)↓⍵]           ⍝ null ⍺: success, null ⍵: failure.
}                                            ⍝ otherwise, check remaining items.
