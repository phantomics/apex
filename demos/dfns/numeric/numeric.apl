⍝⍝ Ported from http://dfns.dyalog.com/n_contents.htm into April APL


⍝⍝ Natural number processing

⍝ From http://dfns.dyalog.com/c_adic.htm

adic ← { ⎕IO←0                               ⍝ Bijective base-⍺ numeration.
  b←⍬⍴⍴a←,⍺                                  ⍝ base and alphabet
  $[1=⍴⍴⍵;b⊥1+a⍳⍵;                           ⍝ vector: decode digs → number, scalar: encode number → digs
    1=b;⍵/⍺;                                 ⍝ unary: special case
    n←⌊b⍟1+⍵×b-1                             ⍝ number of digits
    z←(¯1+b*n)÷b-1                           ⍝ smallest integer with length n
    a[(n/b)⊤⍵-z]                             ⍝ digits from alphabet ⍺.
   ]
}

⍝ From http://dfns.dyalog.com/c_apportion.htm

apportion ← {                                ⍝ Huntington-Hill apportionment.
  ⍺←435                                      ⍝ default number of seats
  ⍵{                                         ⍝ population per state
    d←(⍵×⍵+1)*0.5                            ⍝ divisor
    cs←⍺÷d                                   ⍝ priority value
    ⍵+cs=⌈/cs                                ⍝ next seat allocation
  }⍣(⍺-≢⍵),1                                 ⍝ iterated per remaining seat.
}

⍝ From http://dfns.dyalog.com/c_bsearch.htm

bsearch ← {                                  ⍝ Binary search: least n in range ⍵ with ⍺⍺ n.
  $[¯1≤-/⍵;1↑⍵+2-+/⍺⍺¨⍵;                     ⍝ convergence: finished.
    mid←⌈0.5×+/⍵                             ⍝ Mid point:
    $[⍺⍺ mid;∇(1↑⍵),mid;                     ⍝ 1: search lower half.
      ∇ mid,1↓⍵                              ⍝ 0: search upper half.
   ]]
}

⍝ From http://dfns.dyalog.com/c_cfract.htm
  
cfract ← {                                   ⍝ Continued fraction approximation of real ⍵.
  ⍺←⎕CT ⋄ ⎕CT←⍺                              ⍝ default comparison tolerance.
  ,↑{                                        ⍝ cf from rational ⍺÷⍵:
    $[⍵=1;⍺;                                 ⍝ whole number: finished.
      n r←0 ⍵⊤⍺                              ⍝ next term and remainder.
      n,⍵ ∇ r                                ⍝ next term and cf of remainder.
     ]
  }/⌊(1000×⎕CT)+⍵ 1÷1∨⍵                      ⍝ whole number ratio.
} ⍝ TODO: the 1000×⎕CT should not be necessary
  
⍝ From http://dfns.dyalog.com/c_colsum.htm
  
colsum ← {                                   ⍝ Sum of (default decimal) columns.
  ⍺←10 ⋄ ⍺{{(0=⍬⍴⍵)↓⍵}+⌿1 0⌽0,0 ⍺⊤⍵}⍣≡+⌿⍵    ⍝ repeat while overflow.
}

⍝ From http://dfns.dyalog.com/c_efract.htm

efract ← {                                   ⍝ Egyptian fractions: Fibonacci-Sylvester algorithm
  ⍬{
    p q←⍵÷∨/⍵
    $[p=1;⍺,q;r←p|q ⋄ s←(q-r)÷p
      (⍺,s+1)∇(p-r)(q×s+1)
     ]
  }⍺ ⍵
}

⍝ From http://dfns.dyalog.com/c_factorial.htm
  
factorial ← { ⍺←1 ⋄ $[⍵=0;⍺;(⍺×⍵)∇ ⍵-1] }    ⍝ Tail recursive factorial.

⍝ From http://dfns.dyalog.com/c_fibonacci.htm

fibonacci ← {                                ⍝ Tail-recursive Fibonacci.
  ⍺←0 1 ⋄ $[⍵=0;⍬⍴⍺;(1↓⍺,+/⍺)∇ ⍵-1]
}

sulFib ← {                                   ⍝ Sullivan Fibonacci
  z←0.5×1+s←5*0.5
  ((z*⍵)-(2○○⍵)×z*-⍵)÷s
}

⍝ From http://dfns.dyalog.com/c_factors.htm

factors ← {                                  ⍝ Prime factors of ⍵.
  ⍵{                                         ⍝ note: ⎕wa>(⍵*÷2)×2*4.
    ⍵,(⍺÷×/⍵)~1                              ⍝ append factor > sqrt(⍵).
  }∊⍵{                                       ⍝ concatenated,
    (0=(⍵*⍳⌊⍵⍟⍺)|⍺)/⍵                        ⍝ powers of each prime factor.
  }¨⍬{                                       ⍝ remove multiples:
    nxt←⊃⍵                                   ⍝ next prime, and
    msk←0≠nxt|⍵                              ⍝ ... mask of non-multiples.
    $[∧/1↓msk;⍺,⍵;                           ⍝ all non multiples - finished.
      (⍺,nxt)∇ msk/⍵                         ⍝ sieve remainder.
     ]
  }⍵{                                        ⍝ from,
    (0=⍵|⍺)/⍵                                ⍝ divisors of ⍵ in:
  }2,(1+2×⍳⌊0.5×⍵*÷2),⍵                      ⍝ 2,3 5 .. sqrt(⍵),⍵
}

⍝ From http://dfns.dyalog.com/c_gcd.htm

gcd ← { $[⍵=0;|⍺;⍵ ∇ ⍵|⍺] }                  ⍝ Greatest common divisor.

lcm ← { ⍺×⍵÷⍺ gcd ⍵ }                        ⍝ Least common multiple.

⍝ From http://dfns.dyalog.com/c_k6174.htm

kdeco←(4/10)∘⊤⍣¯1 ⍝ TODO: figure out why this is needed

k6174 ← {                                    ⍝ Kaprekar's operation.
  enco←(4/10)∘⊤                              ⍝ 4-digit encode.
  deco←enco⍣¯1                               ⍝   and decode.
  $[1=⍴∪enco ⍵;'error';                      ⍝ all digits the same: no go.
    ⍬{                                       ⍝ starting with null sequence.
      $[⍵=⊃⌽⍺;⍺;                             ⍝ repeated items: done.
        v←{⍵[⍒⍵]}enco ⍵                      ⍝ digits in descending order.
        (⍺,⍵)∇(kdeco v)-kdeco⌽v              ⍝ smaller to larger difference.
       ]
     }⍵                                      ⍝ :: [#] ∇ # → [#]
   ]
}
  
⍝ From http://dfns.dyalog.com/c_int.htm

int ← { ↑⍵{(⍺|⍶+⍵)-⍵}/2*⍺-0 1 }              ⍝ Signed from unsigned integer.

⍝ From http://dfns.dyalog.com/c_uns.htm

uns ← { (2*⍺)|⍵ }                            ⍝ Unsigned from signed integer.

⍝ From http://dfns.dyalog.com/c_nicediv.htm

nicediv ← {                                  ⍝ ⍵ similar integers with sum ⍺.
  q←⍵⍴⌊⍺÷⍵                                   ⍝ quotient.
  d←+\(⍵|⍺)÷⍵⍴⍵                              ⍝ residue spread in ⍵ decimal steps.
  i←2</0,⌊0.5+d                              ⍝ residue spread in ⍵ integer steps.
  q+i
}

⍝ From http://dfns.dyalog.com/s_nicediv.htm

stack ← { ⍉↑( ⍺ nicediv ⍵)/¨'⎕' }

osc ← { $[1=⍵;1;2|⍵;∇ 1+3×⍵;∇ ⍵÷2] }         ⍝ Oscillate - probably returns 1.

⍝ From http://dfns.dyalog.com/c_range.htm

range ← { (⍴⍵)⍴((⍴⍺)↓⍋⍋⍺,,⍵)-⍋⍋,⍵ }          ⍝ Numeric range classification.

⍝ From http://dfns.dyalog.com/c_rational.htm
  
rational ← {                                 ⍝ Rational approximation to real ⍵.
  ⍺←⎕CT ⋄ ⎕CT←⍺                              ⍝ default comparison tolerance.
  ↑⍵ 1÷⊂1∨⍵                                  ⍝ rational pair: ⍵≡÷⌿rational ⍵.
}

⍝ TODO: get roman numeral function in

⍝ From http://dfns.dyalog.com/c_stamps.htm

path ← {                                     ⍝ Shortest path from/to ⍵ in graph ⍺.
  graph (fm tto)←⍺ ⍵                         ⍝ graph and entry/exit vertex vectors
  fm {                                       ⍝ fm is the starting-from vertex
    $[⍺≡⍬;⍬;                                 ⍝ no vertices left: no path
      $[∨/tto∊⍺;                             ⍝ found target: path from tree:
        ⍬(⊃∘⍵) { $[⍵<0;⍺;                    ⍝ root: finished
                   (⍵,⍺) ∇ ⍺⍺ ⍵]             ⍝ accumulated path to next vertex
               } 1↑⍺∩tto;                    ⍝ found vertex ⍺
        next←graph[⍺]∩¨⊂⍸⍵=¯2                ⍝ next vertices to visit
        back←⊃,/⍺+0×next                     ⍝ back links
        wave←⊃,/next                         ⍝ vertex wave front
        (∪wave) ∇ back@wave⊢⍵]]              ⍝ advanced wave front
  }¯2+(⍳⍴⍺)∊fm                               ⍝ null spanning tree
}

stamps ← {                                   ⍝ Postage stamps to the value of ⍵.
  ⍺←1 5 6 10 26 39 43                        ⍝ Default UK stamp denominations.
  graph←⍺{⍵∘∩¨⍵+⊂⍺}⍳⍵+|⌊/⍺                   ⍝ values: 0 ·· ⍵.
  spath←graph path ⎕IO+0 ⍵                   ⍝ shortest path 0→⍵.
  ¯2-/spath                                  ⍝ best-fit stamps.
}

⍝ From http://dfns.dyalog.com/c_sieve.htm

sieve ← {                                    ⍝ Sieve of Eratosthenes.
  ⍺←⍬                                        ⍝ Default no primes yet.
  nxt←1↑⍵                                    ⍝ Next prime, and
  msk←0≠nxt|⍵                                ⍝ ... mask of non-multiples.
  $[∧/1↓msk;⍺,⍵;                             ⍝ All non multiples - finished.
    (⍺,nxt)∇ msk/⍵                           ⍝ Sieve remainder.
   ]
}

⍝ From http://dfns.dyalog.com/c_to.htm

to ← { ⎕IO←0                                 ⍝ Sequence ⍺ .. ⍵
  from step←1 ¯1×-\2↑⍺,⍺+×⍵-⍺                ⍝ step default is +/- 1.
  from+step×⍳0⌈1+⌊(⍵-from)÷step+step=0       ⍝ ⍺ thru ⍵ inclusive.
}

  
⍝⍝ Real number processing

⍝ From http://dfns.dyalog.com/c_alt.htm
alt ← {                                      ⍝ Alternant.
  r c←⍴⍵                                     ⍝ matrix ⍵
  $[0=r;⍵⍵⌿,⍵;                               ⍝ zero-row case
    1≥c;⍺⍺⌿,⍵;                               ⍝ zero/one-column case
    M←~⍤1 0⍨⍳r                               ⍝ minors
    ⍵[;⎕IO]⍺⍺.⍵⍵(∇⍤2)⍵[M;1↓⍳c]
   ]
}

bayes ← { ⍺(×÷+.×)⍵ }                        ⍝ Bayes' formula. (implemented as a fork)

⍝ From http://dfns.dyalog.com/c_Cholesky.htm

Cholesky ← {                                 ⍝ decomposition of a Hermitian positive-definite matrix.
  $[1≥n←≢⍵;⍵*0.5;
    p←⌈n÷2 ⋄ q←⌊n÷2
    X←(p,p)↑⍵⊣Y←(p,-q)↑⍵⊣Z←(-q,q)↑⍵
    L0←∇ X ⋄ L1←∇ Z-(T←(+⍉Y)+.×⌹X)+.×Y
    ((p,n)↑L0)⍪(T+.×L0),L1
  ]
}

⍝ From http://dfns.dyalog.com/c_det.htm

det ← { ⎕IO←0                                ⍝ Determinant of square matrix.
  ⍺←1                                        ⍝ product of co-factor coefficients so far
  $[0=n←≢⍵;⍺;                                ⍝ result for 0-by-0
    i j←(⍴⍵)⊤{⍵⍳⌈/⍵}|,⍵                      ⍝ row and column index of maximal element
    k←⍳n ⋄ (⍺×⍵[i;j]×¯1*i+j)∇ ⍵[k~i;k~j]-⍵[k~i;j]∘.×⍵[i;k~j]÷⍵[i;j]
  ]
}

⍝ From http://dfns.dyalog.com/c_kcell.htm

kcell ← {                                    ⍝ Relationship between point and k-cell.
  ⍺←(≢⍵)/2 1⍴0 1                             ⍝ Default is unit k-cell.
  b←,[(2=⍴⍴⍺)/⎕IO]⍺                          ⍝ Bounds of k-cell.
  p←((2⌊⍴⍴⍺)↓1 1,⍴⍵)⍴⍵                       ⍝ Points to evaluate.
  d←↑,¨⍺⍺⌿×-b,.-p                            ⍝ Apply operand to signum difference.
  $[⍺⍺/⍬;⌈/d;5⊥⍉d]                           ⍝ Result is {¯1,0,1} or integer.
}

⍝ From http://dfns.dyalog.com/c_kball.htm

kball ← {                                    ⍝ Relationship between point and k-ball.
  ⍺←1
  r←⊃⍺ ⋄ p←1/⍵                               ⍝ Default is ball w/radius 1 at origin.
  c←(≢p)↑1↓⍺                                 ⍝ Remaining coordinates are center.
  ×↑-/(⍉p-[⎕IO]c)r+.*¨2                      ⍝ Perform signum difference.
}

⍝ From http://dfns.dyalog.com/c_ksphere.htm

ksphere ← {                                  ⍝ Surface area of k-sphere.
  n←⍺+1                                      ⍝ dimension of enclosed k-ball.
  pi←(○1)*n÷2                                ⍝ power of pi.
  n×(⍵*⍺)×pi÷!n÷2                            ⍝ k-sphere surface area.
}

⍝ From http://dfns.dyalog.com/s_ksphere.htm

kvol ← { ⍵×((⍺-1) ksphere ⍵)÷⍺ }

⍝ From http://dfns.dyalog.com/c_mean.htm

mean ← { sum←+/⍵ ⋄ num←⍴⍵ ⋄ sum÷num }        ⍝ Arithmetic mean.

⍝ From http://dfns.dyalog.com/s_mean.htm

stdev ← {
  square←*∘2 ⋄ sqrt←*∘0.5
  sqrt(mean square ⍵)-square mean ⍵
}

⍝ From http://dfns.dyalog.com/c_NormRand.htm

NormRand ← {                                 ⍝ Random numbers with a normal distribution
  depth←10*9                                 ⍝ randomness depth - can be larger from v14.0
  x y←⊂[1+⍳⍴,⍵](?(2,⍵)⍴depth)÷depth          ⍝ two random variables within ]0;1]
  ⍝ TODO: (x y) causes a bug in the line above
  ((¯2×⍟x)*0.5)×1○○2×y                       ⍝ Box-Muller distribution
}

⍝ Get the phinary function in when possible, it's big
  
⍝ From http://dfns.dyalog.com/c_root.htm

root ← { ⍺←2 ⋄ ⍵*÷⍺ }                        ⍝ ⍺'th root, default to sqrt.

⍝ From http://dfns.dyalog.com/c_roots.htm

realroots ← {                                ⍝ Real roots of quadratic.
  a b c←⍵                                    ⍝ Coefficients.
  d←(b*2)-4×a×c                              ⍝ Discriminant.
  $[d<0;⍬;                                   ⍝ No roots
    d=0;-b÷2×a;                              ⍝ One root
    d>0;(-b+¯1 1×d*0.5)÷2×a                  ⍝ Two roots
   ]
}

⍝ From http://dfns.dyalog.com/s_roots.htm
  
roots ← {                                    ⍝ Roots of quadratic.
  a b c←⍵                                    ⍝ coefficients.
  d←(b*2)-4×a×c                              ⍝ discriminant.
  (-b+¯1 1×d*0.5)÷2×a                        ⍝ both roots.
}


⍝⍝ Complex number processing
  
⍝ From http://dfns.dyalog.com/c_polar.htm

polar ← {                                    ⍝ Polar from/to cartesian coordinates.
  lam←,[⎕IO-÷2]                              ⍝ laminate along new first axis.
  pol_car←{                                  ⍝ polar from cartesian (default).
    radius←{(+⌿⍵*2)*0.5}                     ⍝ radius (pythagorus).
    angle←{                                  ⍝ phase angle.
      x y←⊂⍤¯1⊢⍵                             ⍝ x and y coordinates.
      x0 xn←1 0=⊂0=x                         ⍝ points on/off y axis.
      atan←¯3○y÷x+x0                         ⍝ arctan y÷x (avoiding y÷0).
      qne←(xn×atan)+x0×○0.5×2-×y             ⍝ NE quadrant.
      nsw←○x<0                               ⍝ NW and SW quadrants.
      qse←○2×(x>0)∧y<0                       ⍝ SE quadrant.
      nsw+qse+qne                            ⍝ all quadrants.
    }
    (radius ⍵)lam angle ⍵                    ⍝ (2,···) array of polar coordinates.
  }

  car_pol←{                                  ⍝ cartesian from polar.
    r o←⊂⍤¯1⊢⍵                               ⍝ radius and phase angle.
    (r×2○o)lam r×1○o                         ⍝ r×cos(ø), r×sin(ø).
  }

  ⍺←1                                        ⍝ default polar from cartesian.
  $[⍺=+1;pol_car ⍵;                          ⍝ polar from cartesian.
    ⍺=-1;car_pol ⍵                           ⍝ cartesian from polar.
   ]
}

⍝ From http://dfns.dyalog.com/s_polar.htm

rnd ← { (10*-⍺)×⌊0.5+⍵×10*⍺ }

poly ← { 2 1∘.○(○2÷⍵)×(⍳⍵)-⍳1 }

⍝ From http://dfns.dyalog.com/c_xtimes.htm

xtimes ← { ⎕IO←0                             ⍝ Fast multi-digit product using FFT.
  xroots    ← {×\1,1↓(⍵÷2)⍴¯1*2÷⍵}
  cube      ← {⍵⍴⍨2⍴⍨⌊2⍟⍴⍵}
  extend    ← {(2*⌈2⍟¯1+(⍴⍺)+⍴⍵)↑¨⍺ ⍵}
  floop     ← {(⊣/⍺)∇⍣(×m)⊢(+⌿⍵),[m-0.5]⍺×[⍳m←≢⍴⍺]-⌿⍵}
  FFT       ← {,(cube xroots⍴⍵)floop cube ⍵}
  iFFT      ← {(⍴⍵)÷⍨,(cube+xroots⍴⍵)floop cube ⍵}
  rconvolve ← {(¯1+(⍴⍺)+⍴⍵)↑iFFT⊃×/FFT¨⍺ extend ⍵}
  carry     ← {1↓+⌿1 0⌽0,0 10⊤⍵}
  (+/∧\0=t)↓t←carry⍣≡0,⌊0.5+9○⍺ rconvolve ⍵
}

convolve ← { ⎕IO←0 ⋄ +⌿(-⍳⍴⍺)⌽⍺∘.×⍵,0×1↓⍺ }

⍝ From http://dfns.dyalog.com/c_xpower.htm

xpower ← {                                   ⍝ Fast multi-digit power using FFT.
  xt←{(0,⍺)xtimes 0,⍵} ⋄ b←⌽2⊥⍣¯1+10⊥⍵       ⍝ boolean showing which powers needed
  ↑,/xt/b/{xt⍨⍵}\(⊂,10⊥⍣¯1+⍺)⍴⍨⍴b
}
