⍝⍝ Ported from http://dfns.dyalog.com/n_Graphs.htm into April APL


⍝⍝ Graph processing

⍝ From http://dfns.dyalog.com/n_alists.htm

gperm ← { (⊂⍵)⍳¨⍺[⍵] }                       ⍝ ⍵-permutation of vertices of graph ⍺.

⍝ From http://dfns.dyalog.com/n_insnode.htm

insnode ← {                                  ⍝ Insert vertex ⍵ in graph ⍺.
  (⍵⌈⍴⍺)↑⍺,⍵⍴⊂⍬                              ⍝ extend graph with sufficient nulls.
}

⍝ From http://dfns.dyalog.com/n_remnode.htm

remnode ← {                                  ⍝ Remove vertex ⍵ from graph ⍺.
  new←(⍵≠⍳⍴⍺)/⍺~¨⍵                           ⍝ graph with vertex ⍵ removed,
  new-new>⍵                                  ⍝ and edges adjusted.
}

⍝ From http://dfns.dyalog.com/n_inslink.htm

inslink ← {                                  ⍝ Graph ⍺ with new edge ⍵.
  fm to←⍵                                    ⍝ edge
  ∪∘to¨@fm⊢⍺                                 ⍝ graph with new edge ⍵.
}

⍝ From http://dfns.dyalog.com/n_remlink.htm

remlink ← {                                  ⍝ Graph ⍺ without edge ⍵.
  fm to←⍵                                    ⍝ edge
  ~∘to¨@fm⊢⍺                                 ⍝ graph without edge ⍵.
}

⍝ From http://dfns.dyalog.com/n_search.htm

search ← {                                   ⍝ Breadth-first search of graph ⍺.
  graph←⍺                                    ⍝ ⍺ is graph vector.
  ⍵{                                         ⍝ from starting vertex.
    $[⍵≡⍬;⍺;                                 ⍝ no unvisited vertices: done.
      adjv←⍵⊃¨⊂graph                         ⍝ adjacent vertices.
      next←∪(↑,/adjv)~⍺                      ⍝ unvisited vertices.
      (⍺,next)∇ next                         ⍝ advance wave of visited vertices.
     ]
  }⍵                                         ⍝ from starting vertex.
}

⍝ From http://dfns.dyalog.com/n_path.htm

path ← {                                     ⍝ Shortest path from/to ⍵ in graph ⍺.
  graph (fm to)←⍺ ⍵                          ⍝ graph and entry/exit vertex vectors
  fm {                                       ⍝ fm is the starting-from vertex
    $[⍺≡⍬;⍬;                                 ⍝ no vertices left: no path
      $[∨/to∊⍺;                              ⍝ found target: path from tree:
        ⍬(⊃∘⍵) { $[⍵<0;⍺;                    ⍝ root: finished
                   (⍵,⍺) ∇ ⍺⍺ ⍵]             ⍝ accumulated path to next vertex
               } 1↑⍺∩to;                     ⍝ found vertex ⍺
        next←graph[⍺]∩¨⊂⍸⍵=¯2                ⍝ next vertices to visit
        back←⊃,/⍺+0×next                     ⍝ back links
        wave←⊃,/next                         ⍝ vertex wave front
        (∪wave) ∇ back@wave⊢⍵]]              ⍝ advanced wave front
  }¯2+(⍳⍴⍺)∊fm                               ⍝ null spanning tree
}

⍝ From http://dfns.dyalog.com/n_span.htm

span ← {                                     ⍝ Breadth-first spanning tree for graph ⍺.
  graph←⍺                                    ⍝ ⍺ is graph vector.
  (¯2+(⍳⍴⍺)∊⍵) {                             ⍝ ⍺: partial spanning tree.
    $[⍵≡⍬;⍺;                                 ⍝ no vertices: done.
      next←graph[⍵]∩¨⊂⍸⍺=¯2                  ⍝ untravelled edges
      back←⍵+0×next                          ⍝ back link per edge
      tree←(∊back)@(∊next)⊢⍺                 ⍝ partial spanning tree
      tree ∇∪∊next                           ⍝ advanced wave front
     ]
  }⍵                                         ⍝ ⍵: next wave of vertices to visit.
}

⍝ From http://dfns.dyalog.com/c_dfspan.htm

dfspan ← {                                   ⍝ Depth-first spanning tree: graph ⍺ from vertex ⍵.
  graph←⍺                                    ⍝ ⍺ is graph vector.
  trav ← {                                   ⍝ initial vertex and parent
    $[¯2≠⍺⊃⍵;⍵;                              ⍝ vertex visited: backtrack
      next←⌽⍺⊃graph                          ⍝ edges from vertex ⍺
      tree←⍶@⍺⊢⍵                             ⍝ ⍶ is ⍺'s parent
      ⊃⍺ ∇∇/next,⊂tree                       ⍝ visiting each edge in order
     ]
  }                                          ⍝ :: tree ← vtx (vtx ∇∇) tree
  ⍵(¯1 trav)¯2⊣¨⍺                            ⍝ depth-first traversal of graph ⍵
}

⍝ From http://dfns.dyalog.com/c_stdists.htm

stdists ← {                                  ⍝ Spanning-tree path lengths.
  tree←⍵                                     ⍝ spanning tree
  0{                                         ⍝ distance from root
    next dvec←⍵                              ⍝ chldren and distance vector
    $[next≡⍬;dvec;                           ⍝ no children: finished
      ∆dvec←⍺@next⊢dvec                      ⍝ extended distance vector
      ∆next←⍸tree∊next                       ⍝ grandchildren
      (⍺+1)∇ ∆next ∆dvec                     ⍝ examine rest of tree
     ]
  }(⍵⍳¯1)(⍵⊢¨¯1)                             ⍝ starting vertex and initial distances
}

⍝ From http://dfns.dyalog.com/n_stpath.htm

stpath ← {                                   ⍝ Path through spanning tree ⍺ to vertex ⍵.
  tree←⍺                                     ⍝ (partial) spanning tree.
  ⍬{                                         ⍝ path accumulator.
    $[⍵<0;(⍵=¯2)↓⍺;                          ⍝ root or unvisited vertex: finished.
      (⍵,⍺)∇ ⍵⊃tree                          ⍝ otherwise: prefix previous (parent) vertex.
     ]
  }⍵
}

⍝ From http://dfns.dyalog.com/c_stpaths.htm

stpaths ← {                                  ⍝ Spanning tree paths.
  tree←⍵                                     ⍝ spanning tree.
  root←⍵⍳¯1                                  ⍝ index of root vertex.
  paths←(root=⍳⍴⍵)↑¨root                     ⍝ initial path vector.

  paths{                                     ⍝ path to current vertices.
    next←(⍵=⊂tree)/¨⊂⍳⍴tree                  ⍝ vertices at next tree level.
    $[(⊂⍬)∧.≡next;⍺;                         ⍝ all null: finished.
      exts←(⊂¨⍵⊃¨⊂⍺),¨¨next                  ⍝ paths to next tree level.
      indx←↑,/next                           ⍝ accumulated indices.
      paths←(↑,/exts)@indx⊢⍺                 ⍝ set next level paths in paths vector.
      paths ∇ indx                           ⍝ advance to next tree level.
     ]
  }root                                      ⍝ index of starting vertex.
}

⍝ From http://dfns.dyalog.com/c_X.htm

X ← {                                        ⍝ Exact cover: Knuth's Algorithm X.
  ⍺←1∨.∨⍵                                    ⍝ all cols required by default.
  x←⍳⍴⍺                                      ⍝ column indices.
  d←(x~⍺/x)∘.=x                              ⍝ dummy rows for optional columns.
  z←{                                        ⍝ cover vector.
    r c←⍴⍵                                   ⍝ number of rows and columns.
    $[c=0;r⍴0;                               ⍝ empty matrix: success.
      n←+⌿⍵                                  ⍝ number of covers per column.
      f←(<\n=⌊/n)/⍵                          ⍝ first column with fewest 1s.
      ⍵ ∇{                                   ⍝ ⍺ is constraint matrix.
           $[~1∊⍵;0;                         ⍝ no rows: failure.
             f←<\⍵                           ⍝ first row selector.
             c←,f⌿⍺                          ⍝ cols selected by first row.
             r←∨/c/⍺                         ⍝ rows covered by selected cols.
             s←⍺⍺(~c)/(~r)⌿⍺                 ⍝ sub-matrix covers.
             $[s≡0;⍺ ∇ f<⍵;                  ⍝ failure: try a different row.
               f∨(~r)\s                      ⍝ success: row f included.
              ]
            ]
         },f                                 ⍝ ⍵ is vector of marked rows.
        ]
  }⍵⍪d                                       ⍝ exact cover.
  $[z≡0;0;                                   ⍝ failure: 0
    (-+/~⍺)↓z                                ⍝ without dummy rows.
   ]
}


⍝⍝ Weighted graph processing

wcost←{                                      ⍝ Cost vector for path ⍵ through weighted graph ⍺.
  graph costs←↓⍺                             ⍝ edges and edge-costs.
  $[2>≢⍵;0;                                  ⍝ null path: no cost.
    2{                                       ⍝ ⍺:from, ⍵:to.
      node←,⍺⊃graph                          ⍝ exits from vertex.
      indx←node⍳⍵                            ⍝ index of (⍺ ⍵) vertex.
      indx⊃,⍺⊃costs                          ⍝ ... associated cost.
    }/⍵                                      ⍝ pairwise along path.
   ]
}
