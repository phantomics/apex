⍝ Ported from http://dfns.dyalog.com/c_path.htm into April APL

gperm ← {                               ⍝ ⍵-permutation of vertices of graph ⍺.
  (⊂⍵)⍳¨⍺[⍵]
}

insnode ← {                             ⍝ Insert vertex ⍵ in graph ⍺.
  (⍵⌈⍴⍺)↑⍺,⍵⍴⊂⍬                         ⍝ extend graph with sufficient nulls.
}

remnode ← {                             ⍝ Remove vertex ⍵ from graph ⍺.
  new←(⍵≠⍳⍴⍺)/⍺~¨⍵                      ⍝ graph with vertex ⍵ removed,
  new-new>⍵                             ⍝ and edges adjusted.
}

inslink ← {                             ⍝ Graph ⍺ with new edge ⍵.
  fm to←⍵                               ⍝ edge
  ∪∘to¨@fm⊢⍺                            ⍝ graph with new edge ⍵.
}

remlink ← {                             ⍝ Graph ⍺ without edge ⍵.
  fm to←⍵                               ⍝ edge
  ~∘to¨@fm⊢⍺                            ⍝ graph without edge ⍵.
}

search ← {                              ⍝ Breadth-first search of graph ⍺.
  graph←⍺                               ⍝ ⍺ is graph vector.
  ⍵{                                    ⍝ from starting vertex.
      $[⍵≡⍬;⍺;                          ⍝ no unvisited vertices: done.
        adjv←⍵⊃¨⊂graph                  ⍝ adjacent vertices.
        next←∪(↑,/adjv)~⍺               ⍝ unvisited vertices.
        (⍺,next)∇ next                  ⍝ advance wave of visited vertices.
       ]
  }⍵                                    ⍝ from starting vertex.
}

path ← {                                ⍝ Shortest path from/to ⍵ in graph ⍺.
  graph (fm to)←⍺ ⍵                     ⍝ graph and entry/exit vertex vectors
  fm {                                  ⍝ fm is the starting-from vertex
    $[⍺≡⍬;⍬;                            ⍝ no vertices left: no path
      $[∨/to∊⍺;                         ⍝ found target: path from tree:
        ⍬(⊃∘⍵) { $[⍵<0;⍺;               ⍝ root: finished
                   (⍵,⍺) ∇ ⍺⍺ ⍵]        ⍝ accumulated path to next vertex
               } 1↑⍺∩to;                ⍝ found vertex ⍺
        next←graph[⍺]∩¨⊂⍸⍵=¯2           ⍝ next vertices to visit
        back←⊃,/⍺+0×next                ⍝ back links
        wave←⊃,/next                    ⍝ vertex wave front
        (∪wave) ∇ back@wave⊢⍵]]         ⍝ advanced wave front
  }¯2+(⍳⍴⍺)∊fm                          ⍝ null spanning tree
}

span ← {                                ⍝ Breadth-first spanning tree for graph ⍺.
  graph←⍺                               ⍝ ⍺ is graph vector.
  (¯2+(⍳⍴⍺)∊⍵) {                        ⍝ ⍺: partial spanning tree.
    $[⍵≡⍬;⍺;                            ⍝ no vertices: done.
      next←graph[⍵]∩¨⊂⍸⍺=¯2             ⍝ untravelled edges
      back←⍵+0×next                     ⍝ back link per edge
      tree←(∊back)@(∊next)⊢⍺            ⍝ partial spanning tree
      tree ∇∪∊next                      ⍝ advanced wave front
     ]
  }⍵                                    ⍝ ⍵: next wave of vertices to visit.
}

⍝ dfspan ← {                    ⍝ Depth-first spanning tree: graph ⍺ from vertex ⍵.
⍝   graph←⍺                 ⍝ ⍺ is graph vector.
⍝   trav ← {                  ⍝ initial vertex and parent
⍝     $[¯2≠⍺⊃⍵;⍵;            ⍝ vertex visited: backtrack
⍝       next←⌽⍺⊃graph       ⍝ edges from vertex ⍺
⍝       tree←⍺⍺@⍺⊢⍵         ⍝ ⍺⍺ is ⍺'s parent
⍝       ⊃⍺ ∇∇/next,⊂tree    ⍝ visiting each edge in order
⍝      ]
⍝   }                       ⍝ :: tree ← vtx (vtx ∇∇) tree
⍝   ⍵(¯1 trav)¯2⊣¨⍺         ⍝ depth-first traversal of graph ⍵
⍝ }
