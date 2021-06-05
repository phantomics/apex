⍝ Ported from http://dfns.dyalog.com/c_path.htm into April APL

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
