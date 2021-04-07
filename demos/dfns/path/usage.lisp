(ql:quickload "april")

(in-package :april)

(april-load #p"demos/dfns/path/path.apl")

;; http://dfns.dyalog.com/s_path.htm

; directed graph.
(april "g←(2 3) (3) (2 4) (1 5) (3)")

; single vertex→vertex path.
(april "g path 2 1 ")
; #(2 3 4 1)

; multiple vertex→vertex path.
(april "g path (1 2)(4 5)")
; #(2 3 4)

; each to every single vertex.
(april "g∘path¨⍳5 5")
; #2A((#(1) #(1 2) #(1 3) #(1 3 4) #(1 3 4 5))
;     (#(2 3 4 1) #(2) #(2 3) #(2 3 4) #(2 3 4 5))
;     (#(3 4 1) #(3 2) #(3) #(3 4) #(3 4 5))
;     (#(4 1) #(4 1 2) #(4 5 3) #(4) #(4 5))
;     (#(5 3 4 1) #(5 3 2) #(5 3) #(5 3 4) #(5)))
