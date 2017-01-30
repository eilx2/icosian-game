#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)
(require lang/posn)

; constants
(define WIDTH 1600)
(define HEIGHT 900)
(define radius 27)
(define line-width 3)
(define text-size 40)

(provide start-draw)
(provide get-dist)
(provide find-closest-node)
(provide (struct-out node))

(define cr-text-pos (make-posn 800 400))
(define write-file-pos (make-posn 800 300))

; node is (make-node val pos) where
; - val is a Nat, represent the index of the node
; - pos is a (make-posn x y), represents the position of the node
; represents a node in the graph
(define-struct node [val pos])

; edge is a (make-edge x y)
; represents an edge in the graph (a connection between nodes x and y)
(define-struct edge [x y])

; edge edge -> Boolean
; returns true if the edges are equal, false otherwise
(define (edge-eq? e1 e2)
  (or (and (= (edge-x e1) (edge-x e2))
           (= (edge-y e1) (edge-y e2)))
      (and (= (edge-x e1) (edge-y e2))
           (= (edge-y e1) (edge-x e2)))))

; edge List<edge> -> Boolean
; returns true if a given edge is in the list, false otherwise 
(define (find-edge e edges)
  (cond [(empty? edges) #false]
        [else (or (edge-eq? e (first edges)) (find-edge e (rest edges)))]))

; Nat List<node> -> posn
; given a the index of a node and list of nodes, returns the position of the given node
(define (get-node-pos node nodes)
  (cond [(= node (node-val (first nodes))) (node-pos (first nodes))]
        [else (get-node-pos node (rest nodes))]))

; wstate is a (make-wstate nodes edges shift cr_node ind mv_node e-key moving), where
; - nodes is a List<node>, the current list of nodes
; - edges is a List<edge>, the current list of edges
; - shift is a Boolean, represents the state of the shift key - true for pressed, false otherwise
; - cr_node is a node, the current selected node for edge addition
; - ind is a Nat, the total number of nodes
; - mv_node is a node, the current node that is moving
; - e-key is a Boolean, represents the state of "e" key - true for pressed, false otherwise
; - moving is a Boolean, represents wether this is the first time the mouse is being dragged after a node was selected
; represents the current state of the world(graph)
(define-struct wstate [nodes edges shift cr_node ind mv_node e-key moving])

; save-screen is a (make-save-screen cr-text prev), where
; - cr-text is a String, represents the current input string if the use is on the "level-select" page
; - prev is a List<wstae>, represents the previous wstates
; represents that the use is in the save screen
(define-struct save-screen [cr-text prev])

; List<edge> List<node> -> Image
; given a list of edges and a list of nodes, draws the edges over the scene
(define (draw-edges edges nodes)
  (cond [(empty? edges) (empty-scene WIDTH HEIGHT)]
        [else (local ((define p1 (get-node-pos (edge-x (first edges)) nodes))
                      (define p2 (get-node-pos (edge-y (first edges)) nodes)))
                (add-line (draw-edges (rest edges) nodes)
                          ( posn-x p1) (posn-y p1)
                          ( posn-x p2) (posn-y p2)
                          (make-pen "black" line-width "solid" "butt" "miter")))]))


; List<node> List<edge> List<node> Nat
; given the original list of nodes, the original list of edges, the remaining list of nodes to draw
; and the current selected node
(define (draw-nodes nodes edges cr-node sp-node)
  (cond [(empty? cr-node) (draw-edges edges nodes)]
        [else (local ((define pos (node-pos (first cr-node))))
                (place-image (overlay/align "middle" "middle"
                                            (text (number->string (node-val (first cr-node))) radius "black")
                                            (if (and (not (empty? sp-node)) (equal? (node-val (first cr-node)) (node-val sp-node)))
                                                (overlay/align "middle" "middle" (circle radius "solid" "SkyBlue") (circle (+ radius 2) "solid" "red"))
                                                (overlay/align "middle" "middle" (circle radius "solid" "SkyBlue") (circle (+ radius 1.3) "solid" "black"))))
                             (posn-x pos) (posn-y pos)
                             (draw-nodes nodes edges (rest cr-node) sp-node)))]))

; List<node> Nat -> node 
; given a list of nodes and an index, returns the node with the given index
(define (get-node nodes ind)
  (cond [(= (node-val (first nodes)) ind) (first nodes)]
        [else (get-node (rest nodes) ind)]))

; List<wstate> -> Image
; draws the current world state
(define (draw states)
  (if (save-screen? states)
       (place-images (list (text "Write the name of the file where the graph will be salved:" text-size "black")
                             (text (save-screen-cr-text states) text-size "blue"))
                       (list write-file-pos
                             cr-text-pos)
                       (empty-scene WIDTH HEIGHT))
      (local ((define state (first states)))
        (place-images (list (text "Shift + Click - New Node" 25 "black")
                            (text "e + Click - Select Node" 25 "black")
                            (text "s - Save Graph" 25 "black"))
                      (list (make-posn 180 800)
                            (make-posn 180 840)
                            (make-posn 180 880))
                      (draw-nodes (wstate-nodes state) (wstate-edges state) (wstate-nodes state) (wstate-cr_node state))))))

; starting state
(define start (make-wstate '() '() #false '() 0 '() #false #f))

; List<node> -> string
; given a list of nodes, transforms it into a string in a CSV format
(define (nodes->string nodes)
  (cond [(empty? nodes) ""]
        [else (local ((define node (first nodes)))
          (string-append "Node,"
                         (number->string (node-val node)) ","
                         (number->string (posn-x (node-pos node))) ","
                         (number->string (posn-y (node-pos node))) "\n"
                         (nodes->string (rest nodes))))]))

; List<edge> -> string
; given a lift of edges, transforms it into a string in a CSV format
(define (edges->string edges)
  (cond [(empty? edges) ""]
        [else (local ((define edge (first edges)))
                (string-append "Edge,"
                               (number->string (edge-x edge)) ","
                               (number->string (edge-y edge)) "\n"
                               (edges->string (rest edges))))]))
        

; Filename wstate -> wstate
; saves the current state to a file, and returns the same state
(define (save-state file state)
  (local (
          (define output (string-append "Node Count,"
                                        (number->string (wstate-ind state)) "\n"
                                        (nodes->string (wstate-nodes state))
                                        (edges->string (wstate-edges state))))
          (define wtf (write-file file output)))
    state))

; posn posn -> Number
; returns the euclidean distance between 2 positions in the plane
(define (get-dist pos1 pos2)
  (sqrt (+ (sqr (- (posn-x pos1) (posn-x pos2))) (sqr (- (posn-y pos1) (posn-y pos2))))))

; posn List<node> Number -> node / '()
; given a posn, a list of nodes and a number d, finds the first node
; such that it's distance to the given position is smaller than d,
; or returns '() if there's no such node
(define (find-closest-node pos nodes d)
  (cond [(empty? nodes) '()]
        [else (if (<= (get-dist pos (node-pos (first nodes)))
                      d)
                  (first nodes)
                  (find-closest-node pos (rest nodes) d))])) 
    
; node posn List<node> -> List<node>
; given a node, a posn and a list of nodes, return a new list of nodes
; in which the position of the given node is updated to the given posn
(define (change-node-pos node pos nodes)
  (cond [(empty? nodes) '()]
        [(equal? node (first nodes)) (cons (make-node (node-val node) pos) (rest nodes))]
        [else (cons (first nodes) (change-node-pos node pos (rest nodes)))]))


; List<wstate> Number Number String -> List<wstate>
; handle mouse events
(define (handle-mouse states x y event)
  (if (save-screen? states) states
  (local ((define state (first states))
          (define change #f)
          (define cr-moving (wstate-moving state))
    (define new_state (cond [(string=? event "button-down")
                             (cond [(not (empty? (find-closest-node (make-posn x y) (wstate-nodes state) radius)))
                                    (local ((define node (find-closest-node (make-posn x y) (wstate-nodes state) radius)))
                                      (make-wstate (wstate-nodes state)
                                                   (if (wstate-e-key state)
                                                       (if (empty? (wstate-cr_node state))
                                                           (wstate-edges state)
                                                           (if (find-edge (make-edge (node-val node) (node-val (wstate-cr_node state)))
                                                                          (wstate-edges state))
                                                               (wstate-edges state)
                                                               (local ((define junk (set! change #t))) (cons (make-edge (node-val node) (node-val (wstate-cr_node state))) (wstate-edges state))))
                                                           )
                                                       (wstate-edges state))
                                                   (wstate-shift state)
                                                   (if (wstate-e-key state)
                                                       (if (empty? (wstate-cr_node state)) node '())
                                                       (wstate-cr_node state))
                                                   (wstate-ind state)
                                                   node
                                                   (wstate-e-key state)
                                                   #t))]
                                   [(not (empty? (find-closest-node (make-posn x y) (wstate-nodes state) (+ (* 2 radius) 5)))) state]
                                   [(wstate-shift state) (make-wstate (local ((define junk (set! change #t))) (cons (make-node (+ 1 (wstate-ind state)) (make-posn x y)) (wstate-nodes state)))
                                                                      (wstate-edges state)
                                                                      (wstate-shift state)
                                                                      (wstate-cr_node state)
                                                                      (+ 1 (wstate-ind state))
                                                                      (wstate-mv_node state)
                                                                      (wstate-e-key state)
                                                                      #f)]
                                   [else state])]
                            [(string=? event "button-up") (make-wstate (wstate-nodes state)
                                                                       (wstate-edges state)
                                                                       (wstate-shift state)
                                                                       (wstate-cr_node state)
                                                                       (wstate-ind state)
                                                                       '()
                                                                       (wstate-e-key state)
                                                                       #f)]
                            [(string=? event "drag")  (if (empty? (wstate-mv_node state))
                                                          state
                                                          (make-wstate (change-node-pos (wstate-mv_node state) (make-posn x y) (wstate-nodes state))
                                                                       (wstate-edges state)
                                                                       (wstate-shift state)
                                                                       (if (and (not (empty? (wstate-cr_node state))) (= (node-val (wstate-cr_node state)) (node-val (wstate-mv_node state))))
                                                                           (get-node (wstate-nodes state) (node-val (wstate-mv_node state)))
                                                                           (wstate-cr_node state))
                                                                       (wstate-ind state)
                                                                       (get-node (wstate-nodes state) (node-val (wstate-mv_node state)))
                                                                       (wstate-e-key state)
                                                                       #f))]
                            
                            [else state])))
    (if (or change (and (string=? event "drag") cr-moving))
        (cons new_state states)
        (cons new_state (rest states))))))

; wstate -> wstate
; given a state, sets all its register fields(key status, current node status, etc) to #f or '()
(define (disable-keys state)
  (make-wstate (wstate-nodes state)
               (wstate-edges state)
               #f
               '()
               (wstate-ind state)
               '()
               #f
               #f))

; char -> boolean
; returns true if the given char is either a letter, an alphanumeric or is equal to ".", "-", "_"
(define (valid-chr? chr)
  (or (char-alphabetic? chr) (char-numeric? chr) (char=? chr #\.) (char=? chr #\-) (char=? chr #\_)))

; List<wstate> String -> List<wstate>
; handles key presses
(define (handle-key states key)
  (if (save-screen? states)
      (local ((define state states)
              (define cr-text (save-screen-cr-text state))
              (define event key))
        (cond [(string=? event "\r") (if (> (string-length cr-text) 0)
                                         (begin (save-state cr-text (first (save-screen-prev state)))
                                                (save-screen-prev state))
                                         state)]
              [(string=? event "\b") (if (> (string-length cr-text) 0)
                                         (make-save-screen (substring cr-text 0 (- (string-length cr-text) 1))
                                                           (save-screen-prev state))
                                         state)]
              [(= 1 (length (string->list event))) (local ((define chr (first (string->list event))))
                                                     (make-save-screen (if (valid-chr? chr)
                                                                      (string-append cr-text event)
                                                                      cr-text)
                                                                  (save-screen-prev state)))]
              [else state]))
      (local ((define state (first states)))
        (if (and (string=? key "r") (not (empty? (rest states)))) (cons (disable-keys (first (rest states))) (rest (rest states)))
            (cond [(string=? key "s") (make-save-screen "" states)]
                  [else (cons (make-wstate (wstate-nodes state)
                                     (wstate-edges state)
                                     (if (string=? key "shift") #true (wstate-shift state))
                                     (if (string=? key "c") '() (wstate-cr_node state))
                                     (wstate-ind state)
                                     (wstate-mv_node state)
                                     (if (string=? key "e") #true (wstate-e-key state))
                                     (wstate-moving state)) (rest states))])))))
; List<wstate> String -> List<wstate>
; handles key releases
(define (handle-release states key)
  (if (save-screen? states) states
      (local ((define state (first states)))
        (cons  (make-wstate (wstate-nodes state)
                            (wstate-edges state)
                            (if (string=? key "shift") #false (wstate-shift state))
                            (wstate-cr_node state)
                            (wstate-ind state)
                            (wstate-mv_node state)
                            (if (string=? key "e") #false (wstate-e-key state))
                            (wstate-moving state))
               (rest states)))))

; function to handle the start of the drawing application
(define (start-draw x)
  (big-bang (list start)
          (to-draw draw)
          (on-key handle-key)
          (on-mouse handle-mouse)
          (on-release handle-release)))
