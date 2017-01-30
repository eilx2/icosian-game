#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)
(require lang/posn)
(require "./draw.rkt")

(provide start-game)
(provide over-text?)

; Dynamic Programming and Graph structures
(define graph '())
(define nodes_count 0)
(define dp '())
(define node-list '())
(define state-list '())

; Bitmask is a Number
; holds information in the dynamic programming state or world state
; if the i^(th) bit is set to 1, then the (i+1)^th node is visited

; the solver from the program
; computes the value of the function dp(x,cr_bitmask), where
; dp(x,cr_bitmask)= #t if there exists a hamiltonian cycle which starts in node x
; finishes in node 1, and the nodes represented by cr_bitmask have been already visited
; dp(x,cr_bitmask)= #f, if there's no such cycle


; wstate is a (make-wstate bitmask cr_node solved mouse-pos) where
; - bitmask is a Bitmask, represents information about currently visited nodes
; - cr_node is a Number, represents the index of the current node
; - solve is one of 0,1,2, represents the state of the game
;    - 0, the game is in progress
;    - 1, the game has been finished/solved
;    - 2, the solver was not able to found a solution given the current state
; - mouse-pos is (make-posn x y), represents the current position of the mouse
; represents the world state
(define-struct wstate [bitmask cr_node solved mouse-pos])

; dpstate is a (make-dpstate cr-node bitmask next mouse-pos)
; - cr_node is a Number, the index of the node in current dp state
; - bitmask is a Number, represents the bitmask for which we currently compute the dp
; - next is a Number, represents the next node to be considered
; - mouse-pos is a (make-posn x y), represents the mouse position
; represents a state in the interactive solver 
(define-struct dpstate [cr_node bitmask next mouse-pos])

; Constants
(define WIDTH 1600)
(define HEIGHT 900)
(define radius 27)
(define line-width 3)
(define text-size 40)
(define reset-pos (make-posn 1500 840))
(define fsolve-pos (make-posn 200 850))
(define isolve-pos (make-posn 200 790))
(define message-pos (make-posn 800 820))




; Number Number -> Number
; given the index of 2 nodes, returns a position in the vector graph
; which holds the status of the edge between these 2 nodes
(define (get-ind x y)
  (+ (* (- x 1) nodes_count) y -1))

; Number Bitmask -> Number
; returns a position in the vector dp, which
; holds the value of dp(node,bitmask)
(define (get-dp-ind node bitmask)
  (+ (* (- node 1) (expt 2 nodes_count)) bitmask))

; Number Bitmask -> Boolean
; given a number X and a bitmask, returns true if the bitmask has 1 at
; position X-1, and false otherwise
(define (visited? x bitmask)
  (> (bitwise-and (expt 2 (- x 1)) bitmask) 0))

; posn Number Number (make-posn x y) -> Boolean
; returns if the mouse is located in the rectangle where some text is written
(define (over-text? pos v h mouse-pos)
  (and (<= (abs (- (posn-x pos) (posn-x mouse-pos))) (/ h 2)) (<= (abs (- (posn-y pos) (posn-y mouse-pos))) (/ v 2))))



; List<node> Number -> posn
; given a list of nodes and and a node index, returns the position of the node
(define (get-node-pos nodes ind)
  (cond [(empty? nodes) (make-posn 0 0)]
        [else (if (= (node-val (first nodes)) ind) (node-pos (first nodes)) (get-node-pos (rest nodes) ind))]))

; List<node> Number Number -> Image
; draws the edges of the graph
(define (draw-edges nodes x y)
  (cond [(> x nodes_count) (empty-scene WIDTH HEIGHT)]
        [(> y nodes_count) (draw-edges nodes (+ x 1) (+ x 1))]
        [else (local ((define p1 (get-node-pos nodes x))
                      (define p2 (get-node-pos nodes y))
                      (define type (vector-ref graph (get-ind x y))))
                (if (= 0 type)
                    (draw-edges nodes x (+ y 1))
                    (add-line (draw-edges nodes x (+ y 1))
                              (posn-x p1) (posn-y p1)
                              (posn-x p2) (posn-y p2)
                              (make-pen (if (= type 1) "black" "red") line-width "solid" "butt" "miter"))))]))

; List<node> List<node> Bitmask index
; given the list of all nodes, the current list of nodes, the bitmask and the index of the current node,
; the function draws the graph
(define (draw-graph all_nodes nodes bitmask cr_node)
  (cond [(empty? nodes) (draw-edges all_nodes 1 1)]
        [else (local ((define pos (node-pos (first nodes)))
                      (define vis (visited? (node-val (first nodes)) bitmask))
                      (define cr (= cr_node (node-val (first nodes)))))
                (place-image (overlay/align "middle" "middle"
                                            (text (number->string (node-val (first nodes))) radius "black")
                                            (overlay/align "middle" "middle" (circle radius "solid" (if cr "green" (if vis "Tomato" "SkyBlue")))
                                                           (circle (if cr (+ radius 2.8) (+ radius 1.3)) "solid" (if cr "Blue" "black"))))
                             (posn-x pos) (posn-y pos)
                             (draw-graph all_nodes (rest nodes) bitmask cr_node)))]))

; List<wstate>/List<dpstate> -> Image
; given a list of wstate or dpstate, the function draws the current state
(define (draw states)
  (local ((define state (first states)))
    (place-image (if (wstate? state)
                     (cond [(= (wstate-solved state) 1) (text "Well Done!" text-size "green")]
                           [(= (wstate-solved state) 2) (text "No solution found!" text-size "red")]
                           [else (empty-scene 0 0)])
                     (empty-scene 0 0))
                 (posn-x message-pos) (posn-y message-pos)
                 (place-image (text "Reset" text-size (if (over-text? reset-pos text-size 130 (if (dpstate? state) (dpstate-mouse-pos state) (wstate-mouse-pos state))) "red" "black"))
                              (posn-x reset-pos) (posn-y reset-pos)
                              (place-image (text "Interactive Solver" text-size (if (and (not (dpstate? state)) (over-text? isolve-pos text-size 330 (wstate-mouse-pos state))) "red" "black"))
                                           (posn-x isolve-pos) (posn-y isolve-pos)
                                           (place-image (text "Fast Solver" text-size (if (and (not (dpstate? state)) (over-text? fsolve-pos text-size 220 (wstate-mouse-pos state))) "red" "black"))
                                                        (posn-x fsolve-pos) (posn-y fsolve-pos)
                                                        (if (dpstate? state)
                                                            (draw-graph node-list node-list (dpstate-bitmask state) (dpstate-cr_node state))
                                                            (draw-graph node-list node-list (wstate-bitmask state) (wstate-cr_node state))))))))) 
; Number Number Number -> void
; changes the status of the edge between node x and y
(define (ch-edge-col x y col)
  (void (vector-set! graph (get-ind x y) col) (vector-set! graph (get-ind y x) col)))

; Number Number -> void
; sets all edges in the graph to the initial state
(define (reset-graph x y)
  (cond [(> x nodes_count) void]
        [(> y nodes_count) (reset-graph (+ x 1) 1)]
        [else (void (if (> (vector-ref graph (get-ind x y)) 0) (ch-edge-col x y 1) void)
                    (reset-graph x (+ y 1)))]))

; NodeInd Bitmask Number -> Boolean
; computes the value of the function dp(x,cr_bitmask)
(define (fsolve node bitmask i)
  (cond [(= 0 (vector-ref dp (get-dp-ind node bitmask))) #f]
        [(and (= bitmask (- (expt 2 nodes_count) 1)) (= 1 (vector-ref graph (get-ind node 1)))) (begin (ch-edge-col node 1 2) #t)]
        [(> i nodes_count) (begin (vector-set! dp (get-dp-ind node bitmask) 0) #f)]
        [(or (visited? i bitmask) (= 0 (vector-ref graph (get-ind node i)))) (fsolve node bitmask (+ i 1))]
        [else (begin (ch-edge-col node i 2) (if (fsolve i (bitwise-ior bitmask (expt 2 (- i 1))) 1)
                                                #t
                                                (begin (ch-edge-col node i 1) (fsolve node bitmask (+ i 1)))))]))

; wstate Number Number String -> wstate
; handles mouse presses
(define (handle-mouse states x y event)
  (local ((define state (first states)))
    (cond  [(string=? event "button-down") (cond [(over-text? reset-pos text-size 130 (if (dpstate? state) (dpstate-mouse-pos state) (wstate-mouse-pos state)))
                                                 (begin (reset-graph 1 1)
                                                        (set! dp (make-vector (* nodes_count (expt 2 nodes_count)) -1))
                                                        (reset-graph 1 1)
                                                        (list (make-wstate 1 1 0 (make-posn 0 0))))]
                                                [(dpstate? state) states]
                                                [(= 1 (wstate-solved state)) states]
                                                [(over-text? isolve-pos text-size 330 (wstate-mouse-pos state))
                                                 (begin (set! dp (make-vector (* nodes_count (expt 2 nodes_count)) -1))
                                                        (set! state-list states)
                                                        (list (make-dpstate (wstate-cr_node state)
                                                                            (wstate-bitmask state)
                                                                            1
                                                                            (wstate-mouse-pos state))))]
                                                [(over-text? fsolve-pos text-size 220 (wstate-mouse-pos state))
                                                 (begin (set! dp (make-vector (* nodes_count (expt 2 nodes_count)) -1))
                                                        (set! state-list states)
                                                        (if (fsolve (wstate-cr_node state) (wstate-bitmask state) 1)
                                                            (list (make-wstate (- (expt 2 nodes_count) 1) 1 1 (make-posn x y)))
                                                            (cons (make-wstate (wstate-bitmask state) (wstate-cr_node state) 2 (wstate-mouse-pos state)) (rest states))))]
                                                [else (local ((define nearest-node (find-closest-node (make-posn x y) node-list radius))
                                                              (define nr-node (if (empty? nearest-node) 0 (node-val nearest-node)))
                                                              (define cr-node (wstate-cr_node state)))
                                                        (if (and
                                                             (not (= nr-node 0))
                                                             (= (vector-ref graph (get-ind nr-node cr-node)) 1)
                                                                 (or (not (visited? nr-node (wstate-bitmask state)))
                                                                     (and (= (wstate-bitmask state) (- (expt 2 nodes_count) 1)) (= nr-node 1))))
                                                            (begin (vector-set! graph (get-ind nr-node cr-node) 2)
                                                                   (vector-set! graph (get-ind cr-node nr-node) 2)
                                                                   (cons (make-wstate (bitwise-ior (wstate-bitmask state) (expt 2 (- nr-node 1)))
                                                                                      nr-node
                                                                                      (if (= nr-node 1) 1 0)
                                                                                      (make-posn x y))
                                                                         (if (= nr-node 1) '() states)))
                                                            states))])];
          [else (if (dpstate? state)
                    (cons (make-dpstate (dpstate-cr_node state) (dpstate-bitmask state) (dpstate-next state) (make-posn x y)) (rest states)) 
                    (cons (make-wstate (wstate-bitmask state) (wstate-cr_node state) (wstate-solved state) (make-posn x y)) (rest states)))])))

; wstate String -> wstate
; handles key presses
(define (handle-key states key)
  (cond [(dpstate? (first states)) states]
        [(string=? key "r") (if (empty? (rest states))
                                states
                                (begin (ch-edge-col (wstate-cr_node (first states))
                                                      (wstate-cr_node (first (rest states)))
                                                      1)
                                                      (rest states)))]
        [else states]))

; wstate -> wstate
; handles ticks and is responsible for the interactive solver
; every tick an operation for calculating the dp is done
(define (handle-tick states)
  (if (wstate? (first states))
      states
      (local ((define state (first states))
              (define x (dpstate-cr_node state))
              (define y (dpstate-next state))
              (define bitmask (dpstate-bitmask state)))
        (cond [(> y nodes_count) (if (empty? (rest states))
                                     (cons (make-wstate (wstate-bitmask (first state-list))
                                                        (wstate-cr_node (first state-list))
                                                        2
                                                        (wstate-mouse-pos (first state-list)))
                                           (rest state-list))
                                     (begin (ch-edge-col x (dpstate-cr_node (first (rest states))) 1)
                                            (vector-set! dp (get-dp-ind x bitmask) 0)
                                            (rest states)))]
              [(or (visited? y bitmask)
                   (= 0 (vector-ref graph (get-ind x y)))
                   (= 0 (vector-ref dp (get-dp-ind y (bitwise-ior bitmask (expt 2 (- y 1)))))))
               (if (and (= bitmask (- (expt 2 nodes_count) 1)) (= y 1) (= 1 (vector-ref graph (get-ind x y))))
                    (begin (ch-edge-col x y 2) (list (make-wstate bitmask 1 1 (dpstate-mouse-pos state))))
                    (handle-tick (cons (make-dpstate x bitmask (+ y 1) (dpstate-mouse-pos state)) (rest states))))]
              [else (begin (ch-edge-col x y 2) (cons (make-dpstate y (bitwise-ior bitmask (expt 2 (- y 1))) 1 (dpstate-mouse-pos state)) states))] ))))

; List<List<String>> -> List<Node>
; given the contents of a CSV file, the functions initializes the edges and
; returns the list of nodes
(define (get-graph lst)
  (cond [(empty? lst) '()]
        [else 
         (local ((define line (first lst)))
           (cond [(string=? (first line) "Node") (cons (make-node (string->number (second line)) (make-posn (string->number (third line)) (string->number (fourth line))))
                                                       (get-graph (rest lst)))]
                 [(string=? (first line) "Edge") (local ((define x (string->number (second line)))
                                                         (define y (string->number (third line))))
                                                         (begin (ch-edge-col x y 1)
                                                                (get-graph (rest lst))))]
                 [else (get-graph (rest lst))]))]))

; Filename -> List<wstate>
; reads a given file, initiliazes the graph definitions
; and returns the a list with the first world state
(define (read-graph filename)
  (local ((define lst (read-csv-file filename))
          (define first-line (first lst)))
          (begin (set! nodes_count (string->number (second first-line)))
                 (set! graph (make-vector (* nodes_count nodes_count) 0))
                 (set! dp (make-vector (* nodes_count (expt 2 nodes_count)) -1))
                 (set! node-list (get-graph (rest lst)))
                 (list (make-wstate 1 1 0 (make-posn 0 0))))))

; Filename -> big-bang
; handles game start
(define (start-game filename)
  (big-bang (read-graph filename)
            (to-draw draw)
            (on-key handle-key)
            (on-mouse handle-mouse)
            (on-tick handle-tick 1/20)))


