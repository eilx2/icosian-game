#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require racket/os)
(require "./draw.rkt")
(require "./game.rkt")


; constants
(define WIDTH 1200)
(define HEIGHT 900)
(define play-pos (make-posn 830 330))
(define draw-pos (make-posn 830 480))
(define back-pos (make-posn 120 820))
(define cr-text-pos (make-posn 600 400))
(define write-file-pos (make-posn 600 300))
(define nfound-pos (make-posn 600 800))
(define about-pos (make-posn 830 630))
(define title-pos (make-posn 600 80))
(define background-pos (make-posn 280 460))
(define about-img-pos (make-posn 210 400))
(define emilia-maji-tenshi (make-posn 210 690))

(define background (bitmap "../images/main.png"))
(define about-img (bitmap "../images/about.png"))

(define text-size 70)
(define small-text-size 40)

; a wstate is a (make-wstate page cr-text mouse-pos nfound) where
; - page is a String, represents the current page the user is on
; - cr-text is a String, represents the current input string if the use is on the "level-select" page
; - mouse-pos is a (make-posn x y), represents the coordinates of the mouse
; - nfound is a Boolean, maintains temporary info of wether the last file the user tried to load was present or not in the directory
(define-struct wstate [page cr-text mouse-pos nfound])

; wstate -> Image
; given a wstate draws the corresponding image
(define (draw state)
  (cond [(string=? (wstate-page state) "menu")
         (place-images (list (text "Play Game" text-size (if (over-text? play-pos text-size 340 (wstate-mouse-pos state)) "red" "black"))
                             (text "Draw Graph" text-size (if (over-text? draw-pos text-size 370 (wstate-mouse-pos state)) "red" "black"))
                             (text "About" text-size (if (over-text? about-pos text-size 200 (wstate-mouse-pos state)) "red" "black"))
                             (text "Icosian Game" (+ text-size 20) "RoyalBlue")
                             background)
                       (list play-pos
                             draw-pos
                             about-pos
                             title-pos
                             background-pos)
                       (empty-scene WIDTH HEIGHT))]
        [(string=? (wstate-page state) "level-select")
         (place-images (list (text "Write below the name of the file" small-text-size "black")
                             (text "< Back" small-text-size (if (over-text? back-pos small-text-size 150 (wstate-mouse-pos state)) "red" "black"))
                             (text (wstate-cr-text state) small-text-size "blue")
                             (if (wstate-nfound state) (text "File not found!" small-text-size "red") (empty-scene 0 0)))
                       (list write-file-pos
                             back-pos
                             cr-text-pos
                             nfound-pos)
                       (empty-scene WIDTH HEIGHT))]
        [(string=? (wstate-page state) "about")
         (place-images (list (text "< Back" small-text-size (if (over-text? back-pos small-text-size 150 (wstate-mouse-pos state)) "red" "black"))
                             (text "The icosian game is a mathematical game invented in 1857 by William Rowan Hamilton." 20 "black")
                             (text "The goal of the original game was to find a hamiltonian cycle in the graph" 20 "black")
                             (text "formed by embedding the regular dodecahedron into the plane." 20 "black")
                             (text "A Hamiltonian cycle in a graph is a cycle that visits each node exactly once." 20 "black")
                             (text "This program incorporates two features:" 20 "black")
                             (text " - a graph designer software which allows you to draw graphs in the plane" 20 "black")
                             (text "   and save them in the CSV format for latter uses for the game or other purposes" 20 "black")
                             (text " - an icosian game simulator in which your goal as a player is to find" 20 "black")
                             (text "   a hamiltonian cycle in a given graph, and a solver for the game" 20 "black")
                             (text "   which implements an algorithm that finds a cycle in O(n^2*2^n) time" 20 "black")
                             about-img
                             (text "エミリアたんマジ天使!" 20 "blue"))
                      (list back-pos
                            (make-posn 800 250)
                            (make-posn 800 280)
                            (make-posn 800 310)
                            (make-posn 800 350)
                            (make-posn 800 420)
                            (make-posn 800 450)
                            (make-posn 800 480)
                            (make-posn 800 530)
                            (make-posn 800 560)
                            (make-posn 800 590)
                            about-img-pos
                            emilia-maji-tenshi)
                      (empty-scene WIDTH HEIGHT))]))

; wstate Number Number String -> wstate
; handles mouse events
(define (handle-mouse state x y event)
  (cond [(string=? event "button-down") (if (string=? (wstate-page state) "menu")
                                            (cond [(over-text? play-pos text-size 340 (wstate-mouse-pos state)) (make-wstate "level-select" "" (make-posn x y) #f)]
                                                  [(over-text? draw-pos text-size 370 (wstate-mouse-pos state)) (begin (start-draw 0) state)]
                                                  [(over-text? about-pos text-size 200 (wstate-mouse-pos state)) (make-wstate "about" "" (make-posn x y) #f)]
                                                  [else (make-wstate (wstate-page state) (wstate-cr-text state) (make-posn x y) #f)])
                                            (cond [(over-text? back-pos small-text-size 220 (wstate-mouse-pos state)) (make-wstate "menu" (wstate-cr-text state) (make-posn x y) #f)]
                                                  [else (make-wstate (wstate-page state) (wstate-cr-text state) (make-posn x y) (wstate-nfound state))]))]
        [else (make-wstate (wstate-page state) (wstate-cr-text state) (make-posn x y) (wstate-nfound state))]))

; char -> boolean
; returns true if the given char is either a letter, an alphanumeric or is equal to ".", "-", "_"
(define (valid-chr? chr)
  (or (char-alphabetic? chr) (char-numeric? chr) (char=? chr #\.) (char=? chr #\-) (char=? chr #\_)))

; wstate String -> wstate
; handles key presses
(define (handle-key state event)
  (local ((define cr-text (wstate-cr-text state)))
    (if (string=? (wstate-page state) "level-select")
        (cond [(string=? event "\r") (if (and (> (string-length cr-text) 0) (file-exists? cr-text))
                                            (begin (start-game cr-text) state)
                                            (make-wstate (wstate-page state)
                                                         cr-text
                                                         (wstate-mouse-pos state)
                                                         #t))]
              [(string=? event "\b") (if (> (string-length cr-text) 0)
                                         (make-wstate (wstate-page state)
                                                      (substring cr-text 0 (- (string-length cr-text) 1))
                                                      (wstate-mouse-pos state)
                                                      #f)
                                         state)]
              [(= 1 (length (string->list event))) (local ((define chr (first (string->list event)))) (make-wstate (wstate-page state)
                                                                                                    (if (valid-chr? chr)
                                                                                                        (string-append cr-text event)
                                                                                                        cr-text)
                                                                                                    (wstate-mouse-pos state)
                                                                                                    #f))]
              [else state])
        state)))

; start game
(big-bang (make-wstate "menu" "" (make-posn 0 0) #f)
          (to-draw draw)
          (on-mouse handle-mouse)
          (on-key handle-key))
