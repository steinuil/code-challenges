#lang racket/base

; 465 players; last marble is worth 71940 points

;; Returns (values <new-curr-cons> <maybe-taken-marble>)
(define (place-marble marble curr-cons)
  (if (= (modulo marble 23) 0)
      (let loop ([curr-cons curr-cons])
        (if (eq? (mcar (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr curr-cons)))))))))
                 (- marble 1))
            (let ([next (mcdr (mcdr curr-cons))]
                  [taken (mcar (mcdr curr-cons))])
              (set-mcdr! curr-cons next)
              (values next taken))
            (loop (mcdr curr-cons))))
      (let ([new-curr (mcons marble (mcdr (mcdr curr-cons)))])
        (set-mcdr! (mcdr curr-cons) new-curr)
        (values (mcdr (mcdr curr-cons)) null))))

(define (make-circular-list)
  (define circle (mcons 1 (mcons 0 '())))
  (set-mcdr! (mcdr circle) circle)
  circle)

(define (place-until last-marble n-players)
  (let loop ([m 2]
             [player 2]
             [curr-cons (make-circular-list)]
             [scores #hash()])
    (if (> m last-marble)
        scores
        (let-values ([(curr-cons taken)
                      (place-marble m curr-cons)])
          (let ([scores (if (not (null? taken))
                            (hash-update scores player (λ (s) (+ s (+ m taken))) 0)
                            scores)])
            (loop (+ m 1)
                  (if (>= player n-players) 1 (+ player 1))
                  curr-cons
                  scores))))))

(displayln (apply max (hash-values (place-until 71940 465))))

;(displayln (apply max (hash-values (place-until (* 100 71940) 465))))