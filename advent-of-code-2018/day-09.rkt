#lang racket/base

; 465 players; last marble is worth 71940 points

(struct dcons (data prev next) #:mutable #:transparent)

(define dcdr dcons-next)

(define (insert-after d1 data)
  (define d (dcons data d1 (dcons-next d1)))
  (set-dcons-prev! (dcons-next d1) d)
  (set-dcons-next! d1 d)
  d)

(define (delete-after d)
  (define data (dcons-data (dcons-next d)))
  (set-dcons-prev! (dcons-next (dcons-next d)) d)
  (set-dcons-next! d (dcons-next (dcons-next d)))
  data)

(define (make-circular-dlist data)
  (define d (dcons data null null))
  (set-dcons-next! d d)
  (set-dcons-prev! d d)
  d)

;; Returns (values <new-curr-cons> <maybe-taken-marble>)
(define (place-marble marble curr-cons)
  (if (= (modulo marble 23) 0)
      (let ([before (dcons-prev (dcons-prev (dcons-prev (dcons-prev (dcons-prev (dcons-prev (dcons-prev
                                                                                             (dcons-prev curr-cons))))))))])
        (define taken (delete-after before))
        (values (dcons-next before) taken))
      (values (insert-after (dcons-next curr-cons) marble) null)))

(define (place-until last-marble n-players)
  (let loop ([m 2]
             [player 2]
             [curr-cons (insert-after (make-circular-dlist 0) 1)]
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

(displayln (apply max (hash-values (place-until (* 100 71940) 465))))