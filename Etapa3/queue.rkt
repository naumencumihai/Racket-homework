#lang racket
(require racket/match)
(require racket/trace)

(provide empty-queue)
(provide queue-empty?)
(provide queue-not-empty?)
(provide enqueue)
(provide dequeue)
(provide top)

(provide (struct-out queue)) ; pentru testare

(define-struct queue (left right size-l size-r) #:transparent) 

;; Naumencu Mihai
;; 322 CD

;; === 1 ===

(define empty-queue
  (make-queue '() '() 0 0)
)


;; === 2 ===

(define (queue-empty? q)
  (if (and (empty? (queue-left q)) (empty? (queue-right q)))
    #t
    #f
  )
)

; Folosita in functia serve din 'supermarket.rkt'
(define (queue-not-empty? q)
  (if (and (empty? (queue-left q)) (empty? (queue-right q)))
    #f
    #t
  )
)


;; === 3 ===

(define (enqueue x q)
  (struct-copy queue q [right (cons x (queue-right q))]
                       [size-r (+ (queue-size-r q) 1)]
  )
)


;; === 4 ===

(define (dequeue q)
  (if (empty? (queue-left q))
     ; Daca stiva left este goala
    (struct-copy queue q [left (cdr (reverse (queue-right q)))]
                         [right '()]
                         [size-l (- (queue-size-r q) 1)]
                         [size-r 0]
    ); Daca stiva left are cel putin un element
    (struct-copy queue q [left (cdr (queue-left q))]
                         [size-l (- (queue-size-l q) 1)]
    )
  )
)


;; === 5 ===

(define (top q)
  (if (empty? (queue-left q))
    (car (reverse (queue-right q))); Ultimul element din stiva right
    (car (queue-left q)); Primul element din stiva left
  )
)
