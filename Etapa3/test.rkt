#lang racket
(require racket/match)
(require racket/trace)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

(define-struct counter (index tt et queue) #:transparent)

;; === 6 ===

(define (empty-counter index)
  (make-counter index 0 0 empty-queue)
)


(define (update f counters index)
  (update-helper f counters index '())
)

(define (update-helper f counters index acc)
  (cond
    [(null? counters) (reverse acc)]
    [(equal? (counter-index (first counters)) index)
     (update-helper f
                    (rest counters)
                    index
                    (cons (f (first counters)) acc))]
    [else (update-helper f
                         (rest counters)
                         index
                         (cons (first counters) acc))])
)

(define tt+
  (λ (minutes)
    (λ (C)
      (struct-copy counter C
                   [tt (+ minutes (match C [(counter index tt et queue) tt]))])
    )
  )
)

(define et+
  (λ (minutes)
    (λ (C)
      (struct-copy counter C
                   [et (+ minutes (match C [(counter index tt et queue) et]))]
      )
    )
  )
)


;; === 7 ===

(define (add-to-counter name items)
  (λ (C)
    (struct-copy counter C
        [tt (+ items (match C ((counter index tt et queue) tt)))]
        ; Daca coada e goala, et se schimba, altfel nu
        [et (if (queue-empty? (counter-queue C)) (+ items (counter-et C))
                (counter-et C))
        ]
        [queue (enqueue (cons name items) (counter-queue C))]
    )
  )
)

(define (min-x counters type)
  (cond
    [(equal? type "tt")
      (cond
        [(null? (cdr counters)) (cons (counter-index (car counters)) (counter-tt (car counters)))]
        [(<= (counter-tt (car counters)) (cdr (min-x (cdr counters) "tt")))
            (cons (counter-index (car counters)) (counter-tt (car counters)))]
        [else (min-x (cdr counters) "tt")]
      )
    ]
    [(equal? type "et")
     (cond
        [(null? (cdr counters)) (cons (counter-index (car counters)) (counter-et (car counters)))]
        [(<= (counter-et (car counters)) (cdr (min-x (cdr counters) "et")))
            (cons (counter-index (car counters)) (counter-et (car counters)))]
        [else (min-x (cdr counters) "et")]
      )
    ]
  )
)
(define (min-tt counters)
  (min-x counters "tt")
)
(define (min-et counters)
  (min-x counters "et")
)


;; === 8 ===

(define (remove-first-from-counter C)
  (make-counter (counter-index C); ----------------------------- index
                (cond; ----------------------------------------- tt
                  [(queue-empty? (dequeue (counter-queue C))) 0]
                  ; Timp ramas dupa eliminarea primei persoane
                  [else (- (counter-tt C) (counter-et C))]
                )
                (cond; ----------------------------------------- et
                  [(queue-empty? (dequeue (counter-queue C))) 0]
                  ; Timpul urmatoarei persoana din coada
                  [else (cdr (top (dequeue (counter-queue C))))]
                )
                (dequeue (counter-queue C)); ------------------- queue
  )
)

;; === 9 ===

(define (pass-time-through-counter minutes)
  (λ (C)
    (struct-copy counter C
                 [tt (cond
                       [(queue-empty? (counter-queue C)) 0]
                       [(>= minutes (counter-tt C)) 0]
                       [else (- (counter-tt C) minutes)]
                     )
                 ]
                 [et (cond
                       [(queue-empty? (counter-queue C)) 0]
                       [(>= minutes (counter-et C)) 0]
                       [else (- (counter-et C) minutes)]
                     )
                 ]
                 [queue (cond ; Pentru serve
                          [(queue-empty? (counter-queue C)) (counter-queue C)]
                          [(>= minutes (cdr (top (counter-queue C)))) (dequeue (counter-queue C))]
                          [else (counter-queue C)]
                        )
                 ]
    )
  )
)

(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters '())
)


(define (serve-helper requests fast-counters slow-counters clients-that-left)
  (define all-counters
    (append fast-counters slow-counters))

  (define (update x)
    
    'x
  )

  ; Functia pentru adaugare la coada
  (define (add-person name n-items)
    (if (> n-items ITEMS)
        (serve-helper (cdr requests)
               fast-counters
               (update (add-to-counter name n-items)
                       slow-counters
                       (car (min-tt slow-counters)))
               clients-that-left
        )
        (if (< (car (min-tt (append fast-counters slow-counters))); -- minim dintre toate casele
               (counter-index (car slow-counters))); -- prima casa slow
            (serve-helper (cdr requests)
                   (update (add-to-counter name n-items)
                           fast-counters
                           (car (min-tt fast-counters)))
                   slow-counters
                   clients-that-left
            )
            (serve-helper (cdr requests)
                   fast-counters
                   (update (add-to-counter name n-items)
                           slow-counters
                           (car (min-tt slow-counters)))
                   clients-that-left
            )
        )
    )
  )

  ; Funcita pentru delay
  (define (add-delay index minutes)
    (if (< index (counter-index (car slow-counters))); -- daca e casa fast
        (serve-helper (cdr requests)
               (update (et+ minutes)
                       (update (tt+ minutes)
                               fast-counters
                               index)
                       index)
               slow-counters
        )
        (serve-helper (cdr requests); -------------------------- daca e casa slow
               fast-counters
               (update (et+ minutes)
                       (update (tt+ minutes)
                               slow-counters
                               index)
                       index)
               clients-that-left
        )
    )
  )

  ; Functia pentru ensure
  (define (ensure average)
    (define sum
      (apply + (map (λ(x) (counter-tt x)) all-counters)))

    (define num
      (length all-counters))
    
    (define ttmed
      (/ sum num))

    ; Va fi nevoie sa adaugam case pana cand num >= sum/average
    ; Stiind numarul actual al caselor, putem afla de cate case
    ; este exact nevoie (num - sum/average)

    (if (>= num (/ sum average))
        (serve-helper (cdr requests)
               fast-counters
               slow-counters
               clients-that-left)
        (serve-helper requests
               fast-counters
               (append slow-counters (list (empty-counter (+ (counter-index (car (reverse slow-counters))) 1))))
               ; Creaza un empty-counter pentru ultimul index + 1
               clients-that-left
        )
    )
  )
  
  ; Call-uri catre functii
  (if (null? requests)
      (clients-that-left (append fast-counters slow-counters))
      (match (car requests)
        [x (update x)]
        [(list 'ensure average) (ensure average)]
        [(list name n-items) (add-person name n-items)]
        [(list 'delay index minutes) (add-delay index minutes)]
      )
  )
)






;; ==== test ====

(define Q1 (make-queue '() '() 0 0))
(define Q2 (make-queue '(1 2) '(5 4 3) 2 3))
(define Q3 (make-queue '() '(3 2 1 0) 0 4))
(define Q4 (make-queue '(1) '() 1 0))

(define C1 (empty-counter 1))
(define C2 (make-counter 2 2 2 (queue '((mara . 2)) '() 1 0)))
(define C3 (make-counter 3 12 5 (queue '() '((geo . 7) (lia . 5)) 0 2)))
(define C4 (make-counter 4 7 4 (queue '() '((bogdan . 3) (ana . 2)) 0 2)))
(define C5 (make-counter 5 12 8 (queue '((remus . 6) (vivi . 4)) '() 2 0)))



; Există o formă specială de let care permite radiografierea unei structuri,
; cu evidențierea câmpurilor din interior.
"Match-let"
; (match-let ([(counter index tt queue) C1]) queue)

; Există și o formă specială de lambda, care permite radiografierea
; parametrului funcției.
"Match-lambda"
; (filter (match-lambda [(counter _ tt _) (> tt 5)]) (list C1 C2))

; Echivalent cu
; (filter (lambda (C) (> (counter-tt C) 5)) (list C1 C2))
        
; (map top (map counter-queue (list C1 C2 C3 C4 C5)))
; (map top (filter queue-not-empty? (map counter-queue (list C1 C2 C3 C4 C5))))
; (sort (list C2 C3 C4 C5) compare-counter)
; (map top (filter queue-not-empty? (map counter-queue (sort (list C1 C2 C3 C4 C5) compare-counter))))
; (filter counter-not-empty? (sort (list C1 C2 C3 C4 C5) compare-counter))

(define (queue-not-empty? q)
(if (and (empty? (queue-left q)) (empty? (queue-right q)))
    #f
    #t
  )
)

(define (counter-not-empty? C)
  (if (queue-not-empty? (counter-queue C))
      #t
      #f
  )
)

    (define (passed-counter? C)
      (if (>= 10 (cdr (top (counter-queue C))))
          #t
          #f
      )
    )

; (map cons (map counter-index (filter counter-not-empty? (sort (list C1 C2 C3 C4 C5) compare-counter)))
;      (map car (map top (map counter-queue (filter counter-not-empty? (sort (list C1 C2 C3 C4 C5) compare-counter))))))

;(< C1 C2)

; Compara 2 case dupa nr de items ale primului din coada
(define (compare-counter Ca Cb)
  (cond
    [(and (queue-empty? (counter-queue Ca)) (queue-not-empty? (counter-queue Cb))) #t]
    [(and (queue-empty? (counter-queue Cb)) (queue-not-empty? (counter-queue Ca))) #f]
    [(and (queue-empty? (counter-queue Ca)) (queue-empty? (counter-queue Cb))) #t]
    [(< (cdr (top (counter-queue Ca))) (cdr (top (counter-queue Cb)))) #t]
    [else #f]
  )
)




la testul 10a rezultatul ((#(struct:counter 1 0 0 #(struct:queue () () 0 0))
                           #(struct:counter 2 14 14 #(struct:queue () ((ana . 14)) 0 1))
                           #(struct:counter 3 0 0 #(struct:queue () () 0 0)))
                          #(struct:counter 1 0 0 #(struct:queue () () 0 0))
                          #(struct:counter 2 8 8 #(struct:queue () ((ana . 14)) 0 1))
                          #(struct:counter 3 0 0 #(struct:queue () () 0 0)))
diferă de cel așteptat (() #(struct:counter 1 0 0 #(struct:queue () () 0 0))
                           #(struct:counter 2 8 8 #(struct:queue () ((ana . 14)) 0 1))
                           #(struct:counter 3 0 0 #(struct:queue () () 0 0))) 



la testul 10b rezultatul (((2 . lia))
                          #(struct:counter 1 0 0 #(struct:queue () () 0 0))
                          #(struct:counter 2 2 0 #(struct:queue ((geo . 7)) () 1 0))
                          #(struct:counter 3 0 0 #(struct:queue () () 0 0)))
diferă de cel așteptat (((2 . lia))
                        #(struct:counter 1 0 0 #(struct:queue () () 0 0))
                        #(struct:counter 2 2 2 #(struct:queue ((geo . 7)) () 1 0))
                        #(struct:counter 3 0 0 #(struct:queue () () 0 0))) 



la testul 10c rezultatul (((2 . ana) (1 . lia))

                          #(struct:counter 1 0 -1 #(struct:queue ((geo . 4)) () 1 0))
                          #(struct:counter 2 0 0 #(struct:queue () () 0 0))
                          #(struct:counter 3 1 1 #(struct:queue () ((mia . 6)) 0 1))
                          #(struct:counter 4 0 0 #(struct:queue () () 0 0)))
diferă de cel așteptat (((1 . lia) (2 . ana) (1 . geo))

                        #(struct:counter 1 0 0 #(struct:queue () () 0 0))
                        #(struct:counter 2 0 0 #(struct:queue () () 0 0))
                        #(struct:counter 3 1 1 #(struct:queue () ((mia . 6)) 0 1))
                        #(struct:counter 4 0 0 #(struct:queue () () 0 0))) 

la testul 10e rezultatul (((1 . adi))

                          #(struct:counter 1 0 0 #(struct:queue () () 0 0))
                          #(struct:counter 2 0 0 #(struct:queue () () 0 0)))
diferă de cel așteptat (((2 . adi))

                        #(struct:counter 1 3 3 #(struct:queue () () 0 0))
                        #(struct:counter 2 0 0 #(struct:queue () () 0 0))) 






rezultatul (((1 . mia) (3 . mara) (2 . ana) (2 . ion) (1 . gigi) (1 . lia) (2 . dan))
            #(struct:counter 1 2 2 #(struct:queue ((nera . 4)) () 1 0))
            #(struct:counter 2 0 -1 #(struct:queue ((leo . 1)) () 1 0))
            #(struct:counter 3 0 0 #(struct:queue () () 0 0))
            #(struct:counter 4 2 2 #(struct:queue () ((ada . 7)) 0 1))
            #(struct:counter 5 3 3 #(struct:queue () ((milu . 8)) 0 1)))

diferă de cel așteptat (((1 . mia) (3 . mara) (2 . ana) (2 . ion) (1 . gigi) (1 . lia) (2 . dan) (2 . leo))
                        #(struct:counter 1 2 2 #(struct:queue ((nera . 4)) () 1 0))
                        #(struct:counter 2 0 0 #(struct:queue () () 0 0))
                        #(struct:counter 3 0 0 #(struct:queue () () 0 0))
                        #(struct:counter 4 2 2 #(struct:queue () ((ada . 7)) 0 1))
                        #(struct:counter 5 3 3 #(struct:queue () ((milu . 8)) 0 1))) 











