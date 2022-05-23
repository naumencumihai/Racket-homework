#lang racket
(require racket/match)
(require racket/trace); Pentru testare
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

(define-struct counter (index tt et queue) #:transparent)

;; Naumencu Mihai
;; 322 CD

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
                       [else (- (counter-tt C) minutes)]
                     )
                 ]
                 [et (cond
                       [(queue-empty? (counter-queue C)) 0]
                       [else (- (counter-et C) minutes)]
                     )
                 ]
    )
  )
)

; Functia completa, care afecteaza si queue
; (Poate iesi doar cate un client per casa)
(define (pass-time-through-counter-full minutes)
  (λ (C)
    (struct-copy counter C
                 [tt (cond
                       [(>= minutes (counter-tt C)) 0]  
                       [else (- (counter-tt C) minutes)]
                     )
                 ]
                 [et (cond
                       [(>= minutes (counter-et C))
                         (if (queue-empty? (counter-queue C))
                           0
                           (if (queue-empty? (dequeue (counter-queue C)))
                             0
                             (- (cdr (top (dequeue (counter-queue C))))(- minutes (counter-et C)))
                             ; ^ (et negativ cand au timp sa iasa mai multi clienti) - bad design
                           )
                         )
                       ]
                       [else (- (counter-et C) minutes)]
                     )
                 ]
                 [queue (cond ; Pentru serve
                          [(queue-empty? (counter-queue C)) (counter-queue C)]
                          [(>= minutes (counter-et C)) (dequeue (counter-queue C))]
                          [else (counter-queue C)]
                        )
                 ]
    )
  )
)


;; === 10 ===

(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters '())
)

(define (serve-helper requests fast-counters slow-counters clients-that-left)
  (define all-counters
    (append fast-counters slow-counters)
  )

  ; Functia pentru trecerea timpului
  ; (Functia presupune ca dupa x minute poate iesi doar cate un client per casa)
  (define (pass-time x)
    ; Compara 2 case dupa nr de items ale primului din coada
    (define (compare-counter Ca Cb)
      (cond
        [(and (queue-empty? (counter-queue Ca)) (queue-not-empty? (counter-queue Cb))) #t]
        [(and (queue-empty? (counter-queue Cb)) (queue-not-empty? (counter-queue Ca))) #f]
        [(and (queue-empty? (counter-queue Ca)) (queue-empty? (counter-queue Cb))) #t]
        [(< (counter-et Ca) (counter-et Cb)) #t]
        [else #f]
      )
    )
    
    ; Argument pentru functia flter
    (define (counter-not-empty? C)
      (if (queue-empty? (counter-queue C))
          #f
          #t
      )
    )
    
    ; Argument pentru functia filter (daca a trecut cineva de casa)
    (define (passed-counter? C)
      (if (>= x (counter-et C))
          #t
          #f
      )
    )

    ; Lista clientilor care au trecut de case dupa x minute
    (define top-list
      (map car (map top (map counter-queue (filter passed-counter? (filter counter-not-empty? (sort all-counters compare-counter))))))
    )

    ; Lista indexilor caselor (cele unde a trecut cineva)
    (define index-list
      (map counter-index (filter passed-counter? (filter counter-not-empty? (sort all-counters compare-counter))))
    )
    
    (define (generate-clients-list l)
      (append l (map cons index-list top-list))
    )

    (serve-helper (cdr requests)
                  (map (pass-time-through-counter-full x) fast-counters)
                  (map (pass-time-through-counter-full x) slow-counters)
                  (generate-clients-list clients-that-left)
    )
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

  ; Functia pentru delay
  (define (add-delay index minutes)
    (if (< index (counter-index (car slow-counters))); -- daca e casa fast
        (serve-helper (cdr requests)
               (update (et+ minutes)
                       (update (tt+ minutes)
                               fast-counters
                               index)
                       index)
               slow-counters
               clients-that-left
        )
        (serve-helper (cdr requests); ------------------- daca e casa slow
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
      (cons clients-that-left (append fast-counters slow-counters))
      (match (car requests)
        [(list 'ensure average) (ensure average)]
        [(list name n-items) (add-person name n-items)]
        [(list 'delay index minutes) (add-delay index minutes)]
        [x (pass-time x)]
      )
  )
)

; Pentru testare
;(trace serve-helper)
