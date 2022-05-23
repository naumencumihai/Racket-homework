#lang racket
(require racket/match)
(require racket/trace)

(provide (all-defined-out))

(define ITEMS 5)

(define-struct counter (index tt et queue) #:transparent)



;; === 1 ===

(define (empty-counter index)
  (make-counter index 0 0 '()))

;;TEST

(define C1 (empty-counter 1))
(define C2 (empty-counter 2))
(define C3 (empty-counter 3))
(define C4 (empty-counter 4))
(define C5 (make-counter 5 12 8 '((remus . 6) (vivi . 4))))

;; === 2 ===

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

(define add-to-counter
  (λ (name n-items)
    (λ (C)
      (struct-copy counter C
        [tt (+ n-items (match C ((counter index tt et queue) tt)))]
        [et (if (equal? (counter-queue C) '()) (+ n-items (counter-et C))
                (cdr (car (counter-queue C))))
        ]
        [queue (append (match C ((counter index tt et queue) queue))
                       (list(cons name n-items)))]
      )
    )
  )
)
; Exemplu: (update (add-to-counter 'a' 3) (list C1 C2 C3 C4 C5) 5)
; Se adauga (a . 3) la finalul cozii casei 5

; type - specifica tipul de minim dorit
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

;; === 3 ===

(define (min-tt counters)
  (min-x counters "tt")
)

;; === 4 ===

(define (min-et counters)
  (min-x counters "et")
)

;; === 5 ===

(define (remove-first-from-counter C)
  (make-counter (counter-index C); ----------------------------- index
                (cond; ----------------------------------------- tt
                  [(equal? (cdr (counter-queue C)) '()) 0]
                  ; Timp ramas deoarece se elimina delay-urile
                  [else (- (counter-tt C) (counter-et C))]
                )
                (cond; ----------------------------------------- et
                  [(equal? (cdr (counter-queue C)) '()) 0]
                  ; Timpul urmatoarei persoana din coada
                  [else (cdr (car (cdr (counter-queue C))))]
                )
                (cdr (counter-queue C)); ----------------------- queue
  )
)

;; === 6 ===

(define (serve requests fast-counters slow-counters)
  (define all-counters
    (append fast-counters slow-counters))
  
  (define (add-person name n-items)
    (if (>= n-items ITEMS)
        (serve (cdr requests)
               fast-counters
               (update (add-to-counter name n-items)
                       slow-counters
                       (car (min-tt slow-counters)))
        )
        (if (< (car (min-tt (append fast-counters slow-counters))); -- minim dintre toate casele
               (counter-index (car slow-counters))); -- prima casa slow
            (serve (cdr requests)
                   (update (add-to-counter name n-items)
                           fast-counters
                           (car (min-tt fast-counters)))
                   slow-counters)
            (serve (cdr requests)
                   fast-counters
                   (update (add-to-counter name n-items)
                           slow-counters
                           (car (min-tt slow-counters))))
        )
    )
  )

  (define (add-delay index minutes)
    (if (< index (counter-index (car slow-counters))); -- daca e casa fast
        (serve (cdr requests)
               (update (et+ minutes)
                       (update (tt+ minutes)
                               fast-counters
                               index)
                       index)
               slow-counters
        )
        (serve (cdr requests); -------------------------- daca e casa slow
               fast-counters
               (update (et+ minutes)
                       (update (tt+ minutes)
                               slow-counters
                               index)
                       index)
        )
    )
  )

  (define (remove)
    ; Labels
    (define filtered
      (filter (λ (x) (pair? (counter-queue x))) all-counters))

    (define min-et-index
      (if (equal? filtered '()) 0
      (car (min-et filtered))))

    (define first-slow-index
      (counter-index (car slow-counters)))

    ; Funcite care returneaza casa cu indicele specificat
    (define (get-counter index counters)
      (if (< index (counter-index (car slow-counters))) 
          (list-ref counters (- index 1))
          (list-ref counters (- index first-slow-index))
      )
    )

    ; Urmatoarele 2 functii inlocuiesc o casa specificata, construind o noua lista
    ; cu inceputul si finalul listei initiale lipite (folosind append pentru a alatura
    ; aceste 2 "bucati" in jurul casei specificate)
    ; --------------
    (define (replace-fast-counter index counter)
      (append (append (take fast-counters (- index 1)) (list counter))
              (reverse(take(reverse fast-counters) (-(-(counter-index (car slow-counters)) index) 1))))
      
    )

    (define (replace-slow-counter index counter)
      (append (append (take slow-counters (- index first-slow-index)) (list counter))
              (reverse(take(reverse slow-counters) (-(counter-index(car (reverse slow-counters))) index)))
      )
    )
    ; --------------
    
    (if (equal? filtered '())
    ; Daca nu exista case cu clienti
      (serve (cdr requests)
             fast-counters
             slow-counters)
    ; Daca exista cel putin o casa cu clienti
      (if (< min-et-index
             first-slow-index)
          ; Daca casa cu et minim este fast
          (serve (cdr requests)
                 (replace-fast-counter min-et-index
                                       (remove-first-from-counter (get-counter min-et-index fast-counters)))
                 slow-counters
          )
          ; Daca casa cu et minim este slow
          (serve (cdr requests)
                 fast-counters
                 (replace-slow-counter min-et-index
                                       (remove-first-from-counter (get-counter min-et-index slow-counters))))
      )
    )
  )

  (define (ensure average)
    (define sum
      (apply + (map (λ(x) (counter-tt x)) all-counters))
    )

    (define num
      (length all-counters))
    
    (define ttmed
      (/ sum num)
    )

    ; Va fi nevoie sa adaugam case pana cand num >= sum/average
    ; Stiind numarul actual al caselor, putem afla de cate case
    ; este exact nevoie (num - sum/average)

    (if (>= num (/ sum average))
        (serve (cdr requests)
               fast-counters
               slow-counters)
        (serve requests
               fast-counters
               (append slow-counters (list (empty-counter (+ (counter-index (car (reverse slow-counters))) 1))))
        )
    )
  ) 

  ; Call-uri catre functii
  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)
        [(list 'ensure average) (ensure average)]
        [(list name n-items) (add-person name n-items)]
        [(list 'delay index minutes) (add-delay index minutes)]
        [(list 'remove-first) (remove)]
      )
  )
)


(trace serve)

#|

la testul 6j rezultatul (#(struct:counter 1 7 7 ((cristi . 4)))
                         #(struct:counter 2 11 3 ((dana . 3) (eliza . 7)))
                         #(struct:counter 3 5 5 ())
                         #(struct:counter 4 0 0 ())
                         #(struct:counter 5 0 0 ()))
diferă de cel așteptat (#(struct:counter 1 7 7 ((cristi . 4)))
                        #(struct:counter 2 11 4 ((dana . 3) (eliza . 7)))
                        #(struct:counter 3 5 5 ())
                        #(struct:counter 4 0 0 ())
                        #(struct:counter 5 0 0 ())) 


|#













