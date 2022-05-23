#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

(define-struct counter (index tt queue) #:transparent)

;; Naumencu Mihai
;; 322 CD


;; === 1 ===

(define (empty-counter index)
  (make-counter index 0 '()))



;; === 2 ===

(define (tt+ C minutes)
  (struct-copy counter C [tt
                          (+ minutes
                          (match C [(counter index tt queue) tt]))]))


;; === 3 ===

(define (min-tt counters)
  (cond 
      [(null? (cdr counters)) (cons (counter-index (car counters)) (counter-tt (car counters)))]
      [(<= (counter-tt (car counters)) (cdr (min-tt (cdr counters))))
           (cons (counter-index (car counters)) (counter-tt (car counters)))]
      [else (min-tt (cdr counters))]
  )
)


;; === 4 ===

(define (add-to-counter C name n-items)
  (make-counter (counter-index C)
                (+ n-items (counter-tt C))
                (append (counter-queue C) (list (cons name n-items)))))
  


;; === 5 ===

(define (serve requests C1 C2 C3 C4)
  (define counters (list C1 C2 C3 C4))
  (define big_counters (list C2 C3 C4)); Excluzand C1

  ; Functie care adauga delay
  (define (add-delay index minutes C1 C2 C3 C4)
    (cond
      [(equal? (counter-index C1) index)
          (serve (cdr requests) (tt+ C1 minutes) C2 C3 C4)]
      [(equal? (counter-index C2) index)
          (serve (cdr requests) C1 (tt+ C2 minutes) C3 C4)]
      [(equal? (counter-index C3) index)
          (serve (cdr requests) C1 C2 (tt+ C3 minutes) C4)]
      [(equal? (counter-index C4) index)
          (serve (cdr requests) C1 C2 C3 (tt+ C4 minutes))]))

  ; Functie care adauga persoane in queue
  (define (add-to-queue name n-items C1 C2 C3 C4)
    (cond ; 2 conditii, in functie de n-items (daca clientul poate fi plasat la casa C1 sau nu)
      [(<= n-items ITEMS)
       (cond
         [(equal? (car (min-tt counters)) 1) ;(car (min-tt counters)) ~ indexul necesar
            (serve (cdr requests) (add-to-counter C1 name n-items) C2 C3 C4)]
         [(equal? (car (min-tt counters)) 2)
            (serve (cdr requests) C1 (add-to-counter C2 name n-items) C3 C4)]
         [(equal? (car (min-tt counters)) 3)
            (serve (cdr requests) C1 C2 (add-to-counter C3 name n-items) C4)]
         [(equal? (car (min-tt counters)) 4)
            (serve (cdr requests) C1 C2 C3 (add-to-counter C4 name n-items))])]
      [else
       (cond
         [(equal? (car (min-tt big_counters)) 2)
            (serve (cdr requests) C1 (add-to-counter C2 name n-items) C3 C4)]
         [(equal? (car (min-tt big_counters)) 3)
            (serve (cdr requests) C1 C2 (add-to-counter C3 name n-items) C4)]
         [(equal? (car (min-tt big_counters)) 4)
            (serve (cdr requests) C1 C2 C3 (add-to-counter C4 name n-items))])]))

  ; Implementarea se face prin call-uri la cele 2 functii
  (if (null? requests)
      counters
      (match (car requests)
        [(list 'delay index minutes)
           (add-delay index minutes C1 C2 C3 C4)]
        [(list name n-items)
           (add-to-queue name n-items C1 C2 C3 C4)])))
