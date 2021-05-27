#|
Functions to sum all the prime numbers smaller than a limit
|#

#lang racket


(define (prime? n)
  (let loop ((d 2))
    (cond
      ((<= n 3) (> n 1))
      ((< n (* d d)) #t)
      ((zero? (modulo n d)) #f)
      (else (loop (+ d 1))))))

(define (sum-primes limit)
  ; Loop from 0 to limit
  ; check each number for primality
  ; add to total if it is prime
  (let loop
    ([num 2] [sum 0])
    (cond
      [(and (prime? num) (equal? num limit)) (+ sum num)]
      [(equal? num limit) sum]
      [(prime? num) (loop (+ num 1) (+ sum num))]
      [else (loop (+ num 1) sum)])
    ))

(define (sum-primes-threads limit [start 2])
  (future (lambda ()
  (let loop
    ([num start] [sum 0])
    (cond
      [(and (prime? num) (equal? num limit)) (+ sum num)]
      [(equal? num limit) sum]
      [(prime? num) (loop (+ num 1) (+ sum num))]
      [else (loop (+ num 1) sum)])
    ))))

(define (main num-threads limit)
  ; List of starts per thread
  (define n '(1 11 21))
  ; List of limits per thread
  (define l '(10 20 30))
  (define futures (map sum-primes-threads l n))
  ; Launch all the futures in the list
  (define result (map touch futures))
  (println (apply + result)))






