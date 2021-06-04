


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
  (define time (current-inexact-milliseconds))
  (let loop
    ([num 2] [sum 0])
    (cond
      [(and (prime? num) (equal? num limit)) (println (+ sum num))]
      [(equal? num limit) (println sum)]
      [(prime? num) (loop (+ num 1) (+ sum num))]
      [else (loop (+ num 1) sum)])
    )
   (println (- (current-inexact-milliseconds) time)))

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
  (define time (current-inexact-milliseconds))
  (define start
    (for/list ([i (in-range 0 limit (ceiling(/ limit num-threads)))])
              i))
  ; List of limits per thread
  (define limits (append (rest start) (list limit)))
  (define start_values (map add1 start))
  (define futures (map sum-primes-threads limits start_values))
  ; Launch all the futures in the list
  (define result (map touch futures))
  (println (apply + result))
  (displayln (- (current-inexact-milliseconds) time)))
