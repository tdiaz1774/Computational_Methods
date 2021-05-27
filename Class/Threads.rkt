#|
Examples using threads

Jorge Cabiedes
26/05/2021
|#

#lang racket

(define (make-thread name limit)
  (thread (lambda ()
  (let loop
    ([n 0])
    (cond
      [(< n limit)
       (printf "Thread ~a | Number ~a\n" name n)
       ; Force the thread to wait, let others run too
       (sleep (random))
       (loop (add1 n))]
      [else (printf "Finished!\n")])))))

(define (main num-threads)
  (define my-thread-1 (mak num-threads)e-thread "One" 10))
  (define my-thread-2 (make-thread "Two" 10))
  (define my-thread-3 (make-thread "Three" 10))
  (for ([n 20])
       (sleep (random))
       (printf "In main thread: ~a\n" n))
  (thread-wait my-thread-1)
  (thread-wait my-thread-2)
  (thread-wait my-thread-3)
  (printf "MAIN THREAD FINISHED\n"))

