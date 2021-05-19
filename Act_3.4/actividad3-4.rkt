#lang racket

(require racket/trace)

; Indicate the functions available in this script
(provide main)

;;; Write to file function

(define (write-file out-file-path data)
  (call-with-output-file out-file-path
    #:exists 'truncate
    (lambda (out)
      (let loop
        ([lst data])
        (cond
          [(not (empty? lst))
             (displayln (car lst) out)
             (loop (cdr lst))])))))


;;;  Read file function 

(define (read-file in-file-path)
  (call-with-input-file in-file-path
    (lambda (in)
      (let loop
        ([line (read-line in)]
         [result empty])
        (if (eof-object? line)
            result
            (loop (read-line in) (append result (list line))))))))

(define (strip_whitespace lst)
(let loop
    ([lst lst] [result empty])
    (if (empty? lst)
        result
    (loop
        (cdr lst)
        (append result (regexp-match #px"\\(?>-?\\(?>0|[1-9]\\d*\\)\\(?>\\.\\d+\\)?\\(?>[E|e]-?\\(?>-?\\(?>0|[1-9]\\d*\\)\\(?>\\.\\d+\\)?\\)\\)?\\)" (car lst)))
    ))))



(define (main in-file-path out-file-path)
    ;;; Leer el json
    (define data (read-file in-file-path))
    (println data)
    ;;; Escribir el boiler plate de html
    ;;; Iterar la lista de elementos de json y crear su partes html
    (define test (strip_whitespace (list 1 2 3)))
    (println test))
    ;;; Escribir elementos de json con html en file
    ;;; Escribir closing tags
    ;;; (write-file out-file-path result))


(main "text.txt" "new_text.txt")
