
#lang racket


;;; Get files in a folder
(define (get_json_files folder-path)
    ;;; Get files that are inside a directory
    (define files (directory-list folder-path))
    ;;; Loop through the files and select only the ones that end in ".json"
    (define file (map (lambda (v)
           (regexp-match #px".*.json$" v))
           files))
    (define result (apply append (remove* '(#f) file)))
    result)

(define (main num-threads folder-path)
    ;;; Get all json files in folder
    (define json_files (get_json_files folder-path))
    (println json_files)
    (define files_per_thread
            (for/list ([i (in-range 0 (length json_files) (ceiling(/ json_files num-threads)))])
                    i))

    (define (split-by lst n)
        (if (not (empty? lst))
            (cons (take lst n) (split-by (drop lst n) n))
            '() ))

    (display (split-by json_files 2))
    ;;; ;;; Create threads
    ;;; (define futures (map highlighter json_files files_per_thread))
    ;;; ;;; Launch all the futures in the list
    ;;; (define result (map touch futures))
    
)

(main 4 "../Act_5.2")

;;; (define (write-file num)
;;;   (call-with-output-file (string-join '((number->string num) ".html"))
;;;     #:exists 'truncate
;;;     (lambda (out)
;;;       (let loop
;;;         ([lst data])
;;;         (cond
;;;           [(not (empty? lst))
;;;              (display (car lst) out)
;;;              (loop (cdr lst))])))))
