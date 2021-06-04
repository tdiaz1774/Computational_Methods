
#lang racket

(define (main folder-path)
    ;;; Get files that are inside a directory
    (define files (directory-list folder-path))
    ;;; Loop through the files and select only the ones that end in ".json"
    (define file (map (lambda (v)
           (regexp-match #px".*.json$" v))
           files))
    (define result (apply append (remove* '(#f) file)))
    (car result))
    
           
(main "../Implementacion de metodos computacionales")
