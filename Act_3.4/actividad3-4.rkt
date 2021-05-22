
#lang racket

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
             (display (car lst) out)
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


(define (apply_regex lst)
(let loop
    ([lst lst] [result empty])
    (if (empty? lst)
        result
    (loop
        (cdr lst)
        (append result (list (identify-object (car lst))))
    ))))

(define (identify-object word)
(let loop
  ([word word] [lst empty])
  (if (string=? word "")
    lst
  (let-values ([(token type)
    (cond
      ;;; Key
      [(regexp-match? #px"^\"[^\"]*\"[\\s]*:" word) (values (car (regexp-match #px"^\"[^\"]*\"[\\s]*:" word)) 'key)]
      ;;; String
      [(regexp-match? #px"^\"[^\"]*\"[\\s]*" word) (values (car (regexp-match #px"^\"[^\"]*\"[\\s]*" word)) 'string)]
      ;;; Number
      [(regexp-match? #px"^(?>-?(?>0|[1-9][0-9]*)(?>\\.[0-9]+)?(?>[eE][+-]?[0-9]+)?)" word) (values (car (regexp-match #px"^(?>-?(?>0|[1-9][0-9]*)(?>\\.[0-9]+)?(?>[eE][+-]?[0-9]+)?)" word)) 'number)]
      ;;; Null
      [(regexp-match? #px"^null" word) (values (car (regexp-match #px"^null" word)) 'null)]
      ;;; True
      [(regexp-match? #px"^true" word) (values (car (regexp-match #px"^true" word)) 'true)]
      ;;; False
      [(regexp-match? #px"^false" word) (values (car (regexp-match #px"^false" word)) 'false)]
      ;;; {}
      [(regexp-match? #px"^[{]" word) (values (car (regexp-match #px"^[{]" word)) 'curly_braces_open)]
      [(regexp-match? #px"^[}]" word) (values (car (regexp-match #px"^[}]" word)) 'curly_braces_close)]
      ;;; []
      [(regexp-match? #px"^[[]" word) (values (car (regexp-match #px"^[[]" word)) 'bracket_open)]
      [(regexp-match? #px"^[]]" word) (values (car (regexp-match #px"^[]]" word)) 'bracket_close)]
      ;;; Comma
      [(regexp-match? #px"^," word) (values (car (regexp-match #px"^," word)) 'comma)]
      ;;; whitespace
      [(regexp-match? #px"^\\s+" word) (values (car (regexp-match #px"^\\s+" word)) 'whitespace)]
      ;;; Else
      [else (values " " 'error)]
    )])
    (loop (substring word (string-length token))
      (append lst (list (list token type))))))))

(define (slist->string slst)
  (string-join (map symbol->string slst) " "))

;;; ("{" curly_braces) -> <span="curly_braces">"}"</span>"

(define (tokentype_html token_type)
(define type_string (slist->string (cdr token_type)))
(if (string=? type_string "whitespace")
  (car token_type)
(string-append "<span class=\"" (slist->string (cdr token_type)) "\">" (car token_type) "</span>" )))

;;; linea

(define (line_html line)
(let loop
    ([line line] [result empty])
    (if (empty? line)
        result
    (loop
        (cdr line)
        (append result (list (tokentype_html (car line))))))))

;;; Json

(define (json_to_html json)
  (let loop
    ([json json] [result empty])
    (if (empty? json)
      result
    (loop
        (cdr json)
        (append result (list (line_html (car json))))))))


(define (main in-file-path out-file-path)

    (define time (current-inexact-milliseconds))

    
    ;;; Escribir el boiler plate de html

    (define open_html (list 
    "<!DOCTYPE html>" 
    "<html lang=\"en\">" 
    "<head>" 
    "    <meta charset=\"UTF-8\">" 
    "    <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">" 
    "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">" 
    "    <title>Document</title>" 
    "    <style>" 
    "        .number{ color: red; }" 
    "        .key{ color: green; }" 
    "        .string{ color: orange; }"
    "        .whitespace{ display: none; }" 
    "    </style>" 
    "</head>"
    "<body>"
    "<pre>"))

    ;;; Leer el json

    (define data (read-file in-file-path))

    ;;; Aplicar regex

    (define json (apply_regex data))
    

    ;;; Iterar la lista de elementos de json y crear su partes html

    (define json_html_list (json_to_html json))

    ;;; Volver todo una lista
    
    (define json_html (apply append json_html_list))


    ;;; Escribir closing tags

    (define close_html (list
    "</pre>"
    "</body>"
    "</html>"))

    ;;; Generate file in a list

    (define complete_file (apply append (list open_html json_html close_html)))
    (write-file out-file-path complete_file)
    (displayln (- (current-inexact-milliseconds) time))
    )


(main "input.json" "index.html")
