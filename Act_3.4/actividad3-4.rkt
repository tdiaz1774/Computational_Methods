;;; Problems

;;; 1.- No funciona el regex
;;; 2.- Modificar write-file function para que
;;; -> Empieze a escribir en la ultima linea del documento
;;; -> Pueda escribir correctamente una lista de listas

#lang racket

; Indicate the functions available in this script

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
      [(regexp-match? #px"^\"[^\"]*\":" word) (values (car (regexp-match #px"^\"[^\"]*\":" word)) 'key)]
      ;;; String
      [(regexp-match? #px"^\"[^\"]*\"" word) (values (car (regexp-match #px"^\"[^\"]*\"" word)) 'string)]
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


(define (highlighter in-file-path)

    (define file_name (substring in-file-path 0 (- (string-length in-file-path) 5)))
    (define out-file-path (string-append file_name ".html"))
    ;;; Escribir el boiler plate de html

    (define open_html (list 
    "<!DOCTYPE html>\n" 
    "<html lang=\"en\">\n" 
    "<head>\n" 
    "    <meta charset=\"UTF-8\">\n" 
    "    <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">\n" 
    "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n" 
    "    <title>Document</title>\n" 
    "    <style>\n" 
    "        .number{ color: red; }\n" 
    "        .key{ color: green; }\n" 
    "        .string{ color: orange; }\n"
    "        .whitespace{ display: none; }\n" 
    "    </style>\n" 
    "</head>\n"
    "<body>\n"
    "<pre>\n"))

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
    "</pre>\n"
    "</body>\n"
    "</html>\n"))

    ;;; Generate file in a list

    (define complete_file (apply append (list open_html json_html close_html)))
    (write-file out-file-path complete_file)
    out-file-path
    )

(define (highlight_files files)
(map highlighter files)
)


(highlight_files '("hello.json"))
