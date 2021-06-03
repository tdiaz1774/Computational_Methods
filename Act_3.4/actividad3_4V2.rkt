#lang racket

(define (main in-file-path out-file-path)

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

  (define close_html (list
    "</pre>"
    "</body>"
    "</html>"))

  (define data (read-file in-file-path))
  
  (define json (apply_regex data))

  (define complete_file (apply append (list open_html close_html)))
  (write-file out-file-path complete_file)
  json 
  )

(define (write-file out-file-path data)
  (call-with-output-file out-file-path
    #:exists 'truncate
    (lambda (out)
      (let loop
        ([lst data])
        (cond
          [(not (empty? lst))
           (displayln(car lst) out)
           (loop (cdr lst))])))))

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
      (loop (cdr lst) (append result (list (identify-object (car lst)))))
      )))

(define (indentify-object word)
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
          [(regexp-match? #px"^null" word) (values (car (regexp-match #px"^null" word)) 'null)]
          [(regexp-match? #px"^true" word) (values (car (regexp-match #px"^true" word)) 'true)]
          [(regexp-match? #px"^false" word) (values (car (regexp-match #px"^false" word)) 'false)]
          [(regexp-match? #px"^[{]" word) (values (car (regexp-match #px"^[{]" word)) 'curly_braces_open)]
          [(regexp-match? #px"^[}]" word) (values (car (regexp-match #px"^[}]" word)) 'curly_braces_close)]
          [(regexp-match? #px"^[[]" word) (values (car (regexp-match #px"^[[]" word)) 'bracket_open)]
          [(regexp-match? #px"^[]]" word) (values (car (regexp-match #px"^[]]" word)) 'bracket_close)]
          [(regexp-match? #px"^," word) (values (car (regexp-match #px"^," word)) 'comma)]
          [(regexp-match? #px"^\\s+" word) (values (car (regexp-match #px"^\\s+" word)) 'whitespace)]
          [else (values " " 'error)]
          )])
        (loop (substring word (string-lenght token))
              (append lst (list (list token type))))))))


(main "input.json" "index.html")
