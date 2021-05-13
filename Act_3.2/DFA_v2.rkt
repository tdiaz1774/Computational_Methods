#|
Jorge Cabiedes Acosta a01024053
Tomas Diaz Servin A01637531
Mateo Gonzalez Cosio A01023938

Program which implements a Deterministic Finite Automata (DFA)
to determine token type of an entry arethmetic function.
08/04/2021

|#
#lang racket
(require rackunit)
; Import necesary to view the test results
(require rackunit/text-ui)
(require racket/trace)

(define (validate-string input-string dfa)
    "Determine if the input string is accepted by the dfa
    Ex: (validate-string 'abababa' (list accept-ab 'q0 '(q2)))
    Arguments:
    input-string - string
    dfa - list with these elements
        * transition function
        * start state
        * list of accept states
    Return: boolean"

    (let loop ;recursive function
        ([lst  (remove* (list #\space " ") (string->list input-string))] ; removes spaces from input string
        [state (second dfa)] ; calling second element in DFA list (start state)
        [token-list empty] ; list where token-types are stored once found
        [token-elements empty] ; list where elements are stored after their token types are defined
        [transition (first dfa)]) ; calling first elemnt of DFA list (transition function accept-arithmetic)
       
        (if (empty? lst)
            (if (member state (third dfa)) 
                (append token-list (list (list (list->string token-elements)state))) ; if current start state is a member of accept state list, add token-type to token-list
                 #f)
            (let-values 
                ([(state token-type) (transition state (first lst))]) ; define variabble values with atributes state and transition
                (loop  ; recursive call of function loop
                    (rest lst) ; remaining elements in lst
                    state ; new start state at time of call
                    (if token-type
                        (append token-list (list (list (list->string token-elements)token-type))) ; if a token-type was returned add to token-list, if not pass given token-list
                        token-list)
                    
                    (if token-type
                        (list (first lst))
                        (append token-elements (list (first lst)))) ; if a token-type is returned, also return list that contains token-elements

                    transition))))) ; calls transition function

(define (accept-arithmetic state symbol) ;transition function
    (let
        ([ops (list #\= #\+ #\* #\/ #\^)])
        (cond
            [(eq? state 'q0) (cond 
                [(char-numeric? symbol) (values 'int #f)]
                [(char-alphabetic? symbol) (values 'var #f)] 
                [(member symbol ops) (values 'invalid #f)]
                [(eq? symbol #\() (values 'par_0 #f)]
                [(eq? symbol #\)) (values 'invalid #f)]
                [(eq? symbol #\-) (values 'negative #f)]
                [(eq? symbol #\.) (values 'invalid #f)]
                [else (values 'invalid #f)])]
            [(eq? state 'int) (cond 
                [(char-numeric? symbol) (values 'int #f)]
                [(char-alphabetic? symbol) (values 'invalid #f)] 
                [(member symbol ops) (values 'op 'int)]
                [(eq? symbol #\() (values 'invalid #f)]
                [(eq? symbol #\)) (values 'par_1 'int)]
                [(eq? symbol #\-) (values 'negative 'int)]; integer is found
                [(eq? symbol #\.) (values 'decimal #f)]
                [else (values 'invalid #f)])]
            [(eq? state 'decimal) (cond 
                [(char-numeric? symbol) (values 'float #f)] ; decimal point is found
                [(char-alphabetic? symbol) (values 'invalid #f)] 
                [(member symbol ops) (values 'invalid #f)]
                [(eq? symbol #\() (values 'invalid #f)]
                [(eq? symbol #\)) (values 'invalid #f)]
                [(eq? symbol #\-) (values 'invalid #f)]
                [(eq? symbol #\.) (values 'invalid #f)]
                [else (values 'invalid #f)])]
            [(eq? state 'float) (cond 
                [(char-numeric? symbol) (values 'float #f)]
                [(char-alphabetic? symbol) (values 'invalid #f)]
                [(member symbol ops) (values 'op 'float)] ; float is found
                [(eq? symbol #\() (values 'invalid #f)]
                [(eq? symbol #\)) (values 'par_1 'float)] ; float inside parenthesis is found
                [(eq? symbol #\-) (values 'negative 'float)] ; negative float is found
                [(eq? symbol #\.) (values 'invalid #f)]
                [else (values 'invalid #f)])]
            [(eq? state 'var) (cond 
                [(char-numeric? symbol) (values 'var #f)]
                [(char-alphabetic? symbol) (values 'var #f)]
                [(member symbol ops) (values 'op 'var)] ; variable is found
                [(eq? symbol #\() (values 'invalid #f)]
                [(eq? symbol #\)) (values 'par_1 'var)] ; variable is found inside of parenthesis
                [(eq? symbol #\-) (values 'negative 'var)] ; negative variable is found
                [(eq? symbol #\.) (values 'invalid #f)]
                [else (values 'invalid #f)])]
            [(eq? state 'op) (cond 
                [(char-numeric? symbol) (values 'int 'op)] ; operator is found before int
                [(char-alphabetic? symbol) (values 'var 'op)] ; operator is found before variable
                [(member symbol ops) (values 'invalid #f)]
                [(eq? symbol #\() (values 'par_0 'op)] ; operator is found inside parenthesis
                [(eq? symbol #\)) (values 'invalid #f)]
                [(eq? symbol #\-) (values 'invalid #f)]
                [(eq? symbol #\.) (values 'invalid #f)]
                [else (values 'invalid #f)])]
            [(eq? state 'par_0) (cond 
                [(char-numeric? symbol) (values 'int 'par_0)] ; ( is found
                [(char-alphabetic? symbol) (values 'var 'par_0)] ; ( is found before variable
                [(member symbol ops) (values 'invalid #f)]
                [(eq? symbol #\() (values 'par_0 'par_0)] ; is found inside another (
                [(eq? symbol #\)) (values 'invalid #f)]
                [(eq? symbol #\-) (values 'negative 'par_0)] ; found before a negatve sign
                [(eq? symbol #\.) (values 'invalid #f)]
                [else (values 'invalid #f)])]
            [(eq? state 'par_1) (cond 
                [(char-numeric? symbol) (values 'invalid #f)]
                [(char-alphabetic? symbol) (values 'invalid #f)]
                [(member symbol ops) (values 'op 'par_1)] ; ) found after operator
                [(eq? symbol #\() (values 'invalid #f)]
                [(eq? symbol #\)) (values 'par_1 'par_1)] ; ) found before parenthesis is closed
                [(eq? symbol #\-) (values 'negative 'par_1)] ; - is outside )
                [(eq? symbol #\.) (values 'invalid #f)]
                [else (values 'invalid #f)])]
            [(eq? state 'negative) (cond 
                [(char-numeric? symbol) (values 'int 'op)] ; negative is found before int
                [(char-alphabetic? symbol) (values 'var 'op)] ;negative is found before variable
                [(member symbol ops) (values 'invalid #f)]
                [(eq? symbol #\() (values 'par_0 'op)] ; found before (
                [(eq? symbol #\)) (values 'invalid #f)]
                [(eq? symbol #\-) (values 'invalid #f)]
                [(eq? symbol #\.) (values 'invalid #f)]
                [else (values 'invalid #f)])]
            [(eq? state 'invalid) (values 'invalid #f)]))) ; defines invalid state


(define (arithmetic-lexer input-string) ; MAIN FUNCTION in charge of calling the function containing DFA and recursive properties
    (validate-string input-string (list accept-arithmetic 'q0 (list 'int 'var 'par_1 'space 'float)))) ; calling DFA function (validate-string) & transition function (accecpt-arithmetic)

(define test-arithmetic-lexer
    (test-suite
        " Test function: arithmetic-lexer"
        (check-equal? (arithmetic-lexer "2") '(("2" int)) "Single digit")
        (check-equal? (arithmetic-lexer "261") '(("261" int)) "Multi digit int")
        (check-equal? (arithmetic-lexer "5.23") '(("5.23" float)) "Single float")
        (check-equal? (arithmetic-lexer ".23") #f "Incorrect float")
        (check-equal? (arithmetic-lexer "2.2.3") #f "Incorrect float")
        (check-equal? (arithmetic-lexer "data") '(("data" var)) "Single variable")
        (check-equal? (arithmetic-lexer "data34") '(("data34" var)) "Single variable")
        (check-equal? (arithmetic-lexer "34data") #f "Incorrect variable")
        (check-equal? (arithmetic-lexer "2+1") '(("2" int) ("+" op) ("1" int)) "Binary operation ints")
        (check-equal? (arithmetic-lexer "5.2+3") '(("5.2" float) ("+" op) ("3" int)) "Float and int")
        (check-equal? (arithmetic-lexer "5.2+3.7") '(("5.2" float) ("+" op) ("3.7" float)) "Binary operation floats")
        (check-equal? (arithmetic-lexer "one+two") '(("one" var) ("+" op) ("two" var)) "Binary operation variables")
        (check-equal? (arithmetic-lexer "2 + 1") '(("2" int) ("+" op) ("1" int)) "Binary operation with spaces")
        (check-equal? (arithmetic-lexer "6 = 2 + 1") '(("6" int) ("=" op) ("2" int) ("+" op) ("1" int)) "Multiple operators with spaces")
        (check-equal? (arithmetic-lexer "97 /6 = 2 + 1") '(("97" int) ("/" op) ("6" int) ("=" op) ("2" int) ("+" op) ("1" int)) "Multiple operators")
        (check-equal? (arithmetic-lexer "7.4 ^3 = 2.0 * 1") '(("7.4" float) ("^" op) ("3" int) ("=" op) ("2.0" float) ("*" op) ("1" int)) "Multiple float operators with spaces")
        (check-equal? (arithmetic-lexer "3// this is all") '(("3" int) ("// this is all" comment)) "Variable and comment")
        (check-equal? (arithmetic-lexer "3+5 // this is all") '(("3" int) ("+" op) ("5" int) ("// this is all" comment)) "Expression and comment")

        (check-equal? (arithmetic-lexer "  2 + 1") '(("2" int) ("+" op) ("1" int)) "Spaces before")
        (check-equal? (arithmetic-lexer "  2 + 1 ") '(("2" int) ("+" op) ("1" int)) "Spaces before and after")
    ))

; Start the execution of the test suite
(displayln "Testing: arithmetic-lexer")
(run-tests test-arithmetic-lexer)





