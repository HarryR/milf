(define (prt lst)
  (while lst
    (writeln (car lst))
    (set! lst (cdr lst))))

(prt '(kiss me alice))
