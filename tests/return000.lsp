(define (test n)
  (if (= n 0)
    (begin
      (writeln "zero!")
      (return 42)
      (writeln "i'm fucked..."))
    666))

(define n 999)
(writeln (test 69))
(writeln n)
(writeln (test 0))
(writeln n)
