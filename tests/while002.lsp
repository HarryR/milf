(define cnt 5)
(while (> cnt 0)
  (writeln cnt)
  (if (= cnt 3) (begin (set! cnt 1) (continue 42)))
  (set! cnt (- cnt 1)))
