(define dir 42)
(for-each-dir "pth"
  (lambda (dir)
    (writeln dir)))
(writeln "!!!")
(writeln dir)
