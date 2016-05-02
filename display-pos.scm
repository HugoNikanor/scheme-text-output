(define (display-at line col message)
  (display (string-append (string #\escape) "[" (string line) ";" (string col) "H" (string message))))
