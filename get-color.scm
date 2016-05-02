; returns color name, depending on the number.
(define (get-color depth)
  (define colors '#(red green blue yellow purple cyan))
  (define no-colors (vector-length colors))
  (vector-ref colors (remainder depth no-colors)))
