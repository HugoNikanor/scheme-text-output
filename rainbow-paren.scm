(define (rainbow-paren str)
  (define str-list (string->list str))
  (define (inner remaining depth escaped in-string)
    (if (not (null? remaining))
      (let ((ch (car remaining)))
        (if escaped
          (begin
            (display ch)
            (inner (cdr remaining) depth #f in-string))
          (cond
            ((eqv? ch #\\)
             (display ch)
             (inner (cdr remaining) depth #t in-string))
            ((eqv? ch #\")
             (display ch)
             (if in-string
               (inner (cdr remaining) depth #f #f)
               (inner (cdr remaining) depth #f #t)))
            ((eqv? ch #\()
             (if in-string
               (begin
                 (display ch)
                 (inner (cdr remaining) depth #f in-string))
               (begin
                 (display (color-char (get-color depth) 'none ch))
                 (inner (cdr remaining) (inc depth) #f in-string))))
            ((eqv? ch #\))
             (if in-string
               (begin
                 (display ch)
                 (inner (cdr remaining) depth #f in-string))
               (begin
                 (display (color-char (get-color (dec depth)) 'none ch))
                 (inner (cdr remaining) (dec depth) #f in-string))))
            (else
              (display ch)
              (inner (cdr remaining) depth #f in-string)))))))
  (inner str-list 0 #f #f))
