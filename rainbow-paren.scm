(define (rainbow-paren str)
  (define str-list (string->list str))
  (define (inner remaining depth escaped in-string)
    (if (not (null? remaining))
      (let ((ch (car remaining))
            (default-print (lambda (ch)
                             (display ch)
                             (inner (cdr remaining)
                                    depth
                                    (if (eqv? ch #\\) #t #f)
                                    (if (eqv? ch #\") (not in-string) in-string))))
            (fancy-print (lambda (ch depth-mod get-next)
                           (display (color-char (get-color (depth-mod depth)) 'none ch))
                           (inner (cdr remaining) (get-next depth) #f in-string))))
        (cond
          (escaped
            (display ch)
            (inner (cdr remaining) depth #f in-string))
          ((or (eqv? ch #\() (eqv? ch #\)))
           (if in-string
             (default-print ch)
             (if (eqv? ch #\))
               (fancy-print ch dec dec)
               (fancy-print ch (lambda (x) x) inc))))
          (else
            (default-print ch))))))
  (inner str-list 0 #f #f))
