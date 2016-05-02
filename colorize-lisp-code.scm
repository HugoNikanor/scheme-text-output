; returns color name, depending on the number.
(define (get-color depth)
  (define colors '#(red green blue yellow purple cyan))
  (define no-colors (vector-length colors))
  (vector-ref colors (remainder depth no-colors)))

; generates color data for a lisp list, where every set of parenthesis has their own color
; really hard to print out, see 'deep-print'
(define (make-color-expr-list exp)
  (define (inner depth exp color-escape-return)
    (let ((current-color-escape (make-color-escape 'none (get-color depth))))
      (list current-color-escape
            (if (list? exp)
              (map (lambda (e) 
                     (if (list? e)
                       (inner (+ depth 1) e current-color-escape)
                       e))
                   exp)
              exp)
            color-escape-return))) 
  (inner 0 exp escape-color-reset))


; uses some weird tricks to print the lists produced by 'make-color-expr-list'
(define (deep-print expr)
  (define (break-out-escapes full-expr)
    (define (inner done remaining)
      (if (null? remaining)
        done
        (let ((next (car remaining)))
          (if (list? next)
            (inner (cons (caddr next) (cons (cadr next) (cons (car next) done))) (cdr remaining))
            (inner (cons next done) (cdr remaining))))))
    (let ((ret-val (reverse (inner '() full-expr))))
      (map (lambda (l) (if (has-inner-list? l)
                         (break-out-escapes l)
                         l))
           ret-val)))
  (let ((start-esc (car expr))
        (exp (cadr expr))
        (end-esc (caddr expr)))
    (display start-esc)
    (if (has-inner-list? exp)
      (display (break-out-escapes exp))
      (display exp))
    (display end-esc)))
