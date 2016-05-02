(define (get-background-number) 4)

(define (get-color-number color)
  (define (get-name color)
    (car color))

  (define (get-value color)
    (cdr color))

  (define colors 
    (list
      (cons 'none   "0")
      (cons 'red    "1")
      (cons 'green  "2")
      (cons 'yellow "3")
      (cons 'blue   "4")
      (cons 'purple "5")
      (cons 'cyan   "6")
      (cons 'white  "7")
      (cons 'black  "8")))

  (define (inner remaining)
    (let ((current (car remaining)))
      (if (eqv? (get-name current) color)
        (get-value current)
        (inner (cdr remaining)))))
  (inner colors))

(define (make-color-escape foreground background)
  (string-append
    (string #\escape)
    "[0;3"
    (get-color-number foreground)
    ";4"
    (get-color-number background)
    "m"))

(define escape-color-reset
  (string-append (string #\escape) "[m"))

(define (reset-color)
  (display escape-color-reset))

(define (background color . text)
  (define (surround str)
    (string-append
    (make-color-escape 'none color)
    (string str) escape-color-reset))
  (accumulate string-append "" (map surround text)))
