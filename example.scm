(define fold-left
  (let ((car car)
        (cdr cdr)
        (null? null?))
    (lambda (f z xs)
      (if (null? xs)
          z
		  (fold-left f (f z (car xs)) (cdr xs))))))

(define +
  (let ((fold-left fold-left)
        (+ +))
    (lambda x
      (fold-left + 0 x))))

(define x (+ 2))
((lambda (x) 
	(set! x (+ 2 3))
	x) x)