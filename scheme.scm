(lambda (x y)
	(lambda ()
		(set! x y))
	(lambda ()
		(set! y x)))