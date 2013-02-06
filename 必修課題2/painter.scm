(define perorin (lambda (part)
	(cond 
		((eq? part 'frame) (vertexes->painter
			(list
				(make-vect .05 .1) (make-vect .95 .1)
				(make-vect .95 .1) (make-vect .5 .85)
				(make-vect .5 .85) (make-vect .05 .1))
			#t ))
		((eq? part 'left-eye) (vertexes->painter
			(list
				(make-vect .35 .5) (make-vect .4 .5)
				(make-vect .4 .5) (make-vect .4 .55)
				(make-vect .4 .55) (make-vect .35 .55)
				(make-vect .35 .55) (make-vect .35 .5))
			#t ))
		((eq? part 'right-eye) (vertexes->painter
			(list
				(make-vect .65 .5) (make-vect .6 .5)
				(make-vect .6 .5) (make-vect .6 .55)
				(make-vect .6 .55) (make-vect .65 .55)
				(make-vect .65 .55) (make-vect .65 .5))
			#t ))
		((eq? part 'mouth) (vertexes->painter
			(list
				(make-vect .2 .25) (make-vect .8 .25)
				(make-vect .8 .25) (make-vect .8 .28)
				(make-vect .8 .28) (make-vect .2 .28)
				(make-vect .2 .28) (make-vect .2 .25))
			#t ))
		((eq? part 'tongue) (vertexes->painter
			(list
				(make-vect .6 .25) (make-vect .74 .25)
				(make-vect .74 .25) (make-vect .67 .4)
				(make-vect .67 .4) (make-vect .6 .25))
			#t )) ) ))

(define draw-perorin (lambda (frm)
	(clear-picture)
	(set-color #x0B6A4A)
	((perorin 'frame) frm)
	(set-color #x0ffffff)
	((perorin 'left-eye) frm)
	((perorin 'right-eye) frm)
	((perorin 'mouth) frm)
	(set-color #xCB361F)
	((perorin 'tongue) frm)
))

(define draw-perorin-square-limit (lambda (n frm)
	(clear-picture)
	(set-color #x0B6A4A)
	((square-limit (perorin 'frame) n) frm)
	(set-color #x0ffffff)
	((square-limit (perorin 'left-eye) n) frm)
	((square-limit (perorin 'right-eye) n) frm)
	((square-limit (perorin 'mouth) n) frm)
	(set-color #xCB361F)
	((square-limit (perorin 'tongue) n) frm)
))

