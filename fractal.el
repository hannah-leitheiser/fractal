; mandlebrot.el
;  emacs LISP
;  by Hannah Leithieiser
;  prints ASCII text of the Mandlebrot
;  	set

(defun mandlebrot(x y) 
	"Pots the Mandelbrot set.  
	0 if inside, otherwise 1-7 if outside,
	higher numbers for more iterations before escape."

	; creating an internal recursive function
	(defun mand(x y i zr zi)
	(let ((zr2 zr)(zi2 zi)) 
	(if (> i 30) 0
		; if out of bounds,
		(if (> (+ (* zr2 zr2) 
			  (* zi2 zi2)) 4)
			  ; return 1-7
			  (floor (* (+ i 1) 0.25))
			; otherwise call anothre recursion
			(mand 
				x 
				y 
				(+ i 1) 
				(+ (- (* zr2 zr2) (* zi2 zi2)) x) 
				(+ (* 2 zr2 zi2) y)
				)
			)
		)))
	; call the function initially
	(mand x y 0 0 0)
	)

; 8 characters in order of darkness, by my guess
(setq chars '(" " "." ":" "-" "*" "%" "#" "¶") ) 

; 400 lines is pretty extreme, true
(dotimes (y 200) 
	(dotimes (x 400)  
		(princ (nth (mandlebrot 
			(* (- x 300) 0.0060) 
			(* (- y 100) 0.012)) 
			chars))) 
	(princ "\n"))
