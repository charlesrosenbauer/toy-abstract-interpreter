;;(use-package :trivia)


;; exp  = int
;;	| exp + exp
;;	| exp * exp
;;  |     - exp
;;	| exp = exp
(defstruct numexp (num 0))
(defstruct addexp (a 0) (b 0))
(defstruct mulexp (a 0) (b 0))
(defstruct negexp (a 0))
(defstruct eqlexp (a 0) (b 0))



(defun simple-eval (exp)
	(cond
		((typep exp `integer) exp)
		((typep exp `addexp) (+
								(simple-eval (addexp-a exp))
								(simple-eval (addexp-b exp))))
		((typep exp `mulexp) (* 
								(simple-eval (mulexp-a exp))
								(simple-eval (mulexp-b exp))))
		((typep exp `negexp) (- 0
								(simple-eval (negexp-a exp))))
		((typep exp `eqlexp) (if
								(eql
									(simple-eval (eqlexp-a exp)) 
									(simple-eval (eqlexp-b exp)))
								1 0))
		(t exp)))

(defun member? (x s) (intersection (list x) s))

(defun abstract   (exp)
	(cond
		((typep exp `integer)
			(cond
				((<   exp 0) `(:N))
				((eql exp 0) `(:Z))
				((>   exp 0) `(:P))))
		
		((typep exp `addexp)
			(let ((a (abstract (addexp-a exp)))
				  (b (abstract (addexp-b exp))))

			(reduce `union	(list
				(if (and (member? :P a) (member? :P b)) `(:P        ) NIL)
				(if (and (member? :P a) (member? :Z b)) `(:P        ) NIL)
				(if (and (member? :Z a) (member? :P b)) `(:P        ) NIL)
				(if (and (member? :P a) (member? :N b)) `(:P  :Z  :N) NIL)
				(if (and (member? :Z a) (member? :Z b)) `(    :Z    ) NIL)
				(if (and (member? :N a) (member? :P b)) `(:P  :Z  :N) NIL)
				(if (and (member? :N a) (member? :Z b)) `(        :N) NIL)
				(if (and (member? :Z a) (member? :N b)) `(        :N) NIL)
				(if (and (member? :N a) (member? :N b)) `(        :N) NIL)))))

		((typep exp `mulexp)
			(let ((a (abstract (mulexp-a exp)))
				  (b (abstract (mulexp-b exp))))

			(reduce `union	(list
				(if (and (member? :P a) (member? :P b)) `(:P        ) NIL)
				(if (and (member? :P a) (member? :Z b)) `(    :Z    ) NIL)
				(if (and (member? :Z a) (member? :P b)) `(    :Z    ) NIL)
				(if (and (member? :P a) (member? :N b)) `(        :N) NIL)
				(if (and (member? :Z a) (member? :Z b)) `(    :Z    ) NIL)
				(if (and (member? :N a) (member? :P b)) `(        :N) NIL)
				(if (and (member? :N a) (member? :Z b)) `(    :Z    ) NIL)
				(if (and (member? :Z a) (member? :N b)) `(    :Z    ) NIL)
				(if (and (member? :N a) (member? :N b)) `(:P        ) NIL)))))

		((typep exp `negexp)
			(let ((a (abstract (negexp-a exp))))

			(reduce `union	(list
				(if (member? :N a) `(:P        ) NIL)
				(if (member? :Z a) `(    :Z    ) NIL)
				(if (member? :P a) `(        :N) NIL)))))

		((typep exp `eqlexp) `(:Z :P))

		))




