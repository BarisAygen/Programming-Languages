(define get-operator (lambda (op)
   (cond
     ( (eq? op '-) -) 
     ( (eq? op '*) *) 
     ( (eq? op '/) /) 
     ( (eq? op '+) +) 
     ( else ((display "cs305: ERROR \n\n") (repl env)))
  )))

(define get-value (lambda (var env1 env2)
    (cond
      ((null? env2) (display "cs305: ERROR \n\n") (repl env1))

      ((equal? (caar env2) var) (cdar env2))

      (else (get-value var env1 (cdr env2))))))

(define extend-new-env (lambda (var val env1)
       (cons (cons var val) env1)))

(define define-stmt? (lambda (e)
    (and (list? e) (eq? (length e) 3) (eq? (car e) 'define) (symbol? (cadr e)) (expr? (caddr e)))))

(define define-symbol? (lambda (e env)
  (and (not (null? env)) (if (eq? (caar env) e) 
      #t 
      (if (> (length env) 1) 
        (define-symbol? e (cdr env))
        #f
      )))
))
    
(define expr? (lambda (e)
 (or 
  (symbol? e)
  (number? e)
  (and (list? e) (or (symbol? (car e)) (define-operand? e) (define-if? e) (define-let? e) (lambda? e) (lambda? (car e))))
)))

(define define-if?
  (lambda (e)
    (and (list? e)                      
         (= (length e) 4)               
         (eq? (car e) 'if))))            

(define define-operand? (lambda (e)
  (and 
		(list? e)
		(or (eq? '+ (car e)) (eq? '- (car e)) (eq? '* (car e)) (eq? '/ (car e)))
		(> (length e) 2)
	) 
))

(define apply-operation (lambda (e env)
  (let (
    (operands (map s6-interpret (cdr e) (make-list (length (cdr e)) env)))
    (operator (get-operator (car e))))
        (apply operator operands)
  ))
)

(define valid-list?
  (lambda (e)
    (if (null? e)
        #t
        (and (pair? (car e))          
             (= 2 (length (car e)))   
             (symbol? (caar e))       
             (valid-list? (cdr e))))))

(define define-let? (lambda (e)
    (and
      (eq? (length e) 3)
      (eq? (car e) 'let)
      (or (eq? (cadr e) '() ) (valid-list? (cadr e)))
    )))

(define apply-let (lambda (e env)
  (let(
    (vars (map car (cadr e)))
    (vals (map cadr (cadr e)))
    )
    (let 
      ((vals_interpreted (map (lambda (val) (s6-interpret val env)) vals)))
      (let 
        ((env2 (append (map cons vars vals_interpreted) env)))
        (s6-interpret (caddr e) env2)
      )
    )
  )))

(define list-binding? (lambda (e)
	(and 
		(symbol? (car e))
    (list? e)
		(or (null? (cdr e)) (list-binding? (cdr e)))
	)
))

(define lambda? (lambda (e)
  (and 
    (list? e) 
    (and (eq? 'lambda (car e)) (list-binding? (cadr e)) (expr? (caddr e)) (not (define-stmt? (caddr e)))) 
  )))

(define apply-lambda
  (lambda (e env)
    (let ((param-count (length (cadar e)))
          (arg-count (length (cdr e))))
      (if (= param-count arg-count)
          (let* ((args (map (lambda (arg) (s6-interpret arg env))
                            (cdr e)))
                 (new-env (append (map cons (cadar e) args) env)))
            (s6-interpret (caddar e) new-env))
          (begin
            (display "cs305: ERROR \n\n")
            (repl env))))))

(define repl (lambda (env)
  (let* (
      (dummy1 (display "cs305> "))
      (expr (read))  
      (env2 (if (define-stmt? expr) 
        (extend-new-env (cadr expr) (s6-interpret (caddr expr) env) env) env)) 
      (val (if (define-stmt? expr)
        (cadr expr)
        (s6-interpret expr env)))
      (dummy2 (display "cs305: "))
      (dummy3 (display val))
      (dummy4 (newline))
      (dummy4 (newline)))
    (repl env2))))

(define s6-interpret (lambda (e env)
  (if (expr? e)
    (cond
      ((number? e) e)
      ((and (symbol? e) (define-symbol? e env)) (get-value e env env))
      ((and (symbol? e) (or (define-symbol? e env) (memv e '(- + / *))))display "[PROCEDURE]")
      ((not (list? e)) (display "cs305: ERROR \n\n") (repl env))
      ((null? e) e)
      ((define-if? e) (let ((val 
                  (if (not (eq? (s6-interpret (cadr e) env) 0))
                    (s6-interpret (caddr e) env)
                    (s6-interpret (cadddr e) env)))) val))
      ((define-let? e) (apply-let e env))
      ((lambda? e) e)
      ((lambda? (car e)) (apply-lambda e env))
      ((define-operand? e) (apply-operation e env))
      (else (s6-interpret (append (list (get-value (car e) env env)) (cdr e))env ))
      )
    ((display "cs305: ERROR \n\n") (repl env))
    )))

(define cs305 (lambda () (repl '())))