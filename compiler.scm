(load "parser.scm")

;TDL:
; hw2 completion - handle nested begin.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Eliminate-Nested-Defines;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eliminate-nested-defines
	(lambda (parsed_exp)
		(if (or (null? parsed_exp) (atom? parsed_exp))
			parsed_exp
			(if (should_be_eliminated? parsed_exp)	;are you lambda expression?
				(let ((eliminated_lambda_exp (eliminate-nested-defines-helper parsed_exp)))
				   (cons (car eliminated_lambda_exp)
				   	     (eliminate-nested-defines (cdr eliminated_lambda_exp))))
				(cons (eliminate-nested-defines (car parsed_exp))
					  (eliminate-nested-defines (cdr parsed_exp)))))))

(define eliminate-nested-defines-helper
   (lambda (parsed_lambda_exp)
            (let* ((lambda_body (find_lambda_body parsed_lambda_exp)) ;returns inside list
            	   (defs_exps_list (build_list lambda_body (lambda (ds es) (list ds es))))
            	   (defs_list (car defs_exps_list)))
             	(if (null? defs_list)
             		parsed_lambda_exp
             		(let* ((applic_exp (create_applic defs_exps_list))
						   (lambda_type (car parsed_lambda_exp))
						   (lambda_vars (find_lambda_vars parsed_lambda_exp)))
             			`(,lambda_type ,@lambda_vars ,applic_exp))))))

(define create_applic
	(lambda (defs_exps_list)
		(let* ((defs_list (car defs_exps_list))
			   (exps_list (cadr defs_exps_list))
			   (sets_list (map create_set defs_list)) 
			   (args_list (map cadadr defs_list))		
			   (args_count (length defs_list))			
			   (lambda_simple_body `(seq ,(append sets_list exps_list)))
			   (falses_list (create_falses_list args_count))) 		
			`(applic (lambda-simple ,args_list
				,lambda_simple_body)
				,falses_list))))

(define create_set
	(lambda (def_element)
		`(set ,(cadr def_element) ,(caddr def_element))))

(define create_falses_list
	(lambda (args_count)
		(letrec ((recursive_create_falses_list
					(lambda (num acc)
						(if (equal? num 0)
							acc
							(recursive_create_falses_list (- num 1)
														  (cons `(const #f) acc))))))
			(recursive_create_falses_list  args_count  (list)))))
                
(define build_list
    (lambda (pes ret_des+exprs)
        (if (null? pes)
            (ret_des+exprs '() '())
            (build_list (cdr pes)
		                (lambda (ds es)
		                    (cond ((eq? (caar pes) 'def)
		                            (ret_des+exprs (cons (car pes) ds) es))
		                          ((eq? (caar pes) 'seq) 
		                            (build_list (cadar pes)
		                                 (lambda (ds1 es1)
		                                    (ret_des+exprs (append ds1 ds)
		                                                   (append es1 es)))))
		                          (else 
		                            (ret_des+exprs ds (cons (car pes) es)))))))))

(define should_be_eliminated?
	(lambda (parsed_exp)
		 (or (equal? (car parsed_exp) 'lambda-simple)
      		 (equal? (car parsed_exp) 'lambda-opt)
			 (equal? (car parsed_exp) 'lambda-var))))

(define find_lambda_body
	(lambda (lambda_expr)
		(if (equal? (car lambda_expr) 'lambda-opt)
			(cdddr lambda_expr)
            (cddr lambda_expr))))

(define find_lambda_vars
	(lambda (lambda_expr)
		(if (equal? (car lambda_expr) 'lambda-opt)
			(list (cadr lambda_expr) (caddr lambda_expr))  
			(list (cadr lambda_expr)))))
