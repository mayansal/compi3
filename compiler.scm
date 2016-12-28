;; works
(define eliminate-nested-defines
   (lambda (parsed_exp)
         (if (not (should_be_eliminated? parsed_exp)) ;what is the condition exactly?
             parsed_exp
            (let* ((def_lambda (find_def_value parsed_exp))
            	   (parsed_exp_beginning (take_beginning parsed_exp))
            	   (lambda_body (find_lambda_body def_lambda))
            	   (defs_exps_list (build_list lambda_body (lambda (ds es) (list ds es))))
            	   (defs_list (car defs_exps_list)))
             	(if (null? defs_list)
             		parsed_exp
             		(let* ((applic_exp (create_applic defs_exps_list))
					 	   (def (car parsed_exp))
			  			   (vars (cadr parsed_exp))
						   (lambda_all (cddr parsed_exp))
						   (lambda_type (caar lambda_all))
						   (lambda_vars (cadar lambda_all)))
             			`(,def ,vars (,lambda_type ,lambda_vars ,applic_exp))))))))

; ;; works
; (define take_beginning
; 	(lambda (parsed_exp)
; 		(let* ((def (car parsed_exp))
; 			   (vars (cadr parsed_exp))
; 			   (lambda_all (cddr parsed_exp))
; 			   (lambda_type (caar lambda_all))
; 			   (lambda_vars (cadar lambda_all)))
; 			`(,def ,vars (,lambda_type ,lambda_vars ,applic_exp)))))


;; works
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

;; works
(define create_set
	(lambda (def_element)
		`(set ,(cadr def_element) ,(caddr def_element))))

;; works
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
		(if (not (equal? (car parsed_exp) 'def))
			#f
			(let ((def_body (find_def_value parsed_exp)))
				 (or (equal? (car def_body) 'lambda-simple)
				 	(equal? (car def_body) 'lambda-opt)
				 	(equal? (car def_body) 'lambda-var))))))


(define find_def_value
	(lambda (def_parsed_exp)
		(caddr def_parsed_exp)))

(define find_lambda_body
	(lambda (lambda_expr)
		(cddr lambda_expr)))
