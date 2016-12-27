(define eliminate_nested_defines
   (lambda (parsed_exp)
         (if (not (should_be_eliminated? parsed_exp)) ;what is the condition exactly?
             parsed_exp
            (let* ((def_lambda (find_def_value parsed_exp))
            	   (lambda_body (find_lambda_body def_lambda))
            	   (defs_exps_list (build_list lambda_body (lambda (ds es) (list ds es)))))
                   ;(applic_eliminiated (build_applic defs_exps_list)))
                defs_exps_list))))
                ;`(,parsed_exp_begining ,applic_eliminiated)))))
                

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
