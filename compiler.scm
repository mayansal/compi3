(load "parser.scm")

;TDL:
; hw2 completion - handle nested begin. beginSeq


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Eliminate-Nested-Defines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eliminate-nested-defines
	(lambda (parsed_exp)
		(if (or (null? parsed_exp) (atom? parsed_exp))
			parsed_exp
			(if (is_lambda_exp? parsed_exp)	;are you lambda expression?
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

(define is_lambda_exp?
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Remove-Applic-Lambda-Nil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define remove-applic-lambda-nil 
	(lambda (parsed_exp)
		(if (or (null? parsed_exp)(atom? parsed_exp))
			parsed_exp
			(if (is_redundant? parsed_exp)	;are you applic lambda-nil expression?
				(let ((lambda_nil_body (remove-applic-lambda-nil-helper parsed_exp)))
				   (remove-applic-lambda-nil  lambda_nil_body))
				(cons (remove-applic-lambda-nil  (car parsed_exp))
					  (remove-applic-lambda-nil  (cdr parsed_exp)))))))

(define remove-applic-lambda-nil-helper
	(lambda (redundant_parsed_exp)
		(let* ((lambda_nil (cadr redundant_parsed_exp))
			   (lambda_nil_body (caddr lambda_nil)))
			lambda_nil_body)))

(define is_redundant?
	(lambda (parsed_exp)
		(if (and (equal? (car parsed_exp) 'applic)
				 (equal? (caadr parsed_exp) 'lambda-simple)
				 (equal? (cadadr parsed_exp) (list)))
			#t
			#f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Box-Set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define box-set 
	(lambda (parsed_exp)
		(if (or (null? parsed_exp)(atom? parsed_exp))
			parsed_exp
			(if (is_lambda_exp? parsed_exp)	;are you a lambda?
				(let ((boxed_lambda (box-set-helper parsed_exp)))
				   (cons (car boxed_lambda) 
				   	     (box-set (cdr boxed_lambda))))
				(cons (box-set (car parsed_exp))
					  (box-set (cdr parsed_exp)))))))
					  
(define box-set-helper
    (lambda (parsed_lambda_exp)
        (let* ((lambda_body (find_lambda_body parsed_lambda_exp)) ;returns inside list
               (lambda_vars (find_lambda_vars parsed_lambda_exp)) ;returns inside list
               (should_box_vars (filter (should_box_var? lambda_body) lambda_vars)))
            (if (null? should_box_vars)
                parsed_lambda_exp
                (let* ((boxed_body_exp (put_boxes should_box_vars lambda_body))
                        (lambda_type (car parsed_lambda_exp)))
                    `(,lambda_type ,@lambda_vars ,boxed_body_exp))))))
            
(define should_box_var?
    (lambda (lambda_body)
        (lambda (lambda_var)
            (if (and (is_exist_get? lambda_body lambda_var)
                     (is_exist_set? lambda_body lambda_var)
                     (is_exist_bound? lambda_body lambda_var))
                #t
                #f))))
                
(define is_get_exp_for_var?
    (lambda (exp_part suspected_var)
;;         (cond ((null? exp_part) #f)
;;               ((equal? exp_part `(var ,suspected_var)) #t)
;;               ((atom? exp_part) #f)
;;               ((and (equal? (car exp_part) 'set) 
;;                     (equal? (cadr exp_part) suspected_var))
;;                #f)
;;               ((and (is_lambda_exp? exp_part)
;;                     (member suspected_var (car (find_lambda_vars exp_part))))
;;                #f)
;;               ((member suspected_var exp_part) #t)
;;               (else (or (is_get_exp_for_var? (cdr exp_part) suspected_var)
;;                         (is_get_exp_for_var? (car exp_part) suspected_var))))))
 #t))
;; 
;; 
;;                    
(define is_exist_set?
     (lambda (exp_part suspected_var)
        (cond ((null? exp_part) #f)
              ((atom? exp_part) #f)
              ((and (equal? (car exp_part) 'set) 
                    (equal? (cadadr exp_part) suspected_var))
               #t)
              (else (or (is_exist_set? (cdr exp_part) suspected_var)
                        (is_exist_set? (car exp_part) suspected_var))))))
                    
 (define is_exist_bound?
     (lambda (exp_part suspected_var)
        #t))

(define put_boxes
    (lambda (should_box_vars lambda_body)
        (if (null? should_box_vars)
            lambda_body
            (put_boxes (cdr should_box_vars)
                       (put_var_boxes lambda_body (car should_box_vars))))))

(define put_var_boxes
    (lambda (lambda_body should_box_var)       
        (let* ((with_set_boxes (put_set_boxes should_box_var lambda_body))
               (with_get_and_set_boxes (put_get_boxes should_box_var with_set_boxes)))
            with_get_and_set_boxes)))
            
(define put_set_boxes
    (lambda (var body)
        ;return body with sets for this var
        body))

(define put_get_boxes
    (lambda (var body)
        ;return body with gets for this var
        body))




        
        
        
        
        
;;;;;;probably garbage;;;;;
;; (define is_exist_get?
;;     (lambda (lambda_body suspected_var)
;;         (if (null? lambda_body)
;;             #f
;;             (if (is_get_exp_for_var? lambda_body suspected_var)
;;                 #t
;;                 (let* ((first_exp (car lambda_body))
;;                        (rest_exp (cdr lambda_body)))
;;                     (or (is_exist_get? first_exp suspected_var)
;;                         (is_exist_get? rest_exp suspected_var)))))))











