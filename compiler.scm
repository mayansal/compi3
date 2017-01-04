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
		(if (null? parsed_exp)
			#f
			 (or (equal? (car parsed_exp) 'lambda-simple)
	      		 (equal? (car parsed_exp) 'lambda-opt)
				 (equal? (car parsed_exp) 'lambda-var)))))

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
               (should_box_vars (filter (should_box_var? lambda_body) (car lambda_vars))))
            (if (null? should_box_vars)
                parsed_lambda_exp
                (let* ((boxed_body_exp (put_boxes should_box_vars lambda_body))
                	   (body_sets_and_boxes (create_set_box_body should_box_vars boxed_body_exp))
                       (lambda_type (car parsed_lambda_exp)))
                    `(,lambda_type ,@lambda_vars  ,body_sets_and_boxes))))))
            
(define should_box_var?
    (lambda (lambda_body)
        (lambda (lambda_var)
            (if (and (is_exist_get? lambda_body lambda_var)
                     (is_exist_set? lambda_body lambda_var)
                     (is_exist_bound? lambda_body lambda_var))
                #t
                #f))))
                
(define is_exist_get?
    (lambda (exp_part suspected_var)
         (cond ((null? exp_part) #f)
               ((equal? exp_part `(var ,suspected_var)) #t)
               ((atom? exp_part) #f)
               ((and (equal? (car exp_part) 'set) 
                     (equal? (cadadr exp_part) suspected_var))
                  #f)
               ((and (is_lambda_exp? exp_part)
                     (member suspected_var (car (find_lambda_vars exp_part))))
                  #f)
               (else (or (is_exist_get? (cdr exp_part) suspected_var)
                         (is_exist_get? (car exp_part) suspected_var))))))

                    
(define is_exist_set?
     (lambda (exp_part suspected_var)
        (cond ((null? exp_part) #f)
              ((atom? exp_part) #f)              
              ((and (equal? (car exp_part) 'set) 
                    (equal? (cadadr exp_part) suspected_var))
              	 #t)
			  ((and (is_lambda_exp? exp_part)
                    (member suspected_var (car (find_lambda_vars exp_part))))
			     #f)
              (else (or (is_exist_set? (cdr exp_part) suspected_var)
                        (is_exist_set? (car exp_part) suspected_var))))))
                 
(define is_exist_bound?
     (lambda (exp_part suspected_var)
        (if (or (null? exp_part) (atom? exp_part))
        	#f
        	(if (is_lambda_exp? exp_part)
        		(is_exist_bound_helper? exp_part suspected_var)
        		(or (is_exist_bound? (cdr exp_part) suspected_var)
                	(is_exist_bound? (car exp_part) suspected_var))))))

(define is_exist_bound_helper?
    (lambda (exp_part suspected_var)
         (cond ((null? exp_part) #f)
               ((equal? exp_part `(var ,suspected_var)) #t)
               ((atom? exp_part) #f)
               ((and (equal? (car exp_part) 'set) 
                     (equal? (cadadr exp_part) suspected_var))
                  #t)
               ((and (is_lambda_exp? exp_part)
                     (member suspected_var (car (find_lambda_vars exp_part))))
                  #f)
               (else (or (is_exist_bound_helper? (cdr exp_part) suspected_var)
                         (is_exist_bound_helper? (car exp_part) suspected_var))))))

(define put_boxes
    (lambda (should_box_vars lambda_body)
        (if (null? should_box_vars)
            lambda_body
            (put_boxes (cdr should_box_vars)
                       (put_var_boxes lambda_body 
                       				  (car should_box_vars))))))

(define put_var_boxes
    (lambda (lambda_body should_box_var)       
        (let* ((with_set_boxes (put_set_boxes lambda_body should_box_var))
               (with_get_and_set_boxes (put_get_boxes with_set_boxes should_box_var)))
            with_get_and_set_boxes)))
            
        
;returns body with sets for this var
(define put_set_boxes
     (lambda (exp_part suspected_var)
        (cond ((or (null? exp_part) (atom? exp_part))
         		 exp_part)              
              ((and (equal? (car exp_part) 'set) 
                    (equal? (cadadr exp_part) suspected_var))
              	 (cons 'box-set
              		   (cdr exp_part))) 
			  ((and (is_lambda_exp? exp_part)
                    (member suspected_var (car (find_lambda_vars exp_part))))
			     exp_part)
              (else (cons (put_set_boxes (car exp_part) suspected_var)
                          (put_set_boxes (cdr exp_part) suspected_var))))))


(define put_get_boxes
    (lambda (exp_part suspected_var)
         (cond ((null? exp_part) exp_part)
               ((equal? exp_part `(var ,suspected_var)) 
               	   (cons 'box-get
              		   (list exp_part)))
               ((atom? exp_part) exp_part)
               ((and (equal? (car exp_part) 'box-set) 
                     (equal? (cadadr exp_part) suspected_var))
                  exp_part)
               ((and (is_lambda_exp? exp_part)
                     (member suspected_var (car (find_lambda_vars exp_part))))
                  exp_part)
               (else (cons (put_get_boxes (car exp_part) suspected_var)
                           (put_get_boxes (cdr exp_part) suspected_var))))))


(define create_set_box_body 
	(lambda (should_box_vars boxed_body_exp)
		`(seq ,(append (map make_set should_box_vars)
				boxed_body_exp))))
;; could be another seq inside 

(define make_set
	(lambda (var_to_box)
		`(set (var ,var_to_box) (box (var ,var_to_box))) ))


(define flat_seq
	(lambda (exp)
		(if (or (null? exp) (atom? exp))
			exp
			(if (equal? (car exp) 'seq)
				(flat_seq (cadr exp))
				(cons (flat_seq (car exp))
					  (flat_seq (cdr exp))))
			)))

			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Pe->Lex-Pe ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pe->lex-pe
	(lambda (parsed_exp)
        (let ((with_pvar_and_bvar (pe->lex-pe-core parsed_exp)))
            (add_fvars with_pvar_and_bvar))))		

(define pe->lex-pe-core
	(lambda (parsed_exp)
		(if (or (null? parsed_exp)(atom? parsed_exp))
			parsed_exp
			(if (is_lambda_exp? parsed_exp)	;are you a lambda?
				(let ((lex_lambda (pe->lex-pe-helper parsed_exp)))
				   (cons (car lex_lambda) 
				   	     (pe->lex-pe-core (cdr lex_lambda))))
				(cons (pe->lex-pe-core (car parsed_exp))
					  (pe->lex-pe-core (cdr parsed_exp)))))))					  
					  
(define pe->lex-pe-helper
    (lambda (parsed_lambda_exp)
        (let* ((lambda_body (find_lambda_body parsed_lambda_exp)) ;returns inside list
               (lambda_vars (find_lambda_vars parsed_lambda_exp))) ;returns inside list
            (if (null? lambda_vars)
                parsed_lambda_exp
                (let* ((lex_body (update_vars lambda_vars lambda_body))
                       (lambda_type (car parsed_lambda_exp)))
                    `(,lambda_type ,@lambda_vars  ,lex_body))))))
                    
(define update_vars
    (lambda (lambda_vars lambda_body)
        (if (null? lambda_vars)
            lambda_body
            (update_vars (cdr lambda_vars)
                         (put_var_params_bounds lambda_body 
                                                (car lambda_vars))))))
                                                
(define put_var_params_bounds
    (lambda (lambda_body var)       
        (let* ((with_pvar (put_pvar lambda_body var))
               (with_pvar_and_bvar (put_bvar with_pvar var)))
            with_pvar_and_bvar)))

;find all apperances of var as the current lambda parameter
(define put_pvar
    (lambda (exp_part var)
        (cond ((null? exp_part) exp_part)
              ((equal? exp_part `(var ,var))  
                  `(pvar ,var))
              ((atom? exp_part) exp_part)
              ((and (equal? (car exp_part) 'box-set)           
                    (equal? (cadadr exp_part) var))  
                `(set (pvar ,var) ,(caddr exp_part)))
              ((equal? (car exp_part) 'box)           
                    exp_part)
              ((is_lambda_exp? exp_part)
                exp_part)
              (else (cons (put_pvar (car exp_part) var)
                          (put_pvar (cdr exp_part) var))))))
                          
(define put_bvar
    (lambda (exp_part var)
        exp_part))
        
        
(define add_fvars
    (lambda (exp_pbvars)
        (cond ((null? exp_pbvars) exp_pbvars)
              ((equal? (car exp_pbvars) 'var)
                   `(fvar ,(cdr exp_pbvars)))
              ((atom? exp_pbvars) exp_pbvars)
;;            ((and (equal? (car exp_part) 'box-set)           
;;                  (equal? (cadadr exp_part) var))  
;;                `(set (pvar ,var) ,(caddr exp_part)))
;;            ((equal? (car exp_part) 'box)           
;;                         exp_part)
              (else (cons (add_fvars (car exp_pbvars))
                          (add_fvars (cdr exp_pbvars)))))))
        
    
					  
					  
					  
					  
					  
					  
					  
					  
