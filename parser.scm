(load "pc.scm")
(load "pattern-matcher.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HW2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;CORE&MACRO-EXPANSION&ASSETS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define simple-const? 
	(lambda (exp)
		(or (number? exp)
			(boolean? exp)
			(vector? exp)
			(char? exp)
			(string? exp) )))

(define not-reserved?
	(lambda (x)
		(not(member x '(and begin cond define do else if lambda 
					  	     let let* letrec or quasiquote unquote
					 	     unquote-splicing quote set!)))))
(define cons_false 
	(lambda (var)
		(cons var (list #f))))

(define cons_set
	(lambda (pair)
	 	(cons 'set! pair)))

(define till_point
	(lambda (lst new_lst)
		(if (pair? lst)
			(till_point (cdr lst)
						(append new_lst (list (car lst))))
			new_lst)))

(define till_point_help
	(lambda (lst)
		(till_point lst (list))))

(define no-duplications?
	(lambda (lst)
		(cond
			;improper list: 
			((and (not (list? lst)) (pair? lst)) (no-duplications? (till_point_help lst)))
			;not a list:
			((not (pair? lst)) #t)
			;else, list:
			((null? lst) #t) 
      		((member (car lst) (cdr lst)) #f)
      		(else (no-duplications? (cdr lst))))))

(define var?
	(lambda (x)
		(and (symbol? x)
			 (not-reserved? x))))

(define is-car-var?
	(lambda (x)
		(if (not (pair? x))
			#f
			(var? (car x)))))

(define identify-lambda
	(lambda (argl ret-simple ret-opt ret-var)
		(cond 
			((null? argl) (ret-simple '()))
			((var? argl) (ret-var argl))      
			(else (identify-lambda (cdr argl)
					(lambda (s) (ret-simple `(,(car argl) ,@s))) ;simple
					(lambda (s opt) (ret-opt `(,(car argl) ,@s) opt)) ;opt
					(lambda (var) (ret-opt `(,(car argl)) var)))))))
                                
(define parse
	(lambda (exp)
		(let ((answer (parse-2 exp)))
			(if (is_contain_fail? answer)
				'"ERROR"
				answer))))

(define is_contain_fail?
	(lambda (lst)
		(if (null? lst)
			#f
			(if (equal? lst 'fail!)
				#t
				(if (atom? lst)
					#f
					(or (is_contain_fail? (car lst))
						(is_contain_fail? (cdr lst))))))))

(define parse-2
	(let ((run (compose-patterns

					;Constants;
					(pattern-rule
						`(quote ,(? 'c))
						(lambda (c) `(const ,c)))
					(pattern-rule
						(? 'c simple-const?)
						(lambda (c) `(const ,c)))

					;Conditionals;
					(pattern-rule
						`(if ,(? 'test) ,(? 'dit))
						(lambda (test dit) 
							`(if3 ,(parse-2 test) ,(parse-2 dit) (const ,(void)))))
					(pattern-rule 
						`(if ,(? 'test) ,(? 'dit) ,(? 'dif))
						(lambda (test dit dif)
							`(if3 ,(parse-2 test) ,(parse-2 dit) ,(parse-2 dif))))
					
					;Disjunctions;
					(pattern-rule
						`(or)
						(lambda () (parse-2 #f)))
					(pattern-rule 
						`(or ,(? 'expr).,(? 'exprs))
						(lambda (expr exprs)
							(if (null? exprs)
								(parse-2 expr)
								`(or ,(map parse-2
									 	   (cons expr exprs))))))
                                        
                    ;Lambda forms;       
					(pattern-rule
						`(lambda ,(? 'argl no-duplications?) ,(? 'expr).,(? 'exprs))
						(lambda (argl expr exprs)
							(identify-lambda    argl 
                                                (lambda (s) `(lambda-simple ,s ,(parse-2 `(begin ,@(cons expr exprs)))))
                                                (lambda (s opt) `(lambda-opt ,s ,opt ,(parse-2 `(begin ,@(cons expr exprs)))))
                                                (lambda (var) `(lambda-var ,var ,(parse-2 `(begin ,@(cons expr exprs))))))))
                
		    		;Define;
					(pattern-rule	
						`(define ,(? 'var var?) ,(? 'expr) .,(? 'exprs))
						(lambda (var expr exprs)
							`(def (var ,var) ,(parse-2 `(begin ,@(cons expr exprs))))))
					
					 ;Define MIT;
					 (pattern-rule
					 	`(define ,(? 'var_args is-car-var?) ,(? 'expr) . ,(? 'exprs))
					 	(lambda (var_args expr exprs)
					 		`(def (var ,(car var_args)) ,(parse-2 `(lambda ,(cdr var_args) ,expr ,@exprs)))))


 					;Assignments;
				    (pattern-rule
					 	`(set! ,(? 'v var?) ,(? 'pexpr))
					 	(lambda (v pexpr)
							`(set (var ,v) ,(parse-2 pexpr)))) 

				    ;Applications;
				    (pattern-rule
					 	`( ,(? 'expr not-reserved?) .,(? 'exprs))
					 	(lambda (expr exprs)
							`(applic ,(parse-2 expr) ,(map parse-2 exprs))))
		    		
		    		;Sequences;
					(pattern-rule
						`(begin)
						 (lambda () `(const ,(void))))

					(pattern-rule
  						`(begin ,(? 'expr).,(? 'exprs))
  						(lambda (expr exprs)
                              (if (null? exprs)
                              	  (parse-2 expr)
 							   	  `(seq ,(map parse-2 (cons expr exprs))))))
                     
					;Variables;
					(pattern-rule
					 	(? 'v var?)				
					 	(lambda (v) `(var ,v)))

					
					;;;;;;; Macro - Expansions ;;;;;;;;
					;And; 
					(pattern-rule
						`(and)
						(lambda () (parse-2 #t)))
					
					(pattern-rule
					 	`(and ,(? 'expr) .,(? 'exprs)) 				
					 	(lambda (expr exprs)
						    (if (null? exprs)
                                (parse-2 expr)
							    (parse-2 `(if ,expr (and ,@exprs) #f)))))

					;Cond;
					(pattern-rule
					 	`(cond ,(? 'expr pair?) .,(? 'exprs)) 				
					 	(lambda (expr exprs)
						    (if (null? exprs)
                                (if (equal? (car expr) 'else)
                                	(parse-2 `(begin ,@(cdr expr)))
                                	(parse-2 `(if ,(car expr) (begin ,@(cdr expr)))))
							 	(parse-2 `(if ,(car expr) (begin ,@(cdr expr)) (cond ,@exprs))))))

					;Let;
					(pattern-rule
						`(let ,(? 'pairs list?) ,(? 'body) .,(? 'bodies))
						(lambda (pairs body bodies)
							(parse-2 `((lambda ,(map car pairs) ,@(cons body bodies))
									 ,@(map cadr pairs)))))

					;Let*;
					(pattern-rule
						`(let* ,(? 'pairs list?) ,(? 'body) .,(? 'bodies))
						(lambda (pairs body bodies)
							(cond ((null? pairs)
									(parse-2 `(let () ,@(cons body bodies))))
								  ((null? (cdr pairs))
								  	(parse-2 `(let (,(car pairs)) ,@(cons body bodies))))
								  (else 
								    (parse-2 `(let (,(car pairs)) (let* ,(cdr pairs) ,@(cons body bodies))))))))
					;Letrec;
					(pattern-rule
						`(letrec ,(? 'pairs list?) ,(? 'body) .,(? 'bodies))
						(lambda (pairs body bodies)
							(parse-2 `(let ,(map cons_false (map car pairs))
										 ,@(map cons_set pairs)
										 ((lambda () ,@(cons body bodies)))))))

					;QQ;
					 (pattern-rule
					 	`(quasiquote .,(? 'expr))
					 	(lambda (expr)
					 		(parse-2 (expand-qq (car expr)))))


					)))
				
		(lambda (sexpr)
			(run sexpr (lambda () 'fail!)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;QQ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; qq.scm
;;; A naive, one-level quasiquote implementation + optimizations
;;;
;;; Programmer: Mayer Goldberg, 2016

(load "pattern-matcher.scm")

;;;

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
	 (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
		 simple-sexprs-predicates)
	  (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (symbol? e)
	    (vector? e))
	`',e
	e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
	(cadr e)
	e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
	 (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const? e1) (const? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HW1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;whitespaces&comments;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
	 (new (*parser (char #\newline))
	      (*parser <end-of-input>)
	      (*disj 2)
	      done)))
    (new (*parser (char #\;))
	 
	 (*parser <any-char>)
	 (*parser <end-of-line-comment>)
	 *diff *star

	 (*parser <end-of-line-comment>)
	 (*caten 3)
	 done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <Sexpr>))
       (*caten 2)
       done))
       
       
(define <Infix-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <InfixExpression>))
       (*caten 2)
       done))

       
(define <comment>
  (disj <line-comment>
        <Infix-comment>
	<sexpr-comment>))

(define <skip>
  (disj <comment>
	<whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
	   (*parser <p>)
	   (*parser <wrapper>)
	   (*caten 3)
	   (*pack-with
	    (lambda (_left e _right) e))
	   done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;simple parsers;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;boolean;;;;;;;;;;;;;;;;;;

(define <Boolean>
(^<skipped*>
  (new (*parser (word-ci "#t"))
       (*pack (lambda (a) '#t))
       (*parser (word-ci "#f"))
       (*pack (lambda (a) '#f))
       (*disj 2)

    done)))

;;;;;;;;;;;;;;;;;;;;char;;;;;;;;;;;;;;;;;;;;;

(define <CharPrefix>
    (new 
        (*parser (word "#\\"))
    done)) 

(define <VisibleSimpleChar>
(^<skipped*>
    (new 
        (*parser (range #\! #\~))
    done)))

(define <NamedChar>
(^<skipped*>
    (new
        (*parser (word-ci "lambda"))
        (*pack (lambda (a) #\λ))
        (*parser (word-ci "newline"))
        (*pack (lambda (a) #\newline))
        (*parser (word-ci "nul"))
        (*pack (lambda (a) #\nul))
        (*parser (word-ci "page"))
        (*pack (lambda (a) #\page))
        (*parser (word-ci "return"))
        (*pack (lambda (a) #\return))
        (*parser (word-ci "space"))
        (*pack (lambda (a) #\space))
        (*parser (word-ci "tab"))
        (*pack (lambda (a) #\tab))
        (*disj 7)
    done)))
    
(define <HexChar>
    (new
        (*parser (range #\0 #\9))
        (*parser (range #\a #\f))
        (*parser (range #\A #\F))
        (*disj 3)
    done))
    
(define <HexUnicodeChar>
(^<skipped*>
    (new
        (*parser (char-ci #\x))
        (*parser <HexChar>) *plus
        
        (*guard (lambda (a)
                        (< (string->number 
                                (list->string `(,@a)) 16)
                            (string->number "10ffff" 16))))
        (*pack (lambda (a) (integer->char 
                                (string->number 
                                    (list->string `(,@a)) 16))))
                        
        (*caten 2)
        (*pack-with (lambda (a b) b))
        done)))

(define <HexFailed>
(^<skipped*>
    (new
        (*parser (char-ci #\x))
        (*parser <HexChar>) *plus
        
        (*guard (lambda (a)
                        (>= (string->number 
                                (list->string `(,@a)) 16)
                            (string->number "10ffff" 16))))                        
        (*caten 2)
        (*pack-with (lambda (a b) a))
        done)))
    

(define <Char>
(^<skipped*>
    (new 
        (*parser <fail>)
        (*parser <CharPrefix>)
        (*parser <NamedChar>)
        (*parser <HexUnicodeChar>)
        (*parser <VisibleSimpleChar>)
        (*parser <HexFailed>)
        *diff
        (*disj 3)
        
        (*caten 2)
        (*pack-with (lambda (a b) b))
        (*disj 2)
    done)))
    
;;;;;;;;;;;;;;;;;;;;number;;;;;;;;;;;;;;;;;;;;;

(define <Natural>
    (new 
        (*parser (range #\0 #\9)) *plus
        (*pack (lambda (a) 
                    (string->number 
                        (list->string a))))
    done))

(define <Integer>
    (new 
        (*parser (char #\+))
        (*parser <Natural>)
        (*caten 2)
        (*pack-with (lambda (a b) b))

        (*parser (char #\-))
        (*parser <Natural>)
        (*caten 2)
        (*pack-with (lambda (a b) (- 0 b)))

        (*parser <Natural>)
        (*pack (lambda (a) a))

        (*disj 3)
    done))
    
(define <Fraction>
    (new
        (*parser <Integer>)
        (*parser (char #\/))
        (*parser <Natural>)
        
        (*caten 3)
        (*pack-with (lambda (a b c) (/ a c)))
    done))

(define <Number>
(^<skipped*>
    (new
        (*parser <Fraction>)
        (*parser <Integer>)
        (*disj 2)
        (*pack (lambda (a) a))
        (*delayed (lambda () <SymbolChar>))
        (*parser (range #\0 #\9))
        (*disj 1)
        *diff
        *not-followed-by

  done)))

;;;;;;;;;;;;;;;;;;;;String;;;;;;;;;;;;;;;;;;;;;

(define <StringLiteralChar> 
  (new 	
        (*parser <any-char>)
	(*guard (lambda (ch) (not (char=? ch #\\))))
	(*pack (lambda (ch) ch))
  done))


(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
	 (*pack (lambda (_)  ch))
    done)))


(define <StringMetaChar>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*disj 6)
   done))


(define <StringHexChar>
    (new
        (*parser (char #\\))
        (*parser (char #\x))
        (*parser <HexChar>) *star
        (*guard (lambda (a)
                        (< (string->number 
                                (list->string `(,@a)) 16)
                            (string->number "10ffff" 16))))
        (*pack (lambda (a) (integer->char 
                                (string->number 
                                    (list->string `(,@a)) 16))))

	(*parser (char #\;))
        (*caten 4)
        (*pack-with (lambda (a b c d)  c))
    done))

(define <StringChar>
    (new 
         (*parser <StringLiteralChar>)
         (*parser <StringHexChar>)
         (*parser <StringMetaChar>)
        
        (*disj 3)
    done))

(define <String>
(^<skipped*>
 (new
        (*parser (char #\"))
       (*parser <StringChar>)
       (*parser (char #\")) 
       *diff 
       *star
       
       (*parser (char #\")) 

       (*caten 3)

       (*pack-with
	(lambda (open-delim chars close-delim)
          (list->string chars)))
    done)))

;;;;;;;;;;;;;;;;;;;;symbol;;;;;;;;;;;;;;;;;;;;;

(define <SymbolChar>
    (new
        (*parser (range #\a #\z))
        (*parser (range #\A #\Z))
        (*parser (range #\0 #\9))
	(*parser (char #\!))
	(*parser (char #\$))
	(*parser (char #\^))
	(*parser (char #\*))
	(*parser (char #\-))
	(*parser (char #\_))
	(*parser (char #\=))
	(*parser (char #\+))
	(*parser (char #\<))
	(*parser (char #\>))
	(*parser (char #\?))
	(*parser (char #\/))
        (*disj 15)
        (*pack (lambda (a) (char-downcase a)))
        
    done))
  
(define <Symbol>
(^<skipped*>
    (new 
        (*parser <SymbolChar>) *plus
        (*pack (lambda (a)
                    (string->symbol
                        (list->string a))))
                      
    done)))
    
;;;;;;;;;;;;;;;;;;;;proper-list;;;;;;;;;;;;;;;;

(define <ProperList>
    (new
        (*parser (char #\())
        (*delayed (lambda () <Sexpr>)) *star
        (*parser (char #\)))
        
        (*caten 3)
        (*pack-with (lambda (a b c) b))
        
    done))
    
;;;;;;;;;;;;;;;;;;;;ImproperList;;;;;;;;;;;;;;;

(define <ImproperList>
  (new 
    (*parser (char #\())
        (*delayed (lambda () <Sexpr>)) *plus
        (*parser (char #\.))
        (*delayed (lambda () <Sexpr>))
        (*parser (char #\)))
        
        (*caten 5)
        (*pack-with (lambda (a b c d e) `(,@b . ,d)))
    done))
    
;;;;;;;;;;;;;;;;;;;;Vector;;;;;;;;;;;;;;;;;;;;;

(define <Vector>
(^<skipped*>
    (new
        (*parser (char #\#))
        (*parser (char #\())
        (*delayed (lambda () <Sexpr>)) *star 
        
        (*parser (char #\)))
        (*caten 4)
        (*pack-with (lambda (a b c d)  (list->vector c)))
    done)))

;;;;;;;;;;;;;;;;;;;;Quoted;;;;;;;;;;;;;;;;

(define <Quoted>
(^<skipped*>
    (new
        (*parser (char #\'))
        (*delayed (lambda () <Sexpr>))
        
        (*caten 2)
        (*pack-with (lambda (a b) (list 'quote b)))
    done)))
    
;;;;;;;;;;;;;;;;;;;;QuasiQuoted;;;;;;;;;;;;;;;;

(define <QuasiQuoted>
(^<skipped*>
    (new
        (*parser (char #\`))
        (*delayed (lambda () <Sexpr>))
        
        (*caten 2)
        (*pack-with (lambda (a b) (list 'quasiquote b)))
    done)))
    
;;;;;;;;;;;;;;;;;;;;Unquoted;;;;;;;;;;;;;;;;;

(define <Unquoted>
(^<skipped*>
    (new
        (*parser (char #\,))
        (*delayed (lambda () <Sexpr>))
        
        (*caten 2)
        (*pack-with (lambda (a b) (list 'unquote b)))
    done)))
    
;;;;;;;;;;;;;;;;;;;;UnquoteAndSpliced;;;;;;;;

(define <UnquoteAndSpliced>
(^<skipped*>
    (new
        (*parser (word ",@"))
        (*delayed (lambda () <Sexpr>))
        
        (*caten 2)
        (*pack-with (lambda (a b) (list 'unquote-splicing b)))
    done)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;infix;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    
;;;;;;;;;;;;;;;InfixHelpers;;;;;;;;;;;;;;;;;;

(define <AddSubOp>
    (new
        (*parser (char #\+))
        (*parser (char #\-))
        (*disj 2)
    done))
    
    
(define <MulDivOp>
    (new
        (*parser (char #\*))
        (*parser (char #\/))
        (*disj 2)
    done))
    

(define <PowerSymbol>
    (new
        (*parser (char #\^))
        (*parser (word "**"))
        (*disj 2)
    done))

(define <InfixSymbolChar>
    (new
        (*parser <SymbolChar>)
        (*parser (char #\+))
        (*parser (char #\-))
        (*parser (char #\*))
        (*parser (word "**"))
        (*parser (char #\^))
        (*parser (char #\/))
        (*disj 6)
        *diff
    done))

(define <InfixSymbol>
(^<skipped*>
    (new
        (*parser <InfixSymbolChar>) *plus
        (*pack (lambda (a)
                    (string->symbol
                        (list->string a))))
    done)))
    
(define <InfixNumber>
(^<skipped*>
    (new
        (*parser <Fraction>)
        (*parser <Integer>)
        (*disj 2)
        (*pack (lambda (a) a))
        
        (*delayed (lambda () <InfixSymbolChar>))
        (*parser (range #\0 #\9))
        *diff
        
        *not-followed-by
  done)))
  
(define <InfixNeg>
(^<skipped*>
    (new
        (*parser (char #\-))
        
        (*delayed (lambda () <InfixNumber>))        
        (*delayed (lambda () <InfixSymbol>))
;;         (*delayed (lambda () <InfixFuncall>)) 
;;         *diff
        (*delayed (lambda () <InfixExpression>))
        (*disj 3)
        (*pack (lambda (b) `(- ,b)))
        
        (*caten 2)
        (*pack-with (lambda (a b) b))
        
    done)))
    
(define <InfixArgList>
(^<skipped*>
    (new
 
        (*delayed (lambda () <InfixExpression>)) 
        (*parser (char #\,))
        (*delayed (lambda () <InfixExpression>)) 
        (*caten 2)
        (*pack-with (lambda (a b) b)) *star
        
        (*caten 2)
        (*pack-with (lambda (a b) `( ,a ,@b)))
        
        (*parser <epsilon>) 
        (*disj 2)
    
    done)))


(define <InfixSexprEscape>
(^<skipped*>
    (new
        (*delayed (lambda () <InfixPrefixExtensionPrefix>)) 
        (*delayed (lambda () <Sexpr>))
        (*caten 2)
        (*pack-with (lambda (a b) b))         
    done)))  
    
(define <InfixParen>
    (new
        (*parser (char #\())
        (*delayed (lambda () <InfixExpression>)) 
        (*parser (char #\)))
        
        (*caten 3)
        (*pack-with (lambda (a b c) b))
    done))   
    
;;;;;;;;;;;;;;;;;;;;levels;;;;;;;;;;;;;;;;;;

(define <InfixAddSub>
(^<skipped*>
    (new 
        (*delayed (lambda () <InfixMulDiv>))
        (*parser <AddSubOp>)
        (*delayed (lambda () <InfixMulDiv>)) 
        (*caten 2)
        (*pack-with
        (lambda (head tail)
            (lambda (first)
                `(,(string->symbol (string head)) 
                  ,first
                  ,tail))))

        *star
        (*caten 2)
        (*pack-with (lambda (head lambda_tail)
                        (fold-left (lambda (operator element)
                                        (element operator)) 
                                    head
                                    lambda_tail)))       
          
      done)))

(define <InfixMulDiv>
(^<skipped*>
  (new 
        (*delayed (lambda () <InfixPower>)) 
        (*parser <MulDivOp>)
        (*delayed (lambda () <InfixPower>))
        
        (*caten 2)
        (*pack-with
        (lambda (head tail)
            (lambda (first)
                `(,(string->symbol (string head)) 
                  ,first
                  ,tail))))

        *star
        (*caten 2)
        (*pack-with (lambda (head lambda_tail)
                        (fold-left (lambda (operator element)
                                        (element operator)) 
                                    head
                                    lambda_tail)))
    done)))
    

(define <InfixPower>
(^<skipped*>
    (new 
        (*delayed (lambda () <InfixFuncall>)) 
        (*parser <PowerSymbol>)
        (*delayed (lambda () <InfixFuncall>)) 
        (*caten 2)
        (*pack-with
        (lambda (head tail)
            (lambda (first)
                `(,(string->symbol "expt") 
                  ,first
                  ,tail))))

        *star
        (*caten 2)
        (*pack-with (lambda (head lambda_tail)
                        (fold-left (lambda (operator element)
                                        (element operator)) 
                                    head
                                    lambda_tail)))          
    done)))
    

    
(define <InfixFuncall>
(^<skipped*>
    (new 
        (*delayed (lambda () <InfixArrayGet>)) 
        (*parser (char #\())
        (*delayed (lambda () <InfixArgList>)) 
        (*parser (char #\)))
       
        (*caten 3)
        (*pack-with (lambda (b c d) `(,@c)))

        *star
        (*caten 2)
        (*pack-with (lambda (name parameters)
                        (fold-left (lambda (little_name element)
                                        `( ,little_name ,@element)) 
                                    name
                                    parameters)))
    done)))
    
(define <InfixArrayGet>
(^<skipped*>
    (new 
        (*delayed (lambda () <LastLevel>)) 
        (*parser (char #\[))
        (*delayed (lambda () <InfixExpression>)) 
        (*parser (char #\]))
        
        (*caten 3)
        (*pack-with (lambda (b c d) c))
        *star
        (*caten 2)
        (*pack-with (lambda (head parameters)
                        (fold-left (lambda (little_name element)
                                        `(vector-ref ,little_name ,element))
                                    head
                                    parameters)))
    done)))
   
(define <LastLevel>
(^<skipped*>
    (new
        (*parser <InfixSexprEscape>)
        (*parser <InfixNumber>)
        (*parser <InfixNeg>)
        (*parser <InfixSymbol>)
        (*delayed (lambda () <InfixParen>))
        (*disj 5)
    done)))
           

(define <InfixExpression> 
 (^<skipped*>
    (new
         (*parser <InfixAddSub>)
    done)))

    
(define <InfixPrefixExtensionPrefix>
(^<skipped*>
    (new
        (*parser (word "##"))
        (*parser (word "#%"))
        (*disj 2)
    done)))
    

(define <InfixExtension>
(^<skipped*>
    (new
        (*parser <InfixPrefixExtensionPrefix>)
        (*parser <InfixExpression>)
        (*caten 2)
        (*pack-with (lambda (a b) b))
    done)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;sexpr;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <Sexpr>
(^<skipped*>
    (new
        
        (*parser <InfixExtension>)
        (*parser <Boolean>)
        (*parser <Number>)
        (*parser <Symbol>)
        (*parser <Char>)
        (*parser <String>)        
        (*parser <Vector>)
        (*parser <ProperList>)
        (*parser <ImproperList>)
        (*parser <Quoted>)
        (*parser <QuasiQuoted>)
        (*parser <Unquoted>)
        (*parser <UnquoteAndSpliced>)
        
        (*disj 13)
    done)))

