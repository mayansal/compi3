   



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  box   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  lambda-nil   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(remove-applic-lambda-nil
'(applic
(lambda-simple
(fact)
(seq ((set (var fact) (box (var fact)))

(box-set
(var fact)
(lambda-simple
(n)
(if3 (applic (var zero?) ((var n)))
(const 1)

(applic
  (var *)
  ((var n)
    (applic
      (box-get (var fact))
      ((applic (var -) ((var n) (const 1))))))))))
(applic
  (lambda-simple () (applic (box-get (var fact)) ((const 5))))
  ()) 
)))
((const #f))
))


(applic
  (lambda-simple () 1)
  ())


(remove-applic-lambda-nil '(applic (lambda-simple () (applic (lambda-simple () 1) ())) ()))
  ;#t
(is_redundant? '(applic (lambda-simple (a) 1) ()))
  ;#f
(is_redundant? '(applic (lambda-simple () 1) ((const 2))))
  ;#f but eyzlenu returns #t
  ;we didn't take care of that because it's not valid lambda syntax
(is_redundant? '((lambda-simple (a) 1) ()))
  ;#f
(is_redundant? '(applic (lambda-var (a) 1) ()))
  ;#f

(remove-applic-lambda-nil (parse '(lambda () a (lambda () (lambda () b)))))

(remove-applic-lambda-nil (parse '((lambda a a))))

(remove-applic-lambda-nil (parse '((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () (+)))))))))))))))))

(remove-applic-lambda-nil (parse   '(((lambda () f)) ((lambda () g)) ((lambda () h)) (z (m c (d ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () (+)))))))))))))))))))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  nested-defines   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *test-expr*
    '(define my-even? (lambda (e)
                            (define even? (lambda (n) (or (zero? n) (odd? (- n 1)))))
                            (define odd?  (lambda (n) (and (positive? n) (even? (- n 1)))))
                            (even? e))))    
 
                           
> (parse *test-expr*)
(def (var my-even?)
      (lambda-simple (e)
            (seq ((def (var even?)
                        (lambda-simple (n)
                            (or ((applic (var zero?) ((var n)))
                                 (applic (var odd?)  ((applic (var -) ((var n) (const 1)))))))))
                  (def (var odd?)
                        (lambda-simple (n)
                            (if3 (applic (var positive?) ((var n)))
                                 (applic (var even?) ((applic (var -) ((var n) (const 1)))))
                                 (const #f))))
                  (applic (var even?) ((var e)))))))

(seq ((const 1)
      (const 2)))
                  
> (eliminate-nested-defines
(parse *test-expr*))
    
(def (var my-even?)
     (lambda-simple (e)
        (applic (lambda-simple (even? odd?)
                    (seq ((set (var even?)
                               (lambda-simple (n)
                                  (or ((applic (var zero?) ((var n)))
                                       (applic (var odd?) ((applic (var -) ((var n) (const 1)))))))))
                          (set (var odd?)
                               (lambda-simple (n)
                                  (if3 (applic (var positive?) ((var n)))
                                       (applic (var even?) ((applic (var -) ((var n) (const 1)))))
                                       (const #f))))
                          (applic (var even?) ((var e))))) )
                 ((const #f) (const #f)) )  ))




////////////////////////////////////////////////////////
(eliminate-nested-defines '(def (var my-even?)
        (lambda-simple (e)
              (seq ((def (var even?)
                          (lambda-simple (n)
                              (or ((applic (var zero?) ((var n)))
                                   (applic (var odd?)  ((applic (var -) ((var n) (const 1)))))))))
                    (def (var odd?)
                          (lambda-simple (n)
                              (if3 (applic (var positive?) ((var n)))
                                   (applic (var even?) ((applic (var -) ((var n) (const 1)))))
                                   (const #f))))
                    (applic (var even?) ((var e))))))))

;/// winterfeld
(eliminate-nested-defines (parse '(lambda (a . b) (define x 1) 1)))

(eliminate-nested-defines '(def (var a) (lambda-opt () (seq ((const 1)(const 2))))))



(define create_set
  (lambda (def_element)
    `(set (cadr def_element) (caddr def_element))))


((def (var a) (const 1)) (def (var b) (const 2)) (def (var c)(const 3)))

((const 10)(const 20))

(((def (var a) (const 1)) (def (var b) (const 2)) (def (var c)(const 3)))
 ((const 10)(const 20)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;