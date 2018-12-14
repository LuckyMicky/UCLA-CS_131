
;; for eval
(define ns (make-base-namespace))

;;Compare constant literals
(define (compare-constants x y)
  (if (equal? x y) x
    (;;else_1
      if (and (equal? x #t) (equal? y #f))
          '%
      (;;else_2
        if (and (equal? x #f) (equal? y #t))
          '(not %)
        (;;else_3
          cons 'if (cons '% (cons x (cons y '()))) ;;I tired '(if % x y) first but it does not work, ask Eggert why
        )
        )
    )
  )
)

;; to check if the input argument is a constant
(define (check-const x)
    (if (number? x) 
      #t
      (if (string? x) 
        #t
        (if (boolean? x) 
          #t
          (if (char? x) 
            #t
            #f
          )
        ) 
      )
    )
)

;; to check if the length of the two input lists are equal
(define (check-len l1 l2)
  (if (eqv? (length l1) (length l2))
    #t
    #f
  )
)


;; to check if the first input argument(element) is in the list(second input argument)
(define (check-in x l)
  (if (member x l)
    #t
    #f
  )

)

;; bind two variables x & y
(define (bind x y)
  (string->symbol (string-append (symbol->string x) "!" (symbol->string y))
  )
)


;helper functions to track variable binding
(define (add-dictionary dict pair)
  (cons pair dict)
)

(define (get-dictionary dict key)
  (let ((pair (assoc key dict))) 
    (if pair (car(cdr pair)) 
      #f
    )
  )
)

(define (get-key dict value)
  (get-dictionary (map reverse dict) value)
  )

; remove an element x from list l
(define (rmv-from-list x l)
  (if (equal? x (car l))
    (cdr l)
    (
      cons (car l) (rmv-from-list x (cdr l))
    )
  )
)

;fixes the format inside the identifiers section of the let expression
(define (fix-let-identifier id1 id2 acc loc)
  (if (null? id1) 
    (list (append (list acc) (list loc)))
    (if (eqv? (car (car id1)) (car (car id2)))
        (cons `(,(car (car id1)) ,(car(delegator (cdr (car id1))  (cdr (car id2)) acc))) (fix-let-identifier (cdr id1) (cdr id2) acc loc))
        (cons `( ,(bind (car (car id1)) (car(car id2))) ,(car (delegator (cdr (car id1))  (cdr (car id2)) (add-dictionary loc (append (list (car(car id1))) (list (car(car id2)))))))) (fix-let-identifier (cdr id1) (cdr id2) acc (add-dictionary loc (append (list (car(car id1))) (list (car(car id2)))))))
    )
  )
)


(define base-compare ;lowest level comparison of expressions, can't handle nested expressions. Uses bindings stored in the accumulator to output correct variables
  (lambda (l1 l2 acc)
    (cond
      [ (and (symbol? l1) (symbol? l2)) (car(base-compare (list l1) (list l2) acc))]
      [(not( check-len l1 l2)) `(if % ,l1 ,l2)] 
      [(null? l1) '() ] 
      [(or  (and (list? (car l1)) (list? (car l2)))  (and (check-const (car l1)) (check-const (car l2))) ) (cons
                                                  (delegator (car l1) (car l2) acc) (delegator (cdr l1) (cdr l2) acc) ) ]
      [(get-dictionary acc (car l1)) (cond
      																	[(equal? (car l2) (get-dictionary acc (car l1)) ) (cons (bind (car l1) (car l2)) (delegator  (cdr l1) (cdr l2) acc)) ]
      																	[(get-key acc (car l2)) (cons `(if % ,(bind (car l1) (get-dictionary acc (car l1))) ,(bind (get-key acc (car l2)) (car l2))) (delegator  (cdr l1) (cdr l2) acc))]
      																	[else (cons `(if % ,(bind (car l1) (get-dictionary acc (car l1))) ,(car l2)) (delegator  (cdr l1) (cdr l2) acc))]
      	)]
      [(get-key acc (car l2)) (cond
      																	[(equal? (car l1) (get-key acc (car l2)) ) (cons (bind (get-key acc (car l2)) (car l2)) (delegator  (cdr l1) (cdr l2) acc)) ]
      																	[(get-dictionary acc (car l1)) (cons `(if % ,(bind (car l1) (get-dictionary acc (car l1))) ,(bind (get-key acc (car l2)) (car l2))) (delegator  (cdr l1) (cdr l2) acc))]
      																	[else (cons `(if % ,(car l1) ,(bind (get-key acc (car l2)) (car l2)) ) (delegator  (cdr l1) (cdr l2) acc))]
      	)]
      [(eqv? (car l1) (car l2) ) (cons (car l1) (delegator (cdr l1) (cdr l2) acc))] 
      [else (cons `(if % ,(car l1) ,(car l2)) (delegator (cdr l1) (cdr l2) acc)) ] 
      )))


(define (compare-lambda l1 l2 acc)
  (if (equal? (car (cdr (car l1))) (car (cdr (car l2))))
    (cons`(lambda ,(car (cdr (car l1))) ,(delegator  (car(cdr(cdr (car l1)))) (car(cdr(cdr (car l2)))) acc))
           (delegator (cdr l1) (cdr l2) acc))
    (cons `(lambda ,(car(rmv-from-list (last (fix-lamda-identifiers (car (cdr (car l1))) (car (cdr (car l2))) acc '() )) (fix-lamda-identifiers (car (cdr (car l1))) (car (cdr (car l2))) acc '() ))) 
                   ,(delegator (car (cdr (cdr (car l1)))) (car (cdr (cdr (car l2)))) (last (fix-lamda-identifiers (car (cdr (car l1))) (car (cdr (car l2))) acc '() )))) (delegator (cdr l1) (cdr l2) '()))
  )
)

; recursively adding variable to acc list
(define (fix-lamda-identifiers id1 id2 acc param)
  (if (null? id1)
    (cons (reverse param) (list acc)) 
    (if (eqv? (car id1) (car id2))
      (fix-lamda-identifiers (cdr id1) (cdr id2) acc (append (list(car id1)) param))
      (fix-lamda-identifiers (cdr id1) (cdr id2) (add-dictionary acc (append (list (car id1)) (list (car id2)))) (append (list (bind (car id1) (car id2))) param))
    )
  )

)

(define (compare-let l1 l2 accumulator)
  (cons 'let (cons (rmv-from-list (last (fix-let-identifier (car (cdr l1) ) (car (cdr l2)) accumulator '())) (fix-let-identifier (car (cdr l1)) (car (cdr l2)) accumulator '())) 
                   (delegator (cdr (cdr l1)) (cdr (cdr l2)) (car(cdr(last (fix-let-identifier (car (cdr l1)) (car (cdr l2)) accumulator '())))))
             )
  ) 
)


(define (compare-quote l1 l2 accumulator)
  (if (equal? (car (cdr l1))(car (cdr l2)))
    `,l1
    (cons 'if (cons '% (cons l1 (cons l2 '()))))
  )
)

;looks at given expressions and delegates what kind of comparison needs to be made
(define delegator
  (lambda (l1 l2 acc)
    (cond
      [ (null? l1) '()]; null
      [ (equal? '() l1) '()]; empty list
      [ (and (check-const l1) (check-const l2) ) ( compare-constants l1 l2)] ;constant
      [ (and (symbol? l1) (symbol? l2)) (base-compare l1 l2 acc)]; catch symbols that aren't in lists
      [ (and (eqv? 'let (car l1)) (eqv? 'let (car l2)) ) ( compare-let l1 l2 acc)] ;let expressions
      [ (and (list? (car l1)) (list? (car l2)))  (cond ;Check that both are a list of lists then proceed
                            [ (and (eqv? 'lambda (car (car l1))) (eqv? 'lambda (car(car l2)))  ) ( compare-lambda l1 l2 acc)] ;lambda 
                            [ (and (eqv? '() (cdr l1)) (eqv? '() (cdr l2))) `(,(delegator (car l1) (car l2) acc))] ; otherwise there are extra parentheses, this removes them and reevaluates
                            [ else (append `(,(delegator (car l1) (car l2) acc)) (delegator (cdr l1) (cdr l2) acc))]
                            )]
      [ (and (eqv? ( car l1) 'quote) (eqv? (car l2) 'quote) )  (compare-quote l1 l2 acc)] ; quote 

      ;catches case where list heads differ and one starts with 'if'
      [( and (not ( eqv? (car l1) (car l2))) (or (eqv? (car l1) 'if) (eqv? (car l2) 'if)))  `(if % ,l1 ,l2) ]
      [else (base-compare l1 l2 acc)]

      )))

;; accumulator will check variable binding dynamically
(define (compare l1 l2 accumulator)
  (if (and (check-const l1) (check-const l2))
    (delegator l1 l2 accumulator)
    (if (and (list? l1) (list? l2)) 
      (delegator l1 l2 accumulator)
      (cons 'if (cons '% (cons l1 (cons l2 '())))) ;;I tired '(if % x y) first but it does not work, ask Eggert why
    )
  )
)

;; call expr-compare with empty accumulator
(define (expr-compare l1 l2)
  (compare l1 l2 '())
) 

;; add a bool value into l
(define (put b l)
  (if (null? l)
    '()
    (if (eqv? '% l)
      b
      (if (check-const l)
        l
        (if (eqv? '% (car l))
          (cons b (put b (cdr l)))
          (if (list? (car l))
            (cons (put b (car l)) (put b (cdr l)))
            (cons (car l) (put b (cdr l)))
          )
        )
      )
    )
  )
)

;check the result of (expr-compare l1 l2) 
(define (test-expr-compare l1 l2)
  (if (and (equal? (eval l1 ns) (eval (put #t (expr-compare l1 l2)) ns )) 
           (equal? (eval l2 ns) (eval (put #f (expr-compare l1 l2)) ns )))
    #t 
    #f
  )
)


;expr-compare test cases

;; The test cases will have footnote comments corresponding to the above
(define test-expr-x '(if #t ;test #t/#f differences being (%) or (not %) appropriately
       (let ((a 1) (b 2)) ;test lets
   ((lambda (a b) (cons (+ a b) (- a b))) a b)) ; test lists & lambdas with same binding
       (let ((c 3) (d 4)) ;test lets
   ((lambda (c d) (list c d)) c d))));test lambdas with different binding

(define test-expr-y '(if #f
       (let ((a 1) (b 2))
   ((lambda (a b) (list (- a b) (+ a b))) a b))
       (let ((c 3) (d 4))
   ((lambda (d c) (cons c d)) c d))))

