(define diff
  (lambda (l)
    (cond ((number? l) '0 )
          ((equal? l 'x) '1 )
          (else     
           (let ((o (car l)))
             (cond ((equal? o '+)  (cons '+ (map diff (cdr l))) )
                   ((equal? o '-)  (cons '- (map diff (cdr l))) )
                   ((equal? o '*)  (list '+ 
                                         (list '* (cadr l) (diff (caddr l)) ) 
                                         (list '* (diff (cadr l)) (caddr l) ) 
                                   ))
                   ((equal? o '**) (list '* (caddr l)
                                         (list '* (diff (cadr l))
                                               (list '** (cadr l) (- (caddr l) 1) )
                                         )
                                   ))
                   );cond
             )
          );else
    );cond
  ); lambda(l)
)

(define diff2
  (lambda (l d)
    (cond ((number? l) '0 )
          ((equal? l d) '1 );係数1,1次式 かつ 変数=微分対象
          ((pair? l);多項式
           (let ((o (car l)))
             (cond ((equal? o '+)  (cons '+ (map (lambda (ls) (diff2 ls d)) (cdr l) )))
                   ((equal? o '-)  (cons '- (map (lambda (ls) (diff2 ls d)) (cdr l) )) )
                   ((equal? o '*)  (list '+ 
                                         (list '* (cadr l) (diff2 (caddr l) d) ) 
                                         (list '* (diff2 (cadr l) d) (caddr l) ) 
                                   ))
                   ((equal? o '**) (list '* (caddr l)
                                         (list '* (diff2 (cadr l) d)
                                               (list '** (cadr l) (- (caddr l) 1) )
                                         )
                                   ))
                   );cond
             )
          );pair
          (else '0) ;係数1,1次式 かつ 変数!=微分対象
    );cond
  ); lambda(l)
)

;y - f(a) = f'(a)(x-a)
;y = (+ ('* f'(a) x) (- 0 (* f'(a) a)) f(a))
(define tangent
  (lambda (l xs)
    (define fx l)
    (define fdx (diff l))
    (let ((fa ((eval `(lambda (x), fx) (interaction-environment)) xs))
          (fda ((eval `(lambda (x), fdx) (interaction-environment)) xs))
          );let def
          (list '+
                (list '* fda 'x)
                (+ (- 0 (* fda xs)) fa)
          )
    )
  ); lambda(l xs)
)

;return non-zero-list
(define non-zero
  (lambda (l)
    (cond ((null? l) '())
          ((pair? l)
             (cond ((equal? (car l) 0) (non-zero (cdr l)))
                   (else (cons (car l) (non-zero (cdr l))))
             )
          )
    )
  );lambda
)

(define simple+
  (lambda (l)
    (let ((nzl (non-zero l)) )
       (cond ((null? nzl) '0)
             ((null? (cdr nzl)) (car nzl))
             (else (cons '+ nzl))
       )
    )
  );lambda
)
(define simple-
  (lambda (l)
    (let ((nzl (non-zero (cdr l))) )
       (cond ((null? nzl) (car l))
             (else (cons '- (cons (car l) nzl)))
       )
    )
  );lambda
)
(define simple*
  (lambda (l)
    (let ( (p (car l)) (q (cadr l)) )
      (cond ((or (equal? p 0) (equal? q 0) )  0)
            ((equal? p 1) q)
            ((equal? q 1) p)
            (else (cons '* l))
      )
    )
  );lambda
)
(define simple**
  (lambda (l)
    (let ((p (car l)) (q (cadr l)))
      (cond ((equal? q 0) 1)
            ((equal? q 1) p)
            (else (cons '** l))
      )
    )
  );lambda
)
(define simple
  (lambda (l)
    (cond ((number? l) l)
          ((symbol? l) l);記号
          ((pair? l);多項式
           (let ((o (car l)) (ls (map simple (cdr l) )) )
             (cond ((equal? o '+)  (simple+ ls) )
                   ((equal? o '-)  (simple- ls) )
                   ((equal? o '*)  (simple* ls) )
                   ((equal? o '**) (simple** ls) )
                   );cond
             )
          );pair
          (else '())
    );cond
  );lambda
)

;kadai-3-1
(diff 'x)
(diff '(+ x 5))
(diff '(+ (** x 2) (- (* 4 x) 3)))
;kadai-3-2
(define ** expt); is this position right?
(tangent '(+ (** x 2) (* 4 x) 3) 5) ;result is "(+ (* 14 x) -22)", not "(- (* 14 x) 22)"
(tangent '(+ (** x 3) (* -2 (** x 2)) 9) 2)
;kadai-3-3    
(diff2 'x 'x)
(diff2 'y 'x)
(diff2 '(* x y) 'x)
(diff2 '(* (* x 2) y) 'x)
(diff2 '(- x y) 'x)
(diff2 '(+ (** x 2) (** y 2)) 'y)
;kadai-3-4 TODO
(simple (diff2 '(* (* x 2) y) 'x))
(simple '(+ (- 0 (* (** x 0) (+ 0 1))) (- (- 0 3) (* 1 x)) ))
(simple (diff '(+ x 3)))
(simple '(+ (* 1 (* 7 (** x 0))) (- (+ (* 0 x) (* 3 x)) 0)))
(simple (diff '(+ (** x 2) (* 4 x) 5))) 
