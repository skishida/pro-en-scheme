(define kakeizu
    (read
       (open-input-file "/usr/local/class/scheme/kakeizu")))
(define get-depth
  (lambda (l n)
    (if (<= n 1)
        (map car (cdr l)) ; reach the given depth 
        (cond ((null? l) '())
              ((pair? l) (apply append (map (lambda (ls) (get-depth ls (- n 1))) (cdr l) ) ))
              (else '() )
         )
    )
  )
)

(define search
  (lambda (l name n)
    (cond ((null? l) (list 0) )
          ((equal? name (car l)) (list n))
          ((pair? l) (apply append (map (lambda (ls) (search ls name (+ n 1))) (cdr l))))
          (else (list 0))
    )
  )
)

(define get-cousin 
  (lambda (l name)
    (get-depth l (apply + (search l name 0)) )
  )
)


(define get-path
  (lambda (l name)
    (cond ((null? l) '() )
          ((equal? name (car l)) (list (car l)))
          ((pair? l) 
           (let ( (r (apply append (map (lambda (ls) (get-path ls name)) (cdr l)))) )
             (if (equal? r '())
                 '()
                 (cons (car l) r)
                 )
             )
          )
          (else '())
    )
  )
)
;kadai-2-1
(get-depth kakeizu 1)
(get-depth kakeizu 3)
(get-depth kakeizu 6)
;kadai-2-2
(get-cousin kakeizu '秀忠)
(get-cousin kakeizu '吉宗)
(get-cousin kakeizu '家達)
;kadai-2-3
(get-path kakeizu '家光)
(get-path kakeizu '家治)
(get-path kakeizu '家慶)
