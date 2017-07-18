(define TREE '(1 (2 (3 4)) 6 (7 8 9)) )
(define TREE2 '(1 2 (4 5 (8 9)) (6 7) 3) )

(define map-tree
  (lambda (f tree)
    (cond ((null? tree) `())
          ((pair? tree) (cons (map-tree f (car tree)) (map-tree f (cdr tree)) ) )
          (else (f tree))
    )
  )
)

(define map-tree2
  (lambda (f tree)
    (cond ((null? tree) `())
          ((pair? tree) (map (lambda (t) (map-tree2 f t)) tree ) )
          (else (f tree))
    )
  )
)
;kadai-1-1
(map-tree even? TREE)
(map-tree even? TREE2)
(map-tree (lambda (x) (* x x)) TREE)
(map-tree (lambda (x) (* x x)) TREE2)
;kadai-1-2
(map-tree2 even? TREE)
(map-tree2 even? TREE2)
(map-tree2 (lambda (x) (* x x)) TREE)
(map-tree2 (lambda (x) (* x x)) TREE2)
