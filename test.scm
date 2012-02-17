(define nil '())

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list tree)
  (if (null? tree)
      '()
      (append (tree->list (left-branch tree))
              (cons (entry tree)
                    (tree->list (right-branch tree))))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


(define (intersection-set set1 set2)
  (display (tree->list set1))(display "----")(display (tree->list set2))(newline)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((entry1 (entry set1))
            (entry2 (entry set2))
            (left-branch1 (left-branch set1))
            (left-branch2 (left-branch set2))
            (right-branch1 (right-branch set1))
            (right-branch2 (right-branch set2)))
        (cond ((= entry1 entry2)
               (make-tree entry1
                          (intersection-set left-branch1 left-branch2)
                          (intersection-set right-branch1 right-branch2)))
              ((> entry2 entry1)
               (union-tree
                (intersection-set set1 left-branch2)
                (intersection-set right-branch1 set2)))
              ((> entry1 entry2)
               (union-tree
                (intersection-set set2 left-branch1)
                (intersection-set right-branch2 set1)))))))

(define (union-set set1 set2)


;(define s1 (list->tree (list 1 2 3 6 8)))
;(define s2 (list->tree (list 3 4 5 7 9)))

(define s1 (list->tree (list 1 2 3)))
(define s2 (list->tree (list 2 3 4)))


(intersection-set s1 s2)
