(define nil '())

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (car (cdr f)))

(define (edge2-frame f)
  (car (cdr (cdr f))))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment p)
  (car p))

(define (end-segment p)
  (cdr p))

(define (draw-line p1 p2)
  (display "line ")
  (display p1)
  (display p2)
  (newline))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (paint-frame f)
  (segments->painter (list (origin-frame f)
                           (edge1-frame f)
                           (add-vect (edge1-frame f) (edge2-frame f))
                           (edge2-frame f)
                           (origin-frame f))))

(define canvas (make-frame (make-vector 0 0)
                           (make-vector 0 100)
                           (make-vector 100 0)))

(define f (make-frame (make-vector 1 1)
                      (make-vector 0 4)
                      (make-vector 5 0)))

(paint-frame f)