#lang racket/base
(require ffi/vector
         (only-in racket/unsafe/ops
                  unsafe-fl+
                  unsafe-fl*
                  unsafe-flsqrt)
         (only-in racket/match
                  match match*)
         (only-in racket/math
                  pi)
         racket/contract)
(provide
 (contract-out
  [tau real?]
  [deg/rad real?] [deg->rad (-> real? real?)]
  [rad/deg real?] [rad->deg (-> real? real?)]
  [normalize-angle (->* (real?)
                        ((one-of/c 'signed
                                   'unsigned))
                        real?)]
  [v2? (-> any/c boolean?)]
  [v2 (-> real? real? v2?)]
  [v2-x (-> v2? real?)]
  [v2-y (-> v2? real?)]
  [v2=? (-> v2? v2? boolean?)]
  [v2~a (-> v2? string?)]
  [v2-from-angle (-> real? v2?)]
  [v2-from-angle/deg (-> real? v2?)]
  [v2+ (-> v2? v2? v2?)]
  [v2* (case-> (-> real? v2? v2?)
               (-> v2? real? v2?))]

  [tmat? (-> any/c boolean?)]
  [tmat-identity (-> tmat?)]
  [tmat-translated (case-> (-> v2? tmat?)
                           (-> real? real? tmat?))]
  [tmat-rotated (-> real? tmat?)]
  [tmat-rotated/deg (-> real? tmat?)]
  [tmat-scaled (case-> (-> (or/c v2? real?) tmat?)
                       (-> real? real? tmat?))]
  [tmat=? (-> tmat? tmat? boolean?)]
  [tmat~a (-> tmat? string?)]
  [list->tmat (-> list? tmat?)]
  [tmat->list (-> tmat? pair?)]

  [tmat* (->* (tmat?) #:rest (or/c tmat? v2?) (or/c tmat? v2?))]
  [tmat*v2 (-> tmat? v2? v2?)]
  [tmat*tmat (-> tmat? tmat? tmat?)]
  ))


;; angles

(define tau 6.283185307179586)
(define deg/rad (/ 360.0 tau))
(define rad/deg (/ tau 360.0))

(define (deg->rad x) (* x rad/deg))
(define (rad->deg x) (* x deg/rad))

(define (normalize-angle x
                         [range-fmt 'unsigned])
  (define (norm-s x)
    (cond
      [(>= x pi)    (norm-s (- x tau))]
      [(< x (- pi)) (norm-s (+ x tau))]
      [else x]))
  (define (norm-u x)
    (cond
      [(>= x tau) (norm-u (- x tau))]
      [(< x 0.0)  (norm-u (+ x tau))]
      [else x]))
  (case range-fmt
    [(unsigned) (norm-u x)]
    [(signed)   (norm-s x)]))



;; v2:  2d vector (represented as 2-element f32vector)

(define (v2? v)
  (and (f32vector? v)
       (= 2 (f32vector-length v))))

(define (v2 x y)
  (f32vector (real->single-flonum x)
             (real->single-flonum y)))

(define (v2-from-angle ang)
  (f32vector (real->single-flonum (cos ang))
             (real->single-flonum (sin ang))))

(define (v2-from-angle/deg deg)
  (v2-from-angle (* deg rad/deg)))

(define (v2-x v) (f32vector-ref v 0))
(define (v2-y v) (f32vector-ref v 1))

(define (v2=? u v)
  (and (= (v2-x u) (v2-x v))
       (= (v2-y u) (v2-y v))))

(define (write-v2 v [out (current-output-port)])
  (fprintf out "<~a,~a>"
           (f32vector-ref v 0)
           (f32vector-ref v 1)))
(define (v2~a v)
  (let ([out (open-output-string)])
    (write-v2 v out)
    (get-output-string out)))

(define (v2+ u v)
  (let ([f+ unsafe-fl+])
    (v2 (f+ (v2-x u) (v2-x v))
        (f+ (v2-y u) (v2-y v)))))

(define (v2* a1 a2)
  (match* (a1 a2)
    [((? v2? v) (? real? a))
     (v2 (* (v2-x v) a)
         (* (v2-y v) a))]
    [((? real? a) (? v2? v))
     (v2 (* a (v2-x v))
         (* a (v2-y v)))]
    [(_ _)
     (error "invalid argument types to v2*")]))

(define (v2-magnitude v)
  (let ([x (v2-x v)]
        [y (v2-y v)]
        [f+ unsafe-fl+]
        [f* unsafe-fl*])
    (unsafe-flsqrt (f+ (f* x x)
                       (f* y y)))))




;; tmat:  transformation matrix (represented as 6-element f32vector)

(define (tmat? m)
  (and (f32vector? m)
       (= 6 (f32vector-length m))))

(define (tmat-identity)
  (f32vector 1.0f0 0.0f0 0.0f0
             0.0f0 1.0f0 0.0f0))

(define tmat-translated
  (case-lambda
    [(v)
     (f32vector 1.0f0 0.0f0 (v2-x v)
                0.0f0 1.0f0 (v2-y v))]
    [(x y)
     (f32vector 1.0f0 0.0f0 (real->single-flonum x)
                0.0f0 1.0f0 (real->single-flonum y))]))

(define (tmat-rotated ang)
  (let ([sn (real->single-flonum (sin ang))]
        [cs (real->single-flonum (cos ang))])
    (f32vector cs (- sn) 0.0f0
               sn    cs  0.0f0)))

(define (tmat-rotated/deg ang)
  (tmat-rotated (* ang rad/deg)))

(define tmat-scaled
  (case-lambda
    [(a)
     (if (v2? a)
         (f32vector (v2-x a) 0.0f0 0.0f0
                    0.0f0 (v2-y a) 0.0f0)
         (let ([a (real->single-flonum a)])
           (f32vector a 0.0f0 0.0f0
                      0.0f0 a 0.0f0)))]
    [(ax ay)
     (f32vector (real->single-flonum ax) 0.0f0 0.0f0
                0.0f0 (real->single-flonum ay) 0.0f0)]))

(define (tmat=? m1 m2)
  (for/and ([i (in-range 6)])
    (= (f32vector-ref m1 i)
       (f32vector-ref m2 i))))

(define (list->tmat lst)
  (cond
    [(= 6 (length lst))
     (apply f32vector
            (map real->single-flonum lst))]
    [else
     (error "expected 6 elements to create 2x3 matrix")]))

(define tmat->list
  f32vector->list)

(define (tmat~a mat)
  (match (f32vector->list mat)
    [(list a b c d e f)
     (format "(~a ~a ~a; ~a ~a ~a)"
             a b c d e f)]))


;; transformation

(define (tmat* m0 . args)
  (let-values
      ([(res _)
        (for/fold ([m m0]
                   [no-more? #f])
                  ([arg (in-list args)])
          (cond
            [no-more?
             (error "arguments following vector value in tmat*")]
            [(v2? arg)
             (values (tmat*v2 m arg)
                     #t)]
            [(tmat? arg)
             (values (tmat*tmat m arg)
                     #f)]
            [else
             (error "invalid argument type to tmat*")]))])
    res))

(define (tmat*v2 m v)
  ; ( a b c )  ( x ) = ( a*x + b*y + c )
  ; ( d e f )  ( y )   ( d*x + e*y + f )
  (let ([x (f32vector-ref v 0)]
        [y (f32vector-ref v 1)]
        [a (f32vector-ref m 0)]
        [b (f32vector-ref m 1)]
        [c (f32vector-ref m 2)]
        [d (f32vector-ref m 3)]
        [e (f32vector-ref m 4)]
        [f (f32vector-ref m 5)]
        [f+ unsafe-fl+]
        [f* unsafe-fl*])
    (v2 (f+ (f+ (f* a x)
                (f* b y))
            c)
        (f+ (f+ (f* d x)
                (f* e y))
            f))))

(define (tmat*tmat m1 m2)
  ; ( a b c ) ( q r s )   ( aq+bt ar+bu as+bv+c )
  ; ( d e f ) ( t u v ) = ( dq+et dr+eu ds+ev+f )
  ; ( 0 0 1 ) ( 0 0 1 )   (   0     0      1    )
  (let ([a (f32vector-ref m1 0)]
        [b (f32vector-ref m1 1)]
        [c (f32vector-ref m1 2)]
        [d (f32vector-ref m1 3)]
        [e (f32vector-ref m1 4)]
        [f (f32vector-ref m1 5)]
        [q (f32vector-ref m2 0)]
        [r (f32vector-ref m2 1)]
        [s (f32vector-ref m2 2)]
        [t (f32vector-ref m2 3)]
        [u (f32vector-ref m2 4)]
        [v (f32vector-ref m2 5)]
        [f+ unsafe-fl+]
        [f* unsafe-fl*])
    (f32vector (f+ (f* a q) (f* b t))
               (f+ (f* a r) (f* b u))
               (f+ (f+ (f* a s) (f* b v)) c)
               (f+ (f* d q) (f* e t))
               (f+ (f* d r) (f* e u))
               (f+ (f+ (f* d s) (f* e v)) f))))
