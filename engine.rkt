#lang racket
(require racket/gui/base
         sgl/gl
         data/gvector
         e2d/math)

(struct e2d-engine
  ([frame #:mutable]
   [look #:mutable]
   tick-hooks
   draw-hooks
   extra-gl-hooks))

(define (make-e2d-engine)
  (e2d-engine #f
               '(800 600 #f)
               (gvector)
               (gvector)
               (gvector)))

(define (e2d-engine-title ng) "e2d")


;; (current-e2d-engine) : e2d-engine?
(define current-e2d-engine (make-parameter #f))

;; (e2d-windowed w h : integer?
;;               [resize? : boolean? = #f]
;;               [ng : e2d-engine? = (current-e2d-engine)])
(define (e2d-windowed w h [resizable? #f]
                      [ng (current-e2d-engine)])
  (set-e2d-engine-look! ng
                         (list w h resizable?)))

(define (e2d-fullscreen [ng (current-e2d-engine)])
  (set-e2d-engine-look! ng
                         'fullscreen))

(define (e2d-get-size ng)
  (match (e2d-engine-frame ng)
    [#f (values 0 0)]
    [frame
     (send frame get-client-size)]))

(define (e2d-start [ng (current-e2d-engine)])
  (let ([f (new e2d-frame% [ng ng])])
    (send f show #t)
    (send f tick)))

(define (with-new-e2d-engine fn)
  (parameterize ([current-e2d-engine (make-e2d-engine)])
    (fn)))

(define e2d-frame%
  (class* frame% ()
    (inherit show fullscreen
             is-shown?)
    (init-field ng)
    (set-e2d-engine-frame! ng this)

    ; determine frame style from ng
    (match (e2d-engine-look ng)
      [(list w h resiz?)
       (super-new [label (e2d-engine-title ng)]
                  [width w]
                  [height h]
                  [style (if resiz?
                             '()
                             '(no-resize-border))])]
      ['fullscreen
       (super-new [label (e2d-engine-title ng)]
                  [style '(fullscreen-button)])
       (fullscreen #t)])

    (define canv (new e2d-canvas%
                      [parent this]
                      [ng ng]))

    (define/public (tick)
      (when (is-shown?)
        (send canv refresh)
        (queue-callback (lambda () (tick)) #f)))

    (define/override (on-size w h)
      (send canv sized))
    ))

(define e2d-canvas%
  (class* canvas% ()
    (inherit with-gl-context
             swap-gl-buffers
             refresh)
    (super-new [style '(gl no-autoclear)]
               [min-width 400]
               [min-height 300])
    (init-field ng)

    (define/override (on-paint)
      (with-gl-context
        (lambda ()
          (glClearColor 0.0 0.0 1.0 1.0)
          (glClear GL_COLOR_BUFFER_BIT)
          (glFlush)
          (glColor4f 1.0 0.0 0.0 1.0)
          (glBegin GL_LINES)
          (glVertex3f 0 0 0)
          (let-values ([(w h) (e2d-get-size ng)])
            (glVertex3f w h 0))
          (glEnd)
          (swap-gl-buffers))))

    (define/public (sized)
      (let-values ([(w h) (e2d-get-size ng)])
        (with-gl-context
          (lambda ()
            (glViewport 0 0 w h)
            (glMatrixMode GL_PROJECTION)
            (glLoadIdentity)
            (glOrtho 0 w h 0 -1 +1)))))

    (with-gl-context
      (lambda ()
        (glDisable GL_DEPTH)
        (glEnable GL_BLEND)
        (glBlendFunc GL_SRC_ALPHA
                     GL_ONE_MINUS_SRC_ALPHA)))

    ))

(define (go)
  (with-new-e2d-engine
    (lambda ()
      (e2d-start))))
