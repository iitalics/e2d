#lang racket
(require racket/gui/base
         sgl/gl
         data/gvector
         e2d/math)
(provide
 e2d-engine?
 make-e2d-engine
 current-e2d-engine
 with-new-e2d-engine

 e2d-windowed
 e2d-fullscreen
 e2d-start
 e2d-get-size

 e2d-add-hook
 )


(struct e2d-engine
  ([frame #:mutable]
   [look #:mutable]
   tick-hooks
   draw-hooks
   post-gl-hooks))

(define (make-e2d-engine)
  (e2d-engine #f
              '(800 600 #f)
              (gvector)
              (gvector)
              (gvector)))

;; (current-e2d-engine) : e2d-engine?
(define current-e2d-engine (make-parameter #f))

;; (with-new-e2d-engine [fn : (-> any)]) -> any
(define (with-new-e2d-engine fn)
  (parameterize ([current-e2d-engine (make-e2d-engine)])
    (fn)))

;; (e2d-windowed [w h : integer?]
;;               [resize? : boolean? = #f]
;;               [ng : e2d-engine? = (current-e2d-engine)])
(define (e2d-windowed w h [resizable? #f]
                      [ng (current-e2d-engine)])
  (set-e2d-engine-look! ng
                        (list w h resizable?)))

;; (e2d-fullscreen [ng : e2d-engine? = (current-e2d-engien)])
(define (e2d-fullscreen [ng (current-e2d-engine)])
  (set-e2d-engine-look! ng
                        'fullscreen))

;; (e2d-get-size [ng : e2d-engine? = (current-e2d-engine)])
;;   -> integer?
;;      integer?
(define (e2d-get-size ng)
  (match (or (e2d-engine-frame ng)
             (e2d-engine-look ng))
    [(list w h w?)
     (values w h)]
    [frame
     (send frame get-client-size)]
    [_
     (values 0 0)]))

;; (e2d-start [ng : e2d-engine? = (current-e2d-engine)])
(define (e2d-start [ng (current-e2d-engine)])
  (let ([f (new e2d-frame% [ng ng])])
    (send f show #t)
    (send f tick)))

;; (e2d-add-hook [s : (or/c 'tick
;;                         'draw
;;                         'post-gl)]
;;              [fn : (-> any)]
;;              [ng : e2d-engine? = (current-e2d-engine)])
(define (e2d-add-hook s fn [ng (current-e2d-engine)])
  (gvector-add! (case s
                  [(tick) (e2d-engine-tick-hooks ng)]
                  [(draw) (e2d-engine-draw-hooks ng)]
                  [(post-gl) (e2d-engine-post-gl-hooks ng)])
                fn))

;; Subclass of GUI frame%, for initializing the e2d engine's window.
(define e2d-frame%
  (class* frame% ()
    (inherit show fullscreen
             is-shown?)
    (init-field ng)
    (set-e2d-engine-frame! ng this)

    ; determine frame style from ng
    (match (e2d-engine-look ng)
      [(list w h resiz?)
       (super-new [label "e2d"]
                  [width w]
                  [height h]
                  [style (if resiz?
                             '()
                             '(no-resize-border))])]
      ['fullscreen
       (super-new [label "e2d"]
                  [style '(fullscreen-button)])
       (fullscreen #t)])

    (define canv (new e2d-canvas%
                      [parent this]
                      [ng ng]))

    (define/public (tick)
      (when (is-shown?)
        (parameterize ([current-e2d-engine ng])
          (for ([fn (in-gvector (e2d-engine-tick-hooks ng))])
            (fn)))
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

    ;;; Draw the screen
    (define/override (on-paint)
      (with-gl-context
        (lambda ()
          (parameterize ([current-e2d-engine ng])
            (for ([fn (in-gvector (e2d-engine-draw-hooks ng))])
              (fn))
            (glFlush)
            (swap-gl-buffers)
            (for ([fn (in-gvector (e2d-engine-post-gl-hooks ng))])
              (fn))))))

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
