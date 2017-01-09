#lang racket

(module+ main
  (require racket/gui/base
           ffi/cvector
           sgl/gl
           sgl/gl-vectors
           "math.rkt")

  (define (bitmap->gl-texture img)
    (let* ([width (send img get-width)]
           [height (send img get-height)]
           [gl-pixels (make-gl-ubyte-vector (* 4 width height))])
      (printf "width: ~a\nheight: ~a\nbytes: ~a\n"
              width height
              (* width height 4))
      ; read pixel data and move into `gl-pixels'
      (let ([pixels (make-bytes (* 4 width height))])
            (define (mv i j)
              (gl-vector-set! gl-pixels
                              i
                              (bytes-ref pixels j)))
            (send img get-argb-pixels
                  0 0
                  width height
                  pixels)
            (for ([i (in-range 0 (* 4 width height) 4)])
              (mv (+ i 0) (+ i 1))
              (mv (+ i 1) (+ i 2))
              (mv (+ i 2) (+ i 3))
              (mv (+ i 3) (+ i 0))))
      ; generate GL texture ID
      (let ([id (cvector-ref (glGenTextures 1) 0)])
        (glBindTexture GL_TEXTURE_2D id)
        (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
        (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
        (glTexImage2D GL_TEXTURE_2D
                      0 GL_RGBA8
                      width height
                      0 GL_RGBA
                      GL_UNSIGNED_BYTE
                      gl-pixels)
        id)))


  (define lain-bitmap
    (make-object bitmap%
                 (open-input-file "lain.png")
                 'png/alpha
                 #f))

  (define lain-tex #f)

  (define WIDTH 800)
  (define HEIGHT 600)

  (define init-time (current-milliseconds))
  (define (delta-time)
    (- (current-milliseconds) init-time))

  (define (initialize)
    (glViewport 0 0 WIDTH HEIGHT)
    (glEnable GL_BLEND)
    (glDisable GL_DEPTH)
    (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (glOrtho 0 WIDTH HEIGHT 0 -1 +1)
    (let ([id (bitmap->gl-texture lain-bitmap)])
      (printf "generated texture: ~a\n" id)
      (set! lain-tex id)))

  (define (draw)
    (glClearColor 0.3 0.7 1.0 1.0)
    (glClear GL_COLOR_BUFFER_BIT)
    (when lain-tex
      (glColor4f 1.0 1.0 1.0 1.0)
      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)
      (for* ([lainx (in-range 0 801 50)]
             [lainy (in-range 0 601 50)])
        (glPushMatrix)
        (glTranslatef lainx lainy 0)
        (glScalef 64 68 1)
        (glRotatef (+ lainx lainy (* (delta-time) -0.18)) 0 0 1)
        (glBindTexture GL_TEXTURE_2D lain-tex)
        (glEnable GL_TEXTURE_2D)
        (glBegin GL_QUADS)
        (for ([scoord (in-list '((-0.5 -0.5) (+0.5 -0.5) (+0.5 +0.5) (-0.5 +0.5)))]
              [tcoord (in-list '(( 0  0) ( 1  0) ( 1  1) ( 0  1)))])
          (glTexCoord2i (first tcoord)
                        (second tcoord))
          (glVertex3f (first scoord)
                      (second scoord)
                      0))
        (glEnd)
        (glPopMatrix))))


  (define glcanv%
    (class* canvas% ()
      (inherit swap-gl-buffers with-gl-context
               refresh)
      (super-new [style '(gl no-autoclear)]
                 [min-width WIDTH]
                 [min-height HEIGHT])

      (with-gl-context initialize)

      (define/override (on-paint)
        (with-gl-context
          (lambda ()
            (draw)
            (swap-gl-buffers)
            (glFlush))))
      ))

  (let* ([f (new frame% [label ""])]
         [c (new glcanv% [parent f])])
    (send f show #t)
    (send c refresh)
    (thread
     (lambda ()
       (let loop ()
         (when (send f is-shown?)
           (send c refresh)
           (sleep 1/45)
           (loop))))))

  )
