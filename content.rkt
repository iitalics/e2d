#lang racket/base
(require racket/gui
         ffi/cvector
         sgl/gl
         sgl/gl-vectors)

(provide bitmap->gl-texture)

(define (bitmap->gl-texture img)
  (let* ([width (send img get-width)]
         [height (send img get-height)]
         [gl-pixels (make-gl-ubyte-vector (* 4 width height))])
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

#|





|#
