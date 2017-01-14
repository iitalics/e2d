#lang racket
(require e2d/math
         e2d/content
         e2d/engine
         sgl/gl)


(define TRASH (queue-texture "trash.png"))


(define (post-gl)
  (load-all-content))

(define (draw-texture tex left right top bot)
  (glEnable GL_TEXTURE_2D)
  (glBindTexture GL_TEXTURE_2D (texture-gl-id tex))
  (let ([tc-l (texture-c-left tex)]
        [tc-r (texture-c-right tex)]
        [tc-t (texture-c-top tex)]
        [tc-b (texture-c-bottom tex)])
    (glBegin GL_QUADS)
    (glVertex2f right bot)
    (glTexCoord2f tc-r tc-t)
    (glVertex2f right top)
    (glTexCoord2f tc-l tc-t)
    (glVertex2f left top)
    (glTexCoord2f tc-l tc-b)
    (glVertex2f left bot)
    (glTexCoord2f tc-r tc-b)
    (glEnd))
  (glDisable GL_TEXTURE_2D))

(define (my-draw)
  (when (loaded-all-content?)
    (glClearColor 0.0 1.0 1.0 1.0)
    (glClear GL_COLOR_BUFFER_BIT)
    (draw-texture TRASH 50 250 50 260)))

(with-new-e2d-engine
  (lambda ()
    (e2d-add-hook 'draw my-draw)
    (e2d-add-hook 'post-gl post-gl)
    (e2d-start)))
