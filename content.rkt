#lang racket/base
(require racket/gui
         ffi/cvector
         sgl/gl
         sgl/gl-vectors
         racket/generic
         data/queue)
(provide
 resource?
 resource-loaded?
 unload-resource

 content-mgr?
 make-content-mgr
 current-content-mgr
 with-new-content-mgr
 loaded-all-content?
 load-all-content

 texture?
 texture-c-left texture-c-right
 texture-c-top texture-c-bottom
 texture-gl-id
 texture-size texture-width texture-height
 queue-texture

 tileset?
 tileset-tile-size tileset-tile-width tileset-tile-height
 tileset-texture
 tileset-textures
 queue-tileset
 )





;;;; Resources ;;;;

;;; Loaded texture data
(struct texdata (gl-id width height))

;;; Generic resource
(define-generics resource
  [resource-loaded? resource]
  [unload-resource resource])

;;; Base type for image resources (GL textures)
;;; The texdata object is put in a (box ...) so that the texture
;;;  data can be loaded AFTER the texture object is created.
(struct imgres (data/box)
  #:methods gen:resource
  [(define (unload-resource ir)
     (let ([d/b (imgres-data/box ir)])
       (match d/b
         [(box #f) (void)]
         [(box (texdata gl-id _ _))
          (glDeleteTextures (vector gl-id))
          (set-box! d/b #f)])))])

;;; A texture is a rectangle region of an image.
(struct texture imgres
  (c-left c-right c-top c-bottom))

;;; Get the GL texture ID of this texture. Raises an exception
;;;  if the texture has not be loaded yet.
;; (texture-gl-id [tex : texture?]) -> integer?
(define (texture-gl-id tex)
  (match tex
    [(texture (box (texdata gl-id _ _)) _ _ _ _) gl-id]
    [_
     (error "cannot retrieve GL texture ID of texture that is not loaded yet.")]))

;;; Get the size of a texture. Raises an exception if
;;;  the image is not yet loaded.
;; (texture-size [tex : texture?])
;;   -> real?
;;      real?
;; (texture-width [tex : texture?]) -> real?
;; (texture-height [tex : texture?]) -> real?
(define (texture-size tex)
  (match tex
    [(texture (box (texdata _ w h)) l r t b)
     (values (* w (- r l))
             (* h (- b t)))]
    [_
     (error "cannot retrieve size of texture that is not loaded yet.")]))
(define (texture-width tex)
  (let-values ([(w h) (texture-size tex)]) w))
(define (texture-height tex)
  (let-values ([(w h) (texture-size tex)]) h))

(define (texture/gl-bind-and-coords tex)
  (match tex
    [(texture (box (texdata gl-id _ _)) l r t b)
     (void)]))


;;; A tileset is an image divided up into ``tiles'' of
;;;  equal with and height.
(struct tileset imgres
  (tile-width tile-height x-spacing y-spacing))

;;; Retrieves a single tile out of a tileset, by its
;;;  x and y coordinates.
;; (tileset-texture [tiles : tileset?]
;;                  [x-coord y-coord : integer?])
;;   -> texture?
(define (tileset-texture tiles
                         x-coord y-coord)
  (match tiles
    [(tileset (and d/b (box (texdata gl-id total-w total-h)))
              tile-w tile-h
              x-space y-space)
     (cond
       [(and (zero? x-space) (zero? y-space))
        (let ([x-ratio (/ tile-w total-w)]
              [y-ratio (/ tile-h total-h)])
          (texture d/b
                   (* x-coord x-ratio)
                   (* y-coord y-ratio)
                   x-ratio
                   y-ratio))]
       [else
        (let ([x-ratio (/ (+ tile-w x-space) total-w)]
              [y-ratio (/ (+ tile-h y-space) total-h)])
          (texture d/b
                   (* x-coord x-ratio)
                   (* y-coord y-ratio)
                   (/ tile-w total-w)
                   (/ tile-h total-h)))])]
    [_
     (error "cannot retrieve tile out of tileset that is not loaded yet.")]))

;;; Retrieves all textures out of a tileset, in a row-major
;;;  order vector.
;; (tileset-textures [tiles : tileset?]) -> (vectorof texture?)
(define (tileset-textures tiles)
  (match tiles
    [(tileset (box (texdata _ total-w total-h))
              tile-w tile-h
              x-space y-space)
     (let ([nx (quotient total-w (+ tile-w x-space))]
           [ny (quotient total-h (+ tile-h y-space))])
       (for*/vector #:length (* nx ny)
                    ([x (in-range nx)]
                     [y (in-range ny)])
         (tileset-texture tiles x y)))]
    [_
     (error "cannot retrieve tile out of tileset that is not loaded yet.")]))

;;; Returns the width and height of each tile in the tileset.
;; (tileset-tile-size [tiles : tileset?])
;;   -> integer?
;;      integer?
(define (tileset-tile-size tiles)
  (values (tileset-tile-width tiles)
          (tileset-tile-height tiles)))





;;;; Content manager ;;;;

(struct content-mgr (to-load loaded))

;;; Create a new content manager.
;; (make-content-mgr) -> content-mgr?
(define (make-content-mgr)
  (content-mgr (make-queue)
               (make-queue)))

;;; Parameter for a global content manager.
;; (current-content-mgr) : content-mgr?
(define current-content-mgr (make-parameter (make-content-mgr)))

;;; Call a thunk with a new current content manager.
;; (with-new-content-mgr [fn : (-> any)]) -> any
(define (with-new-content-mgr fn)
  (parameterize ([current-content-mgr (make-content-mgr)])
    (fn)))

(define-generics pending-resource
  [load-resource pending-resource])

;; (loaded-all-content? [mgr : content-mgr? = (current-content-mgr)])
;;   -> boolean?
(define (loaded-all-content? [mgr (current-content-mgr)])
  (queue-empty? (content-mgr-to-load mgr)))

;;; Load all of the queued content in a content manager.
;; (load-all-content [mgr : content-mgr? = (current-content-mgr)])
;;   -> void?
(define (load-all-content [mgr (current-content-mgr)])
  (queue-filter!
   (content-mgr-to-load mgr)
   (lambda (pending)
     (enqueue! (content-mgr-loaded mgr)
               (load-resource pending))
     #f)))

;;; Load a bitmap% into OpenGL a 2D texture ID
;; (bitmap->gl-texture [img : (is-a?/c bitmap%)]) -> integer?
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

;;; Pending texture (loads result into a box):
(struct pending-texture (data/box path)
  #:methods gen:pending-resource
  [(define/match (load-resource p-t)
     [((pending-texture bx path))
      (printf "Loading texture: ~v\n" path)
      (let* ([img (make-object bitmap%
                               path
                               'unknown/alpha
                               #f
                               #t)]
             [width (send img get-width)]
             [height (send img get-height)])
        ; install the texture into the box:
        (set-box! bx
                  (texdata (bitmap->gl-texture img)
                           width
                           height))
        ; return a resource that can be unloaded:
        (imgres bx))])])

;;; Queue a texture to be loaded by the content manager (by path).
;; (queue-texture [path : string?]
;;                [mgr : content-mgr? = (current-content-mgr)])
;;   -> texture?
(define (queue-texture path [mgr (current-content-mgr)])
  (let ([d/b (box #f)])
    (enqueue! (content-mgr-to-load mgr)
              (pending-texture d/b path))
    (texture d/b
             0.0 1.0
             0.0 1.0)))

;;; Queue a tileset to be loaded by the content manager.
;; (queue-tileset [path : string?]
;;                [tile-width tile-height : integer?]
;;                [x-spacing y-spacing : integer? = 0]
;;                [mgr : content-mgr? = (current-content-mgr)])
;;   -> texture?
(define (queue-tileset path
                       tile-w tile-h
                       [x-spacing 0] [y-spacing 0]
                       [mgr (current-content-mgr)])
  (let ([d/b (box #f)])
    (enqueue! (content-mgr-to-load mgr)
              (pending-texture d/b path))
    (tileset d/b
             tile-w tile-h
             x-spacing y-spacing)))
