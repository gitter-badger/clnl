(in-package #:clnl-interface)

(defvar *patch-size* 13d0)
(defvar *world-dims* '(:xmin -5 :xmax 5 :ymin -5 :ymax 5))

(defvar *turtle-list* nil)

; It may be useful to keep windows around
(defvar *glut-window-opened* nil)

(defvar *colors*
 '((140 140 140) ; gray       (5)
   (215 48 39) ; red       (15)
   (241 105 19) ; orange    (25)
   (156 109 70) ; brown     (35)
   (237 237 47) ; yellow    (45)
   (87 176 58) ; green     (55)
   (42 209 57) ; lime      (65)
   (27 158 119) ; turquoise (75)
   (82 196 196) ; cyan      (85)
   (43 140 190) ; sky       (95)
   (50 92 168) ; blue     (105)
   (123 78 163) ; violet   (115)
   (166 25 105) ; magenta  (125)
   (224 126 149) ; pink     (135)
   (0 0 0) ; black
   (255 255 255))) ; white

(defun nl-color->rgb (color)
 (let*
  ((step (+ (/ (- (mod (floor (* color 10)) 100) 50) 50.48) 0.012)))
  (mapcar
   (lambda (x) (/ (+ x (floor (* (if (< step 0d0) x (- 255 x)) step))) 255))
   (nth (floor color 10) *colors*))))

(defun render-scene ()
 (gl:clear :color-buffer-bit :depth-buffer-bit)
 (gl:matrix-mode :projection)
 (gl:load-identity)
 (gl:ortho -71 71 -71 71 1 5000)
 (gl:matrix-mode :modelview)
 (gl:load-identity)
 (mapcar
  (lambda (turtle)
   (let
    ((color (nl-color->rgb (getf turtle :color))))
    (gl:color (car color) (cadr color) (caddr color)))
   (gl:with-pushed-matrix
    (gl:translate (* (getf turtle :xcor) *patch-size*) (* (getf turtle :ycor) *patch-size*) 0)
    (gl:rotate (getf turtle :heading) 0 0 -1)
    (gl:call-list *turtle-list*)))
  (clnl-nvm:current-state))
 (gl:flush))

(defun display ()
 (render-scene)
 (cl-glut:swap-buffers))

(defun idle ()
 (cl-glut:post-redisplay))

(defun close-func ()
 (sb-ext:exit))

(defun reshape (width height)
 (when (and (/= 0 width) (/= 0 height))
  (gl:viewport 0 0 width height)))

(cffi:defcallback display :void () (display))
(cffi:defcallback idle :void () (idle))
(cffi:defcallback close-func :void () (close-func))
(cffi:defcallback reshape :void ((width :int) (height :int)) (reshape width height))

(defun set-turtle-list ()
 (setf *turtle-list* (gl:gen-lists 1))
 (gl:with-new-list (*turtle-list* :compile)
  (gl:rotate 180 0 0 -1)
  (gl:scale (* (/ 1d0 300d0) 13) (* (/ 1d0 300d0) 13) 1)
  (gl:translate -150 -150 -4.0)
  (gl:begin :polygon)
  (gl:vertex 150 5 0)
  (gl:vertex 40 250 0)
  (gl:vertex 150 205 0)
  (gl:vertex 260 250 0)
  (gl:end)))

(defun run ()
 "RUN => RESULT

ARGUMENTS AND VALUES:

  RESULT: undefined, should never get here

DESCRIPTION:

  RUN runs the view in an external window.

  This should be run inside another thread as it starts the glut main-loop.
  Closing this window will then cause the entire program to terminate."
 ; I do this because I don't know who or what in the many layers
 ; is causing the floating point errors, but I definitely don't
 ; want to investigate until simply ignoring them becomes a problem.
 (sb-int:with-float-traps-masked (:invalid)
  (cl-glut:init)
  (gl:clear-color 0 0 0 1)
  (cl-glut:init-window-size
   (floor (* *patch-size* (1+ (- (getf *world-dims* :xmax) (getf *world-dims* :xmin)))))
   (floor (* *patch-size* (1+ (- (getf *world-dims* :ymax) (getf *world-dims* :ymin))))))
  (setf *glut-window-opened* t)
  (cl-glut:create-window "CLNL Test Window")
  (cl-glut:init-display-mode :double :rgba)
  (cl-glut:display-func (cffi:get-callback 'display))
  (glut:reshape-func (cffi:callback reshape))
  (cl-glut:idle-func (cffi:get-callback 'idle))
  (cl-glut:close-func (cffi:get-callback 'close-func))
  (set-turtle-list)
  (cl-glut:main-loop)))

(defun export-view ()
 "EXPORT-VIEW => IMAGE-DATA

ARGUMENTS AND VALUES:

  IMAGE-DATA: A vector, pixel data as returned by opengls readPixels

DESCRIPTION:

  EXPORT-VIEW returns the current view in raw data of RGBA pixels.

  Each pixel is made up of 4 bytes of data, which an be walked over.  The number
  of pixels is the current width x height.  Converting to some other image format
  is a matter of pulling that information out and putting it into whatever format
  you like.

  This requires opengl to run, but can be used with xvfb in a headless mode."
 (sb-int:with-float-traps-masked (:invalid)
  (when (not *glut-window-opened*)
   (cl-glut:init)
   (gl:clear-color 0 0 0 1)
   (cl-glut:init-window-size 1 1)
   (cl-glut:create-window "CLNL Test Window")
   (set-turtle-list)
   (setf *glut-window-opened* t))
  (let
   ((fbo (first (gl:gen-framebuffers 1)))
    (render-buf (first (gl:gen-renderbuffers 1)))
   ;(width (floor (* *patch-size* (1+ (- (getf *world-dims* :xmax) (getf *world-dims* :xmin))))))
   ;(height (floor (* *patch-size* (1+ (- (getf *world-dims* :ymax) (getf *world-dims* :ymin))))))
    (width 143)  ; Hard coded for now, yay v1 (if you see this comment in a year, please cry for me)
    (height 143))
   (gl:bind-framebuffer :framebuffer fbo)
   (gl:bind-renderbuffer :renderbuffer render-buf)
   (gl:renderbuffer-storage :renderbuffer :rgba8 width height)
   (gl:framebuffer-renderbuffer :draw-framebuffer :color-attachment0 :renderbuffer render-buf)
   (gl:viewport 0 0 width height)
   (render-scene)
   (gl:read-pixels 0 0 width height :rgba :unsigned-byte))))
