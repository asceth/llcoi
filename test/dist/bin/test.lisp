(defparameter *cfgpath* "/home/jlong/development/games/lib/llcoi/test/dist/bin/")
(push *cfgpath* asdf:*central-registry*)
(require 'ogre)

(defparameter *windowhandle* nil)
(defparameter *camerahandle* nil)
(defparameter *viewporthandle* nil)
(defparameter *windowhwnd* nil)
(defparameter *entityhandle* nil)
(defparameter *nodehandle* nil)
(defparameter *lighthandle* nil)
(defparameter *mouse* nil)
(defparameter *keyboard* nil)
(defparameter *timer* 0)
(defparameter *use-mouse* T)
(defparameter *continue-loop* 1)


(ogre:create-root (concatenate 'string *cfgpath* "plugins.cfg")
                  (concatenate 'string *cfgpath* "ogre.cfg")
                  (concatenate 'string *cfgpath* "ogre.log"))

(ogre:restore-config)

(ogre:setup-resources "resources.cfg")

(setq *windowhandle* (ogre:root-initialise 1 "therenderwindow"))

(ogre:set-default-num-mipmaps 5)

(ogre:initialise-all-resourcegroups)

(ogre:create-scene-manager "OctreeSceneManager" "scenemanager")

(setq *camerahandle* (ogre:create-camera "myCam"))

(ogre:camera-set-position *camerahandle* 0.0 0.0 500.0)

(ogre:camera-lookat *camerahandle* 0.0 0.0 -300.0)

(ogre:camera-set-near-clip-distance *camerahandle* 5.0)

(setq *viewporthandle* (ogre:add-viewport *camerahandle*))

(ogre:viewport-set-background-colour *viewporthandle* 0.0 0.0 0.0)

(ogre:camera-set-aspect-ratio *camerahandle* 800.0 600.0)

(setq *entityhandle* (ogre:create-entity "OgreHead" "ogrehead.mesh"))

(setq *nodehandle* (ogre:create-child-scenenode "headnode"))

(ogre:attach-entity-to-scenenode *entityhandle* *nodehandle*)

(ogre:set-ambient-light-rgb 0.5 0.5 0.5)

(setq *lighthandle* (ogre:create-light "mainLight"))

(ogre:light-set-position *lighthandle* 20.0 80.0 50.0)

(ogre:log-message "Hello World from Lisp")

(setq *windowhwnd* (ogre:render-window-get-hwnd *windowhandle*))

;(ogre:create-input-system *windowhwnd*);

;(setq *keyboard* (ogre:create-keyboard-object 0))

;(if *use-mouse*
;    (setq *mouse* (ogre:create-mouse-object 0)))

(defun handle-input (frametime)
;    (ogre:keyboard-capture *keyboard*)
;    (if *use-mouse*
;        (ogre:mouse-capture *mouse*))
;    (if (= (ogre:keyboard-is-key-down *keyboard* :+KC-ESCAPE+) 1) 0 1))
  (if (= 1 1) 1 0)
)


(defparameter *framequeued-callback* nil)
(setf *framequeued-callback* (cffi:defcallback framequeued :int
                                 ((evt_time :float)
                                  (frame_time :float)
                                  (event_type :int))
                               (setq *timer* (+ *timer* frame_time))
                               (let ((x (* (cos *timer*) 100.0))
                                     (y 50.0)
                                     (z (* (sin *timer*) 100.0)))
                                 (ogre:camera-set-position *camerahandle* x y z)
                                 (ogre::camera-lookat *camerahandle* 0.0 0.0 0.0))
                               (handle-input frame_time)
                               ))

(cffi:callback framequeued)
(ogre:add-frame-listener (cffi:callback framequeued) 2)


;; (defun framequeued (evt_time frame_time event_type)
;;   (ogre:log-message "6"))

;; (cffi:defcallback framequeued_callback :int
;;   ((evt_time :float)
;;    (frame_time :float)
;;    (event_type :int))
;;   (funcall #'framequeued evt_time frame_time event_type))



;; (cffi:callback framequeued_callback)
;; (ogre:add-frame-listener (cffi:callback framequeued_callback) 2)
;; (ogre:remove-frame-listener (cffi:callback framequeued_callback))

(ogre:render-loop)

(defun game-loop ()
  (with-simple-restart
      (skip-ogre-loop "Skip ogre loop body")
    (let ((x (* cos (+ *timer* 1) 100.0))
          (y 50.0)
          (z (* (sin (+ *timer* 1) 100.0))))
      (setf *continue-loop* (ogre:render-loop-once))))
  (with-simple-restart
      (skip-swank-request "Skip swank evaluation")
    (let ((connection
           (or swank::*emacs-connection* (swank::default-connection))))
      (swank::handle-requests connection t)))
  (cond
    ((eq *continue-loop* 1)
     (game-loop))))


(ogre:release-engine)



(cffi:defcfun "qsort" :void
    (base :pointer)
    (nmemb :int)
    (size :int)
    (fun-compar :pointer))


(defun store (par)
  (format t "I'm comparing ~a~%" par))


(cffi:defcallback < :int ((a :pointer) (b :pointer))
    (let ((x (cffi:mem-ref a :int))
          (y (cffi:mem-ref b :int)))
      (store (list x y))
      (cond ((> x y) 1)
            ((< x y) -1)
            (t 0))))

(cffi:with-foreign-object (array :int 10)
  ;; Initialize array.
  (loop for i from 0 and n in '(7 2 10 4 3 5 1 6 9 8)
     do (setf (cffi:mem-aref array :int i) n))
  ;; Sort it.
  (qsort array 10 (cffi:foreign-type-size :int) (cffi:callback <))
  ;; Return it as a list.
  (loop for i from 0 below 10
     collect (cffi:mem-aref array :int i)))
