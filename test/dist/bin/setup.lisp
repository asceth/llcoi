(defparameter *cfgpath* "/home/jlong/development/games/lib/llcoi/test/dist/bin/")
(push *cfgpath* asdf:*central-registry*)
(require 'ogre)

(defparameter *windowhandle* nil)
(defparameter *camerahandle* nil)
(defparameter *viewporthandle* nil)
(defparameter *windowhwnd* nil)
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

(ogre:set-ambient-light-rgb 0.5 0.5 0.5)

(setq *lighthandle* (ogre:create-light "mainLight"))

(ogre:light-set-position *lighthandle* 20.0 80.0 50.0)

(ogre:log-message "Hello World from Lisp")

(setq *windowhwnd* (ogre:render-window-get-hwnd *windowhandle*))

(ogre:create-input-system *windowhwnd*);

(setq *keyboard* (ogre:create-keyboard-object 0))

(if *use-mouse*
    (setq *mouse* (ogre:create-mouse-object 0)))

(defun handle-input (frametime)
  (ogre:keyboard-capture *keyboard*)
  (if *use-mouse*
      (ogre:mouse-capture *mouse*))
  (if (= (ogre:keyboard-is-key-down *keyboard* :+KC-ESCAPE+) 1) 0 1))

(defun framequeued (evt_time frame_time event_type)
  (with-simple-restart
      (skip-frame-queued-body "Skip frame queued body")
    (setq *timer* (+ *timer* frame_time))
    (let ((x (* (cos *timer*) 100.0))
          (y (* (sin *timer*) 100.0))
          (z 50.0)
          )
      (ogre:camera-set-position *camerahandle* x y z)
      (ogre::camera-lookat *camerahandle* 0.0 0.0 0.0))
    (handle-input frame_time)))


(cffi:defcallback framequeued :int
    ((evt_time :float)
     (frame_time :float)
     (event_type :int))
  (funcall #'framequeued evt_time frame_time event_type))

(ogre:add-frame-listener (cffi:callback framequeued) 2)

;;(ogre:remove-frame-listener (cffi:callback framequeued))

;; (defun framequeued (evt_time frame_time event_type)
;;   (ogre:log-message "6"))

;; (cffi:defcallback framequeued_callback :int
;;   ((evt_time :float)
;;    (frame_time :float)
;;    (event_type :int))
;;   (funcall #'framequeued evt_time frame_time event_type))



;; (cffi:callback framequeued_callback)
;; (ogre:add-frame-listener (cffi:callback framequeued_callback) 2)

(ogre:render-loop-once)

(defun game-loop ()
  (loop
     until (eq *continue-loop* 0)
     do
       (with-simple-restart
           (skip-ogre-loop "Skip ogre loop body")
         (setf *continue-loop* (ogre:render-loop-once)))
       (with-simple-restart
      (skip-swank-request "Skip swank evaluation")
         (let ((connection
                (or swank::*emacs-connection* (swank::default-connection))))
           (swank::handle-requests connection t))))
)

(defun pause ()
  (setf *continue-loop* 0))

(defun resume ()
  (setf *continue-loop* 1))


