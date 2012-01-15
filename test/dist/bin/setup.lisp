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

;(ogre:render-loop-once)


(defun pause ()
  (setf *continue-loop* 0))

(defun resume ()
  (setf *continue-loop* 1))


