(load "setup.lisp")

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
          (y 600.0)
          (z 400.0)
          )
      (ogre:camera-set-position *camerahandle* x y z)
      (ogre::camera-lookat *camerahandle* 0.0 0.0 0.0))
    (handle-input frame_time)))


(cffi:defcallback framequeued-callback :int
    ((evt_time :float)
     (frame_time :float)
     (event_type :int))
  (funcall #'framequeued evt_time frame_time event_type))

(ogre:add-frame-listener (cffi:callback framequeued-callback) 2)

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

(game-loop)

(defparameter *plane* nil)
(defparameter *plane-entity* nil)
(defparameter *plane-node* nil)

(setq *plane* (ogre:create-plane-from-normal 0 1 0 0))
(ogre:mesh-manager-create-plane "ground" "General" *plane* 1500 1500 20 20 1 1 5 5 0 0 1)
(setq *plane-entity* (ogre:create-entity "Plane" "ground"))
(setq *plane-node* (ogre:create-child-scenenode "ground-node"))
(ogre:attach-entity-to-scenenode *plane-entity* *plane-node*)
(ogre:entity-set-material-name *plane-entity* "Ogre/Skin" "General")

(defparameter *entityhandle* nil)
(defparameter *nodehandle* nil)
(setq *entityhandle* (ogre:create-entity "OgreHead" "ogrehead.mesh"))

(setq *nodehandle* (ogre:create-child-scenenode "headnode"))

(ogre:attach-entity-to-scenenode *entityhandle* *nodehandle*)
(ogre:scenenode-set-position *nodehandle* 100.0 200.0 0.0)
