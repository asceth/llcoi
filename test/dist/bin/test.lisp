(load "setup.lisp")

(game-loop)

(defparameter *plane* nil)
(defparameter *plane-entity* nil)
(defparameter *plane-node* nil)

(setq *plane* (ogre:create-plane-from-normal 0 1 0 0))
(ogre:mesh-manager-create-plane "ground" "General" *plane* 1500 1500 20 20 1 1 5 5 0 0 1)
(setq *plane-entity* (ogre:create-entity "Plane" "ground"))
(setq *plane-node* (ogre:create-child-scenenode "ground-node"))
(ogre:attach-entity-to-scenenode *plane-entity* *plane-node*)


(defparameter *entityhandle* nil)
(defparameter *nodehandle* nil)
(setq *entityhandle* (ogre:create-entity "OgreHead" "ogrehead.mesh"))

(setq *nodehandle* (ogre:create-child-scenenode "headnode"))

(ogre:attach-entity-to-scenenode *entityhandle* *nodehandle*)
