%module llcoi
%feature("export");
%feature("intern_function","1");

%insert("lisphead") %{
(in-package :ogre)

(define-foreign-library libllcoi
    (:unix (:or "libllcoi.so.3" "libllcoi.so"))
    (t (:default "libllcoi")))

(defun load-foreign-libraries ()
  (use-foreign-library libllcoi)
  (format t "~&[ogre] foreign library libllcoi loaded~%"))

(load-foreign-libraries)

(in-package :ogre)
%}
%insert("swiglisp") %{
#+sbcl(sb-ext::set-floating-point-modes :traps nil)
%}

%include "../h/ogre_interface_prerequisites.h"
%include "../h/ogre_structs.h"
%include "../h/ogre_enums.h"
%include "../h/ogre_camera.h"
%include "../h/ogre_entity.h"
%include "../h/ogre_frame_listener.h"
%include "../h/ogre_image.h"
%include "../h/ogre_light.h"
%include "../h/ogre_manual_object.h"
%include "../h/ogre_math.h"
%include "../h/ogre_mesh_manager.h"
%include "../h/ogre_movable_object.h"
%include "../h/ogre_plane.h"
%include "../h/ogre_render_system.h"
%include "../h/ogre_render_window.h"
%include "../h/ogre_resource_group_manager.h"
%include "../h/ogre_root.h"
%include "../h/ogre_scene_manager.h"
%include "../h/ogre_scene_node.h"
%include "../h/ogre_terrain.h"
%include "../h/ogre_terrain_global_options.h"
%include "../h/ogre_terrain_group.h"
%include "../h/ogre_terrain_layer_blend_map.h"
%include "../h/ogre_viewport.h"
%include "../h/ogre_window_event_listener.h"
%include "../h/ois_interface.h"




