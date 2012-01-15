/******************************************************************************
 * Ogre::SceneNode interface definition
 ******************************************************************************/

#pragma once

DLL CoiHandle create_child_scene_node(CoiHandle scene_manager_handle, const char* node_name);

DLL void scene_node_attach_entity(CoiHandle scene_node_handle, CoiHandle entity_handle);

DLL void scene_node_update(CoiHandle scene_node_handle, int update_children, int parent_has_changed);

DLL void scene_node_update_bounds(CoiHandle scene_node_handle);

DLL CoiHandle scene_node_get_attached_entity_int(CoiHandle scene_node_handle, int entity_index);

DLL CoiHandle scene_node_get_attached_entity(CoiHandle scene_node_handle, const char* entity_name);

DLL int scene_node_num_attached_objects(CoiHandle scene_node_handle);

DLL void scene_node_detach_entity_int(CoiHandle scene_node_handle, int entity_index);

DLL void scene_node_detach_entity(CoiHandle scene_node_handle, CoiHandle entity_handle);

DLL void scene_node_detach_entity_string(CoiHandle scene_node_handle, const char* entity_name);

DLL void scene_node_detach_all_objects(CoiHandle scene_node_handle);

DLL int scene_node_is_in_scenegraph(CoiHandle scene_node_handle);

DLL void scene_node_notify_root_node(CoiHandle scene_node_handle);

DLL void scene_node_show_bounding_box(CoiHandle scene_node_handle, int show_boundingbox);

DLL void scene_node_hide_bounding_box(CoiHandle scene_node_handle, int hide_boundingbox);

DLL int scene_node_get_show_bounding_box(CoiHandle scene_node_handle);

DLL CoiHandle scene_node_get_parent_scene_node(CoiHandle scene_node_handle);

DLL void scene_node_set_visible(CoiHandle scene_node_handle, int visible);

DLL void scene_node_set_visible_cascade(CoiHandle scene_node_handle, int visible, int cascade);

DLL void scene_node_flip_visibility(CoiHandle scene_node_handle);

DLL void scene_node_flip_visibility_cascade(CoiHandle scene_node_handle, int cascade);

DLL void scene_node_set_debug_display_enabled(CoiHandle scene_node_handle, int enabled);

DLL void scene_node_set_debug_display_enabled_cascade(CoiHandle scene_node_handle, int enabled, int cascade);

// FIXME: returns SceneManager, probably need a different function name
DLL CoiHandle scene_node_get_creator(CoiHandle scene_node_handle);

DLL void scene_node_set_direction(CoiHandle scene_node_handle, float x, float y, float z);

DLL void scene_node_set_orientation(CoiHandle scene_node_handle, float w, float x, float y, float z);

DLL void scene_node_set_position(CoiHandle scene_node_handle, float x, float y, float z);

DLL void scene_node_yaw(CoiHandle scene_node_handle, float radians);

DLL void scene_node_set_scale(CoiHandle scene_node_handle, float x, float y, float z);

DLL void scene_node_scale(CoiHandle scene_node_handle, float x, float y, float z);

DLL void scene_node_translate(CoiHandle scene_node_handle, float x, float y, float z);

DLL void scene_node_roll(CoiHandle scene_node_handle, float radians);

DLL void scene_node_pitch(CoiHandle scene_node_handle, float radians);
