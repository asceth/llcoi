/******************************************************************************
 * Ogre::SceneManager interface definition
 ******************************************************************************/

#pragma once

// FIXME: do these exist?
DLL void scene_manager_log_name();
// /FIXME:


DLL CoiHandle create_scene_manager(const char* type_name, const char* instance_name);

DLL CoiHandle get_scene_manager();

DLL CoiHandle get_scene_manager_by_name(const char* scene_manager_instance_name);

DLL void scene_manager_set_ambient_light_rgba(CoiHandle scene_manager_handle, const float r, const float g, const float b, const float a);

DLL void scene_manager_set_ambient_light_rgb(CoiHandle scene_manager_handle, const float r, const float g, const float b);

DLL coiColourValue scene_manager_get_ambient_light(CoiHandle scene_manager_handle);

DLL void scene_manager_set_shadow_technique(CoiHandle scene_manager_handle, const int technique);

DLL void scene_manager_get_shadow_technique(CoiHandle scene_manager_handle, const int technique);


