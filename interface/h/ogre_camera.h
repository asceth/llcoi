/******************************************************************************
 * Ogre::Camera interface definition
 ******************************************************************************/

#pragma once

DLL CoiHandle create_camera(CoiHandle scene_manager_handle, const char* camera_name);

DLL CoiHandle get_camera(CoiHandle scene_manager_handle, const char* camera_name);

DLL void camera_set_near_clip_distance(CoiHandle camera_handle, float d);

DLL void camera_set_far_clip_distance(CoiHandle camera_handle, float d);

DLL void camera_set_aspect_ratio(CoiHandle camera_handle, float w, float h);

DLL void camera_set_auto_aspect_ratio(CoiHandle camera_handle, int on);

DLL void camera_set_fovy(CoiHandle camera_handle, float angle);

DLL void camera_set_frustum_offset(CoiHandle camera_handle, const int offset_x, const int offset_y);

DLL void camera_set_focal_length(CoiHandle camera_handle, float fl);

DLL void camera_set_position(CoiHandle camera_handle, const float x, const float y, const float z);

DLL void camera_lookat(CoiHandle camera_handle, const float x, const float y, const float z);
