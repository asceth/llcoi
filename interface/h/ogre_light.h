/******************************************************************************
 * Ogre::Light interface definition
 ******************************************************************************/

#pragma once

DLL CoiHandle create_light(CoiHandle scene_manager_handle, const char* light_name);

DLL void light_set_position(CoiHandle light_handle, const float x, const float y, const float z);

DLL void light_set_diffuse_colour(CoiHandle light_handle, const float r, const float g, const float b);

DLL coiColourValue light_get_diffuse_colour(CoiHandle light_handle);

DLL void light_set_specular_colour(CoiHandle light_handle, const float r, const float g, const float b);

DLL void light_set_direction(CoiHandle light_handle, const float x, const float y, const float z);

DLL void light_set_spotlight_range(CoiHandle light_handle, const float inner_angle, const float outer_angle, const float falloff);

DLL void light_set_power_scale(CoiHandle light_handle, const float power);

DLL const float light_get_power_scale(CoiHandle light_handle);

DLL void light_set_attenuation(CoiHandle light_handle, const float range, const float constant, const float linear, const float quadratic);

DLL coiVector3 light_get_derived_direction(CoiHandle light_handle);
