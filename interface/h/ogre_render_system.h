/******************************************************************************
 * Ogre::RenderSystem interface definition
 ******************************************************************************/

#pragma once

DLL CoiHandle get_render_system();

DLL CoiHandle get_render_system_by_name(const char* render_system_name);

DLL void add_render_system(CoiHandle render_system);

DLL void set_render_system(CoiHandle render_system);

DLL void render_system_set_config_option(CoiHandle render_system_handle, const char* option, const char* value);

