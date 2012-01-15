/******************************************************************************
 * Ogre::RenderWindow interface definition
 ******************************************************************************/

#pragma once

DLL CoiHandle create_render_window(const char* name, const int width, const int height, const int full_screen);

DLL CoiHandle create_render_window_gl_context(const char* name, const int width, const int height, const int full_screen);

DLL CoiHandle create_render_window_hwnd(const char* name, const int width, const int height, const int full_screen, unsigned long hwnd);

DLL unsigned int render_window_get_hwnd(CoiHandle render_window_handle);

DLL void render_window_set_visible(CoiHandle render_window_handle, int visible);

DLL void render_window_update(CoiHandle render_window_handle, int swap_buffers);

DLL void render_window_resize(CoiHandle render_window_handle, unsigned int width, unsigned int height);

DLL void render_window_moved_or_resized(CoiHandle render_window_handle);

DLL int render_window_closed(CoiHandle render_window_handle);


