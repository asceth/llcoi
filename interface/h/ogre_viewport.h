/******************************************************************************
 * Ogre::Viewport interface definition
 ******************************************************************************/

#pragma once

DLL CoiHandle add_viewport(CoiHandle camera_handle);

DLL void viewport_set_background_colour(CoiHandle viewport_handle, float r, float g, float b);

DLL float viewport_get_width(CoiHandle viewport_handle);

DLL float viewport_get_height(CoiHandle viewport_handle);
