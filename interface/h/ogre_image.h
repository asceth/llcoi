/******************************************************************************
 * Ogre::Image interface definition
 ******************************************************************************/

#pragma once

DLL CoiHandle create_image();
DLL void image_load(CoiHandle image_handle, const char* filename);
DLL void image_load_group(CoiHandle image_handle, const char* filename, const char* group);
DLL void image_flip_around_x(CoiHandle image_handle);
DLL void image_flip_around_y(CoiHandle image_handle);
DLL void image_delete(CoiHandle image_handle);


