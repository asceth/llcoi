/******************************************************************************
 * Ogre::Terrain interface definition
 ******************************************************************************/

#pragma once

DLL int terrain_get_layer_blend_map_size(CoiHandle terrain_handle);
DLL CoiHandle terrain_get_layer_blend_map(CoiHandle terrain_handle, int layer_index);
DLL float terrain_get_height_at_terrain_position(CoiHandle terrain_handle, float x, float y);


