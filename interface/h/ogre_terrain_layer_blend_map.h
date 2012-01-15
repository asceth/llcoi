/******************************************************************************
 * Ogre::TerrainLayerBlendMap interface definition
 ******************************************************************************/

#pragma once

// terrain layer blend maps
DLL float* terrain_layer_blend_map_get_blend_pointer(CoiHandle terrain_layer_blend_map_handle);
DLL void terrain_layer_blend_map_convert_image_to_terrain_space(CoiHandle terrain_layer_blend_map_handle, int x, int y, float* out_x, float* out_y);
DLL void terrain_layer_blend_map_dirty(CoiHandle terrain_layer_blend_map_handle);
DLL void terrain_layer_blend_map_update(CoiHandle terrain_layer_blend_map_handle);


