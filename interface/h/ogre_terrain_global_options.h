/******************************************************************************
 * Ogre::TerrainGlobalOptions interface definition
 ******************************************************************************/

#pragma once

DLL CoiHandle create_terrain_global_options();

DLL CoiHandle terrain_global_options();

DLL void terrain_global_options_set_max_pixel_error(float pixerr);

DLL float terrain_global_options_get_skirt_size();

DLL void terrain_global_options_set_skirt_size(float skirt_size);

DLL void terrain_global_options_set_light_map_direction(float x, float y, float z);
DLL void terrain_global_options_set_light_map_direction_vector3(const coiVector3 vector);

DLL void terrain_global_options_set_composite_map_ambient_rgba(float r, float g, float b, float a);
DLL void terrain_global_options_set_composite_map_ambient_rgb(float r, float g, float b);
DLL void terrain_global_options_set_composite_map_ambient_colour(const coiColourValue colour);

DLL void terrain_global_options_set_composite_map_diffuse_rgba(float r, float g, float b, float a);
DLL void terrain_global_options_set_composite_map_diffuse_rgb(float r, float g, float b);
DLL void terrain_global_options_set_composite_map_diffuse_colour(const coiColourValue colour);

DLL void terrain_global_options_set_composite_map_distance(float c);

