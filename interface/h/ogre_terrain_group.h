/******************************************************************************
 * Ogre::TerrainGroup interface definition
 ******************************************************************************/

#pragma once

DLL CoiHandle create_terrain_group(CoiHandle scene_manager, int terrain_alignment, int terrain_size, float terrain_world_size);

DLL void terrain_group_set_filename_convention(CoiHandle terrain_group_handle, const char* prefix, const char* extension);

DLL void terrain_group_set_origin(CoiHandle terrain_group_handle, float x, float y, float z);

DLL void terrain_group_load_all_terrains(CoiHandle terrain_group_handle, int synchronous);

DLL void terrain_group_load_terrain(CoiHandle terrain_group_handle, long x, long y, int synchronous);

DLL void terrain_group_unload_terrain(CoiHandle terrain_group_handle, long x, long y);

DLL void terrain_group_remove_terrain(CoiHandle terrain_group_handle, long x, long y);

DLL void terrain_group_free_temporary_resources(CoiHandle terrain_group_handle);

DLL const char* terrain_group_generate_filename(CoiHandle terrain_group_handle, long x, long y);

DLL const char* terrain_group_get_resource_group(CoiHandle terrain_group_handle);

DLL void terrain_group_define_terrain(CoiHandle terrain_group_handle, long x, long y);
DLL void terrain_group_define_terrain_height(CoiHandle terrain_group_handle, long x, long y, float height);
DLL void terrain_group_define_terrain_image(CoiHandle terrain_group_handle, long x, long y, CoiHandle image_handle);
DLL void terrain_group_define_terrain_filename(CoiHandle terrain_group_handle, long x, long y, const char* filename);

DLL CoiHandle terrain_group_get_default_import_settings(CoiHandle terrain_group_handle);


// terrain group terrain iterator
DLL CoiHandle terrain_group_get_terrain_iterator(CoiHandle terrain_group_handle);
DLL int terrain_iterator_has_more_elements(CoiHandle terrain_iterator_handle);
DLL CoiHandle terrain_iterator_get_next(CoiHandle terrain_iterator_handle);


// terrain group import data struct
DLL void terrain_group_import_data_set_terrain_size(CoiHandle import_data_handle, int size);
DLL void terrain_group_import_data_set_world_size(CoiHandle import_data_handle, float size);
DLL void terrain_group_import_data_set_input_scale(CoiHandle import_data_handle, float scale);
DLL void terrain_group_import_data_set_min_batch_size(CoiHandle import_data_handle, int size);
DLL void terrain_group_import_data_set_max_batch_size(CoiHandle import_data_handle, int size);

DLL void terrain_group_import_data_resize_layers(CoiHandle import_data_handle, int size);
DLL void terrain_group_import_data_set_layer(CoiHandle import_data_handle, int layer_index, int world_size, const char* texture1, const char* texture2);


