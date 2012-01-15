/******************************************************************************
 * Ogre::Entity interface definition
 ******************************************************************************/

#pragma once

DLL CoiHandle create_entity(CoiHandle scene_manager_handle, const char* entity_name, const char* mesh_file);

DLL void entity_set_material_name(CoiHandle entity_handle, const char* name);

DLL void entity_set_material_name_and_group(CoiHandle entity_handle, const char* name, const char* group_name);
