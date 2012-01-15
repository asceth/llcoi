/******************************************************************************
 * Ogre::ResourceGroupManager interface definition
 ******************************************************************************/

#pragma once

DLL void setup_resources(const char* resources_cfg);

DLL void add_resource_location(const char* location, const char* type, const char* group);

DLL void initialise_all_resource_groups();

DLL int resource_exists(const char* group, const char* name);
