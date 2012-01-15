/******************************************************************************
 * Ogre::TerrainGroup bindings
 ******************************************************************************/

#include "ogre_prerequisites.h"
#include "ogre_terrain_group.h"

#include <OgreRoot.h>
#include <Terrain/OgreTerrainGroup.h>
#include <Terrain/OgreTerrain.h>


// Ogre::TerrainGroup(SceneManager scene_manager, )
CoiHandle create_terrain_group(CoiHandle scene_manager, int terrain_alignment, int terrain_size, float terrain_world_size)
{
  return new Ogre::TerrainGroup(reinterpret_cast<Ogre::SceneManager*>(scene_manager), (Ogre::Terrain::Alignment)terrain_alignment, terrain_size, terrain_world_size);
}

// void setFilenameConvention (const String &prefix, const String &extension)
void terrain_group_set_filename_convention(CoiHandle terrain_group_handle, const char* prefix, const char* extension)
{
  Ogre::TerrainGroup* terrain_group = reinterpret_cast<Ogre::TerrainGroup*>(terrain_group_handle);
  terrain_group->setFilenameConvention(prefix, extension);
}

// void setOrigin(Vector3 &pos)
void terrain_group_set_origin(CoiHandle terrain_group_handle, float x, float y, float z)
{
  Ogre::TerrainGroup* terrain_group = reinterpret_cast<Ogre::TerrainGroup*>(terrain_group_handle);
  terrain_group->setOrigin(Ogre::Vector3(x, y , z));
}

// void loadAllTerrains(bool synchronous=false)
void terrain_group_load_all_terrains(CoiHandle terrain_group_handle, int synchronous)
{
  Ogre::TerrainGroup* terrain_group = reinterpret_cast<Ogre::TerrainGroup*>(terrain_group_handle);
  terrain_group->loadAllTerrains(synchronous);
}

// void loadTerrain(long x, long y, bool synchronous)
void terrain_group_load_terrain(CoiHandle terrain_group_handle, long x, long y, int synchronous)
{
  Ogre::TerrainGroup* terrain_group = reinterpret_cast<Ogre::TerrainGroup*>(terrain_group_handle);
  terrain_group->loadTerrain(x, y, synchronous);
}

// void unloadTerrain(long x, long y)
void terrain_group_unload_terrain(CoiHandle terrain_group_handle, long x, long y)
{
  Ogre::TerrainGroup* terrain_group = reinterpret_cast<Ogre::TerrainGroup*>(terrain_group_handle);
  terrain_group->loadTerrain(x, y);
}

// void removeTerrain(long x, long y)
void terrain_group_remove_terrain(CoiHandle terrain_group_handle, long x, long y)
{
  Ogre::TerrainGroup* terrain_group = reinterpret_cast<Ogre::TerrainGroup*>(terrain_group_handle);
  terrain_group->removeTerrain(x, y);
}

// void freeTemporaryResources()
void terrain_group_free_temporary_resources(CoiHandle terrain_group_handle)
{
  Ogre::TerrainGroup* terrain_group = reinterpret_cast<Ogre::TerrainGroup*>(terrain_group_handle);
  terrain_group->freeTemporaryResources();
}

const char* terrain_group_generate_filename(CoiHandle terrain_group_handle, long x, long y)
{
  Ogre::TerrainGroup* terrain_group = reinterpret_cast<Ogre::TerrainGroup*>(terrain_group_handle);
  return terrain_group->generateFilename(x, y).c_str();
}

const char* terrain_group_get_resource_group(CoiHandle terrain_group_handle)
{
  Ogre::TerrainGroup* terrain_group = reinterpret_cast<Ogre::TerrainGroup*>(terrain_group_handle);
  return terrain_group->getResourceGroup().c_str();
}


void terrain_group_define_terrain(CoiHandle terrain_group_handle, long x, long y)
{
  Ogre::TerrainGroup* terrain_group = reinterpret_cast<Ogre::TerrainGroup*>(terrain_group_handle);
  terrain_group->defineTerrain(x, y);
}

void terrain_group_define_terrain_height(CoiHandle terrain_group_handle, long x, long y, float height)
{
  Ogre::TerrainGroup* terrain_group = reinterpret_cast<Ogre::TerrainGroup*>(terrain_group_handle);
  terrain_group->defineTerrain(x, y, height);
}

void terrain_group_define_terrain_image(CoiHandle terrain_group_handle, long x, long y, CoiHandle image_handle)
{
  Ogre::TerrainGroup* terrain_group = reinterpret_cast<Ogre::TerrainGroup*>(terrain_group_handle);
  Ogre::Image* image = reinterpret_cast<Ogre::Image*>(image_handle);
  terrain_group->defineTerrain(x, y, image);
}

void terrain_group_define_terrain_filename(CoiHandle terrain_group_handle, long x, long y, const char* filename)
{
  Ogre::TerrainGroup* terrain_group = reinterpret_cast<Ogre::TerrainGroup*>(terrain_group_handle);
  terrain_group->defineTerrain(x, y, filename);
}


CoiHandle terrain_group_get_default_import_settings(CoiHandle terrain_group_handle)
{
  Ogre::TerrainGroup* terrain_group = reinterpret_cast<Ogre::TerrainGroup*>(terrain_group_handle);
  return &terrain_group->getDefaultImportSettings();
}


// terrain iterator
CoiHandle terrain_group_get_terrain_iterator(CoiHandle terrain_group_handle)
{
  Ogre::TerrainGroup* terrain_group = reinterpret_cast<Ogre::TerrainGroup*>(terrain_group_handle);
  return &terrain_group->getTerrainIterator();
}

int terrain_iterator_has_more_elements(CoiHandle terrain_iterator_handle)
{
  Ogre::TerrainGroup::TerrainIterator* terrain_iterator = reinterpret_cast<Ogre::TerrainGroup::TerrainIterator*>(terrain_iterator_handle);
  return (int)terrain_iterator->hasMoreElements();
}

CoiHandle terrain_iterator_get_next(CoiHandle terrain_iterator_handle)
{
  Ogre::TerrainGroup::TerrainIterator* terrain_iterator = reinterpret_cast<Ogre::TerrainGroup::TerrainIterator*>(terrain_iterator_handle);
  return terrain_iterator->getNext()->instance;
}


// import data
void terrain_group_import_data_set_terrain_size(CoiHandle import_data_handle, int size)
{
  Ogre::Terrain::ImportData* import_data = reinterpret_cast<Ogre::Terrain::ImportData*>(import_data_handle);
  import_data->terrainSize = size;
}

void terrain_group_import_data_set_world_size(CoiHandle import_data_handle, float size)
{
  Ogre::Terrain::ImportData* import_data = reinterpret_cast<Ogre::Terrain::ImportData*>(import_data_handle);
  import_data->worldSize = size;
}

void terrain_group_import_data_set_input_scale(CoiHandle import_data_handle, float scale)
{
  Ogre::Terrain::ImportData* import_data = reinterpret_cast<Ogre::Terrain::ImportData*>(import_data_handle);
  import_data->inputScale = scale;
}

void terrain_group_import_data_set_min_batch_size(CoiHandle import_data_handle, int size)
{
  Ogre::Terrain::ImportData* import_data = reinterpret_cast<Ogre::Terrain::ImportData*>(import_data_handle);
  import_data->minBatchSize = size;
}

void terrain_group_import_data_set_max_batch_size(CoiHandle import_data_handle, int size)
{
  Ogre::Terrain::ImportData* import_data = reinterpret_cast<Ogre::Terrain::ImportData*>(import_data_handle);
  import_data->maxBatchSize = size;
}

void terrain_group_import_data_resize_layers(CoiHandle import_data_handle, int size)
{
  Ogre::Terrain::ImportData* import_data = reinterpret_cast<Ogre::Terrain::ImportData*>(import_data_handle);
  import_data->layerList.resize(size);
}

void terrain_group_import_data_set_layer(CoiHandle import_data_handle, int layer_index, int world_size, const char* texture1, const char* texture2)
{
  Ogre::Terrain::ImportData* import_data = reinterpret_cast<Ogre::Terrain::ImportData*>(import_data_handle);
  import_data->layerList[layer_index].worldSize = world_size;
  import_data->layerList[layer_index].textureNames.push_back(texture1);
  import_data->layerList[layer_index].textureNames.push_back(texture2);
}
