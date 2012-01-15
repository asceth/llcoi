/******************************************************************************
 * Ogre::TerrainLayerBlendMap bindings
 ******************************************************************************/

#include "ogre_prerequisites.h"
#include "ogre_terrain_layer_blend_map.h"

#include <OgreRoot.h>
#include <Terrain/OgreTerrainGroup.h>
#include <Terrain/OgreTerrain.h>
#include <Terrain/OgreTerrainLayerBlendMap.h>

float* terrain_layer_blend_map_get_blend_pointer(CoiHandle terrain_layer_blend_map_handle)
{
  Ogre::TerrainLayerBlendMap* terrain_layer_blend_map = reinterpret_cast<Ogre::TerrainLayerBlendMap*>(terrain_layer_blend_map_handle);
  return terrain_layer_blend_map->getBlendPointer();
}

void terrain_layer_blend_map_convert_image_to_terrain_space(CoiHandle terrain_layer_blend_map_handle, int x, int y, float* out_x, float* out_y)
{
  Ogre::TerrainLayerBlendMap* terrain_layer_blend_map = reinterpret_cast<Ogre::TerrainLayerBlendMap*>(terrain_layer_blend_map_handle);
  terrain_layer_blend_map->convertImageToTerrainSpace(x, y, out_x, out_y);
}

void terrain_layer_blend_map_dirty(CoiHandle terrain_layer_blend_map_handle)
{
  Ogre::TerrainLayerBlendMap* terrain_layer_blend_map = reinterpret_cast<Ogre::TerrainLayerBlendMap*>(terrain_layer_blend_map_handle);
  terrain_layer_blend_map->dirty();
}

void terrain_layer_blend_map_update(CoiHandle terrain_layer_blend_map_handle)
{
  Ogre::TerrainLayerBlendMap* terrain_layer_blend_map = reinterpret_cast<Ogre::TerrainLayerBlendMap*>(terrain_layer_blend_map_handle);
  terrain_layer_blend_map->update();
}

