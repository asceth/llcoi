/******************************************************************************
 * Ogre::Terrain bindings
 ******************************************************************************/

#include "ogre_prerequisites.h"
#include "ogre_terrain.h"

#include <OgreRoot.h>
#include <Terrain/OgreTerrain.h>
#include <Terrain/OgreTerrainLayerBlendMap.h>


int terrain_get_layer_blend_map_size(CoiHandle terrain_handle)
{
  Ogre::Terrain* terrain = reinterpret_cast<Ogre::Terrain*>(terrain_handle);
  return terrain->getLayerBlendMapSize();
}

CoiHandle terrain_get_layer_blend_map(CoiHandle terrain_handle, int layer_index)
{
  Ogre::Terrain* terrain = reinterpret_cast<Ogre::Terrain*>(terrain_handle);
  return terrain->getLayerBlendMap(layer_index);
}

float terrain_get_height_at_terrain_position(CoiHandle terrain_handle, float x, float y)
{
  Ogre::Terrain* terrain = reinterpret_cast<Ogre::Terrain*>(terrain_handle);
  return terrain->getHeightAtTerrainPosition(x, y);
}

