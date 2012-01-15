/******************************************************************************
 * Ogre::TerrainGlobalOptions bindings
 ******************************************************************************/

#include "ogre_prerequisites.h"
#include "ogre_terrain_global_options.h"

#include <OgreRoot.h>
#include <Terrain/OgreTerrain.h>


// Ogre::TerrainGlobalOptions()
CoiHandle create_terrain_global_options()
{
  return new Ogre::TerrainGlobalOptions();
}

CoiHandle terrain_global_options()
{
  return Ogre::TerrainGlobalOptions::getSingletonPtr();
}

// void setMaxPixelError (Real pixerr)
void terrain_global_options_set_max_pixel_error(float pixerr)
{
  Ogre::TerrainGlobalOptions::getSingletonPtr()->setMaxPixelError(pixerr);
}

// Real getSkirtSize ()
float terrain_global_options_get_skirt_size()
{
  return Ogre::TerrainGlobalOptions::getSingletonPtr()->getSkirtSize();
}

// void setSkirtSize (Real skirtSz)
void terrain_global_options_set_skirt_size(float skirt_size)
{
  Ogre::TerrainGlobalOptions::getSingletonPtr()->setSkirtSize(skirt_size);
}

// void setLightMapDirection (const Vector3 &v)
void terrain_global_options_set_light_map_direction(float x, float y, float z)
{
  Ogre::TerrainGlobalOptions::getSingletonPtr()->setLightMapDirection(Ogre::Vector3(x, y, z));
}

void terrain_global_options_set_light_map_direction_vector3(const coiVector3 vector)
{
  Ogre::TerrainGlobalOptions::getSingletonPtr()->setLightMapDirection(Ogre::Vector3(vector.x, vector.y, vector.z));
}

// void setCompositeMapAmbient (const ColourValue &c)
void terrain_global_options_set_composite_map_ambient_rgba(float r, float g, float b, float a)
{
  Ogre::TerrainGlobalOptions::getSingletonPtr()->setCompositeMapAmbient(Ogre::ColourValue(r, g, b, a));
}

void terrain_global_options_set_composite_map_ambient_rgb(float r, float g, float b)
{
  Ogre::TerrainGlobalOptions::getSingletonPtr()->setCompositeMapAmbient(Ogre::ColourValue(r, g, b));
}

void terrain_global_options_set_composite_map_ambient_colour(const coiColourValue colour)
{
  Ogre::TerrainGlobalOptions::getSingletonPtr()->setCompositeMapAmbient(llcoi_colour_to_ogre_colour(colour));
}

// void setCompositeMapDiffuse (const ColourValue &c)
void terrain_global_options_set_composite_map_diffuse_rgba(float r, float g, float b, float a)
{
  Ogre::TerrainGlobalOptions::getSingletonPtr()->setCompositeMapDiffuse(Ogre::ColourValue(r, g, b, a));
}

void terrain_global_options_set_composite_map_diffuse_rgb(float r, float g, float b)
{
  Ogre::TerrainGlobalOptions::getSingletonPtr()->setCompositeMapDiffuse(Ogre::ColourValue(r, g, b));
}

void terrain_global_options_set_composite_map_diffuse_colour(const coiColourValue colour)
{
  Ogre::TerrainGlobalOptions::getSingletonPtr()->setCompositeMapDiffuse(llcoi_colour_to_ogre_colour(colour));
}

// void setCompositeMapDistance (Real c)
void terrain_global_options_set_composite_map_distance(float c)
{
  Ogre::TerrainGlobalOptions::getSingletonPtr()->setCompositeMapDistance(c);
}

// const Vector3 & 	getLightMapDirection ()
// const ColourValue & 	getCompositeMapAmbient ()
// const ColourValue & 	getCompositeMapDiffuse ()
// Real 	getCompositeMapDistance ()
// bool 	getCastsDynamicShadows ()
// void 	setCastsDynamicShadows (bool s)
// Real 	getMaxPixelError ()
// uint8 	getRenderQueueGroup (void)
// void 	setRenderQueueGroup (uint8 grp)
// uint32 	getVisibilityFlags (void)
// void 	setVisibilityFlags (uint32 flags)
// void 	setQueryFlags (uint32 flags)
// uint32 	getQueryFlags (void)
// void 	addQueryFlags (uint32 flags)
// void 	removeQueryFlags (uint32 flags)
// bool 	getUseRayBoxDistanceCalculation ()
// void 	setUseRayBoxDistanceCalculation (bool rb)
// TerrainMaterialGeneratorPtr 	getDefaultMaterialGenerator ()
// void 	setDefaultMaterialGenerator (TerrainMaterialGeneratorPtr gen)
// uint16 	getLayerBlendMapSize ()
// void 	setLayerBlendMapSize (uint16 sz)
// Real 	getDefaultLayerTextureWorldSize ()
// void 	setDefaultLayerTextureWorldSize (Real sz)
// uint16 	getDefaultGlobalColourMapSize ()
// void 	setDefaultGlobalColourMapSize (uint16 sz)
// uint16 	getLightMapSize ()
// void 	setLightMapSize (uint16 sz)
// uint16 	getCompositeMapSize ()
// void 	setCompositeMapSize (uint16 sz)
// void 	setDefaultResourceGroup (const String &grp)
// const String & 	getDefaultResourceGroup ()

