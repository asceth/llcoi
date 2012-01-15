/******************************************************************************
 * Ogre::Image bindings
 ******************************************************************************/

#include "ogre_prerequisites.h"
#include "ogre_image.h"

#include <OgreRoot.h>
#include <OgreImage.h>

DLL CoiHandle create_image()
{
  return new Ogre::Image();
}

DLL void image_load(CoiHandle image_handle, const char* filename)
{
  Ogre::Image* image = reinterpret_cast<Ogre::Image*>(image_handle);
  image->load(filename, Ogre::ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME);
}

DLL void image_load_group(CoiHandle image_handle, const char* filename, const char* group)
{
  Ogre::Image* image = reinterpret_cast<Ogre::Image*>(image_handle);
  image->load(filename, group);
}

DLL void image_flip_around_x(CoiHandle image_handle)
{
  Ogre::Image* image = reinterpret_cast<Ogre::Image*>(image_handle);
  image->flipAroundX();
}

DLL void image_flip_around_y(CoiHandle image_handle)
{
  Ogre::Image* image = reinterpret_cast<Ogre::Image*>(image_handle);
  image->flipAroundY();
}

DLL void image_delete(CoiHandle image_handle)
{
  Ogre::Image* image = reinterpret_cast<Ogre::Image*>(image_handle);
  delete image;
}


