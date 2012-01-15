/******************************************************************************
 * Ogre::Math bindings
 ******************************************************************************/

#include "ogre_prerequisites.h"
#include "ogre_math.h"

#include <OgreRoot.h>
#include <OgreMath.h>

// Ogre::Math::Clamp(val, min, max)
float math_clamp_f(float value, float min, float max)
{
  Ogre::Math::Clamp(value, min, max);
}
