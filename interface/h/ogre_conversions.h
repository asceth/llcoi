#pragma once

#include <OgreVector3.h>
#include <OgreQuaternion.h>
#include <OgreColourValue.h>

// ogre to llcoi
__inline__ coiVector3 ogre_vector3_to_llcoi_vector3(const Ogre::Vector3& vector)
{
  coiVector3 coivector = {vector.x, vector.y, vector.z};
  return coivector;
}

__inline__ coiQuaternion ogre_quaternion_to_llcoi_quaternion(const Ogre::Quaternion& quat)
{
  coiQuaternion coiquat = {quat.w, quat.x, quat.y, quat.z};
  return coiquat;
}

__inline__ coiColourValue ogre_colour_to_llcoi_colour(const Ogre::ColourValue& colour)
{
  coiColourValue coicolour = {colour.r, colour.g, colour.b, colour.a};
  return coicolour;
}

// llcoi to ogre
__inline__ Ogre::Vector3 llcoi_vector3_to_ogre_vector3(const coiVector3& vector)
{
  return Ogre::Vector3(vector.x, vector.y, vector.z);
}

__inline__ Ogre::Quaternion llcoi_quaternion_to_ogre_quaternion(const coiQuaternion& quat)
{
  return Ogre::Quaternion(quat.w, quat.x, quat.y, quat.z);
}

__inline__ Ogre::ColourValue llcoi_colour_to_ogre_colour(const coiColourValue& colour)
{
  return Ogre::ColourValue(colour.r, colour.g, colour.b, colour.a);
}


