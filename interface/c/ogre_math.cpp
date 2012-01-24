// Ogre::Math bindings

#include "ogre_prerequisites.h"
#include "ogre_math.h"

#include <OgreRoot.h>
#include <OgreMath.h>

// Ogre::Math::Clamp(val, min, max)
float math_clamp_f(float value, float min, float max)
{
  return Ogre::Math::Clamp(value, min, max);
}

void multiply_quaternion_with_quaternion(CoiHandle quat1_handle, CoiHandle quat2_handle, CoiHandle quaternion_output_handle)
{
  HANDLE(coiQuaternion*, quat1);
  HANDLE(coiQuaternion*, quat2);
  HANDLE(coiQuaternion*, quaternion_output);

  Ogre::Quaternion ogre_quat1 = llcoi_quaternion_to_ogre_quaternion(*quat1);
  Ogre::Quaternion ogre_quat2 = llcoi_quaternion_to_ogre_quaternion(*quat2);

  ogre_quaternion_to_llcoi_quaternion_output(ogre_quat1 * ogre_quat2, quaternion_output);
}

void multiply_quaternion_with_vector3(CoiHandle quat_handle, CoiHandle vector_handle, CoiHandle vector_output_handle)
{
  HANDLE(coiQuaternion*, quat);
  HANDLE(coiVector3*, vector);
  HANDLE(coiVector3*, vector_output);

  Ogre::Quaternion ogre_quat = llcoi_quaternion_to_ogre_quaternion(*quat);
  Ogre::Vector3 ogre_vector = llcoi_vector3_to_ogre_vector3(*vector);

  ogre_vector3_to_llcoi_vector3_output(ogre_quat * ogre_vector, vector_output);
}

void multiply_vector3_with_vector3(CoiHandle vector1_handle, CoiHandle vector2_handle, CoiHandle vector_output_handle)
{
  HANDLE(coiVector3*, vector1);
  HANDLE(coiVector3*, vector2);
  HANDLE(coiVector3*, vector_output);

  Ogre::Vector3 ogre_vector1 = llcoi_vector3_to_ogre_vector3(*vector1);
  Ogre::Vector3 ogre_vector2 = llcoi_vector3_to_ogre_vector3(*vector2);

  ogre_vector3_to_llcoi_vector3_output(ogre_vector1 * ogre_vector2, vector_output);
}


