// Ogre::Plane bindings

#include "ogre_prerequisites.h"
#include "ogre_plane.h"

#include <OgreRoot.h>
#include <OgrePlane.h>

/*
Plane ()
Plane (const Plane &rhs)
Plane (const Vector3 &rkNormal, const Vector3 &rkPoint)
Plane (const Vector3 &rkPoint0, const Vector3 &rkPoint1, const Vector3 &rkPoint2)
*/
// Plane (Real a, Real b, Real c, Real d)
CoiHandle create_plane(float a, float b, float c, float d)
{
  return new Ogre::Plane(a, b, c, d);
}

// Plane (const Vector3 &rkNormal, Real fConstant)
CoiHandle create_plane_from_normal(float normal_x, float normal_y, float normal_z, float offset)
{
  return new Ogre::Plane(Ogre::Vector3(normal_x, normal_y, normal_z), offset);
}


/*
enum  	Side { NO_SIDE, POSITIVE_SIDE, NEGATIVE_SIDE, BOTH_SIDE }

Functions
--

Side 	getSide (const Vector3 &rkPoint) const
Side 	getSide (const AxisAlignedBox &rkBox) const
Side 	getSide (const Vector3 &centre, const Vector3 &halfSize) const
Real 	getDistance (const Vector3 &rkPoint) const
void 	redefine (const Vector3 &rkPoint0, const Vector3 &rkPoint1, const Vector3 &rkPoint2)
void 	redefine (const Vector3 &rkNormal, const Vector3 &rkPoint)
Vector3 	projectVector (const Vector3 &v) const
Real 	normalise (void)
bool 	operator== (const Plane &rhs) const
bool 	operator!= (const Plane &rhs) const

Public Attributes
--
Vector3 	normal
Real 	d

_OgreExport friend std::ostream & 	operator<< (std::ostream &o, const Plane &p)
*/
