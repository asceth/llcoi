/******************************************************************************
 * plane_bind.cpp - bindings for Ogre::MovableObject
 ******************************************************************************
 * This file is part of
 *     __ __              _
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / /
 *  / // // /__ / /_/ // /
 * /_//_/ \___/ \____//_/
 *
 * Low Level C Ogre Interface (llcoi)
 *
 * See http://code.google.com/p/llcoi/ for more information.
 *
 * Copyright (c) 2011, Llcoi Team
 *
 * License: MIT
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/
#include "ogre_interface.h"

#include <OgreRoot.h>
#include <OgrePlane.h>
#include "ogre_manager.h"

/*
Plane ()
Plane (const Plane &rhs)
Plane (const Vector3 &rkNormal, const Vector3 &rkPoint)
Plane (const Vector3 &rkPoint0, const Vector3 &rkPoint1, const Vector3 &rkPoint2)
*/
// Plane (Real a, Real b, Real c, Real d)
PlaneHandle create_plane(coiReal a, coiReal b, coiReal c, coiReal d)
{
  return new Ogre::Plane(a, b, c, d);
}

//Plane (const Vector3 &rkNormal, Real fConstant)
PlaneHandle create_plane_from_normal(coiReal normal_x, coiReal normal_y, coiReal normal_z, coiReal offset)
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
