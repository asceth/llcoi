/******************************************************************************
 * Ogre::Plane interface definition
 ******************************************************************************/

#pragma once

DLL CoiHandle create_plane(float a, float b, float c, float d);

DLL CoiHandle create_plane_from_normal(float normal_x, float normal_y, float normal_z, float offset);

