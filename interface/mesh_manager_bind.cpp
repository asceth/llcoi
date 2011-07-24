/******************************************************************************
 * meshmanager_bind.cpp - bindings for Ogre::MeshManager
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
#include <OgreMeshManager.h>
#include "ogre_manager.h"

MeshManagerHandle mesh_manager_get()
{
  return Ogre::MeshManager::getSingletonPtr();
}

// MeshPtr 	createPlane (const String &name, const String &groupName, const Plane &plane, Real width, Real height, int xsegments=1, int ysegments=1, bool normals=true, unsigned short numTexCoordSets=1, Real uTile=1.0f, Real vTile=1.0f, const Vector3 &upVector=Vector3::UNIT_Y, HardwareBuffer::Usage vertexBufferUsage=HardwareBuffer::HBU_STATIC_WRITE_ONLY, HardwareBuffer::Usage indexBufferUsage=HardwareBuffer::HBU_STATIC_WRITE_ONLY, bool vertexShadowBuffer=true, bool indexShadowBuffer=true)
void mesh_manager_create_plane(const char* name, const char* group_name, PlaneHandle plane, coiReal width, coiReal height, int xsegments, int ysegments, int normals, unsigned short num_tex_coord_sets, coiReal u_tile, coiReal v_tile, coiReal upvector_x, coiReal upvector_y, coiReal upvector_z)
{
  Ogre::MeshManager::getSingletonPtr()->createPlane(name, group_name, *reinterpret_cast<Ogre::Plane*>(plane), width, height, xsegments, ysegments, normals, num_tex_coord_sets, u_tile, v_tile, Ogre::Vector3(upvector_x, upvector_y, upvector_z));
}



/*
Functions
--
MeshManager ()
~MeshManager ()
ResourceCreateOrRetrieveResult 	createOrRetrieve (const String &name, const String &group, bool isManual=false, ManualResourceLoader *loader=0, const NameValuePairList *params=0, HardwareBuffer::Usage vertexBufferUsage=HardwareBuffer::HBU_STATIC_WRITE_ONLY, HardwareBuffer::Usage indexBufferUsage=HardwareBuffer::HBU_STATIC_WRITE_ONLY, bool vertexBufferShadowed=true, bool indexBufferShadowed=true)

MeshPtr 	prepare (const String &filename, const String &groupName, HardwareBuffer::Usage vertexBufferUsage=HardwareBuffer::HBU_STATIC_WRITE_ONLY, HardwareBuffer::Usage indexBufferUsage=HardwareBuffer::HBU_STATIC_WRITE_ONLY, bool vertexBufferShadowed=true, bool indexBufferShadowed=true)

MeshPtr 	load (const String &filename, const String &groupName, HardwareBuffer::Usage vertexBufferUsage=HardwareBuffer::HBU_STATIC_WRITE_ONLY, HardwareBuffer::Usage indexBufferUsage=HardwareBuffer::HBU_STATIC_WRITE_ONLY, bool vertexBufferShadowed=true, bool indexBufferShadowed=true)

MeshPtr 	createManual (const String &name, const String &groupName, ManualResourceLoader *loader=0)

MeshPtr 	createCurvedIllusionPlane (const String &name, const String &groupName, const Plane &plane, Real width, Real height, Real curvature, int xsegments=1, int ysegments=1, bool normals=true, unsigned short numTexCoordSets=1, Real uTile=1.0f, Real vTile=1.0f, const Vector3 &upVector=Vector3::UNIT_Y, const Quaternion &orientation=Quaternion::IDENTITY, HardwareBuffer::Usage vertexBufferUsage=HardwareBuffer::HBU_STATIC_WRITE_ONLY, HardwareBuffer::Usage indexBufferUsage=HardwareBuffer::HBU_STATIC_WRITE_ONLY, bool vertexShadowBuffer=true, bool indexShadowBuffer=true, int ySegmentsToKeep=-1)

MeshPtr 	createCurvedPlane (const String &name, const String &groupName, const Plane &plane, Real width, Real height, Real bow=0.5f, int xsegments=1, int ysegments=1, bool normals=false, unsigned short numTexCoordSets=1, Real xTile=1.0f, Real yTile=1.0f, const Vector3 &upVector=Vector3::UNIT_Y, HardwareBuffer::Usage vertexBufferUsage=HardwareBuffer::HBU_STATIC_WRITE_ONLY, HardwareBuffer::Usage indexBufferUsage=HardwareBuffer::HBU_STATIC_WRITE_ONLY, bool vertexShadowBuffer=true, bool indexShadowBuffer=true)

PatchMeshPtr 	createBezierPatch (const String &name, const String &groupName, void *controlPointBuffer, VertexDeclaration *declaration, size_t width, size_t height, size_t uMaxSubdivisionLevel=PatchSurface::AUTO_LEVEL, size_t vMaxSubdivisionLevel=PatchSurface::AUTO_LEVEL, PatchSurface::VisibleSide visibleSide=PatchSurface::VS_FRONT, HardwareBuffer::Usage vbUsage=HardwareBuffer::HBU_STATIC_WRITE_ONLY, HardwareBuffer::Usage ibUsage=HardwareBuffer::HBU_DYNAMIC_WRITE_ONLY, bool vbUseShadow=true, bool ibUseShadow=true)

void 	setPrepareAllMeshesForShadowVolumes (bool enable)

bool 	getPrepareAllMeshesForShadowVolumes (void)

Real 	getBoundsPaddingFactor (void)

void 	setBoundsPaddingFactor (Real paddingFactor)

void 	setListener (MeshSerializerListener *listener)

MeshSerializerListener * 	getListener ()

void 	loadResource (Resource *res)
virtual ResourcePtr 	create (const String &name, const String &group, bool isManual=false, ManualResourceLoader *loader=0, const NameValuePairList *createParams=0)

virtual
ResourceCreateOrRetrieveResult 	createOrRetrieve (const String &name, const String &group, bool isManual=false, ManualResourceLoader *loader=0, const NameValuePairList *createParams=0)

virtual void 	setMemoryBudget (size_t bytes)

virtual size_t 	getMemoryBudget (void) const

virtual size_t 	getMemoryUsage (void) const

virtual void 	unload (const String &name)

virtual void 	unload (ResourceHandle handle)

virtual void 	unloadAll (bool reloadableOnly=true)

virtual void 	reloadAll (bool reloadableOnly=true)

virtual void 	unloadUnreferencedResources (bool reloadableOnly=true)

virtual void 	reloadUnreferencedResources (bool reloadableOnly=true)

virtual void 	remove (ResourcePtr &r)

virtual void 	remove (const String &name)

virtual void 	remove (ResourceHandle handle)

virtual void 	removeAll (void)

virtual void 	removeUnreferencedResources (bool reloadableOnly=true)

virtual ResourcePtr 	getByName (const String &name, const String &groupName=ResourceGroupManager::AUTODETECT_RESOURCE_GROUP_NAME)

virtual ResourcePtr 	getByHandle (ResourceHandle handle)

virtual bool 	resourceExists (const String &name)

virtual bool 	resourceExists (ResourceHandle handle)

virtual void 	_notifyResourceTouched (Resource *res)

virtual void 	_notifyResourceLoaded (Resource *res)

virtual void 	_notifyResourceUnloaded (Resource *res)

virtual ResourcePtr 	prepare (const String &name, const String &group, bool isManual=false, ManualResourceLoader *loader=0, const NameValuePairList *loadParams=0, bool backgroundThread=false)

virtual ResourcePtr 	load (const String &name, const String &group, bool isManual=false, ManualResourceLoader *loader=0, const NameValuePairList *loadParams=0, bool backgroundThread=false)

virtual const StringVector & 	getScriptPatterns (void) const

virtual void 	parseScript (DataStreamPtr &stream, const String &groupName)

virtual Real 	getLoadingOrder (void) const

const String & 	getResourceType (void) const

virtual void 	setVerbose (bool v)

virtual bool 	getVerbose (void)

ResourcePool * 	getResourcePool (const String &name)

void 	destroyResourcePool (ResourcePool *pool)

void 	destroyResourcePool (const String &name)

void 	destroyAllResourcePools ()

ResourceMapIterator 	getResourceIterator (void)

virtual void 	prepareResource (Resource *resource)
*/
