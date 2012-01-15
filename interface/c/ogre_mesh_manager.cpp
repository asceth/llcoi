/******************************************************************************
 * Ogre::MeshManager bindings
 ******************************************************************************/

#include "ogre_prerequisites.h"
#include "ogre_mesh_manager.h"

#include <OgreRoot.h>
#include <OgreMeshManager.h>

CoiHandle get_mesh_manager()
{
  return Ogre::MeshManager::getSingletonPtr();
}

// MeshPtr createPlane (const String &name, const String &groupName, const Plane &plane, Real width, Real height, int xsegments=1, int ysegments=1, bool normals=true, unsigned short numTexCoordSets=1, Real uTile=1.0f, Real vTile=1.0f, const Vector3 &upVector=Vector3::UNIT_Y, HardwareBuffer::Usage vertexBufferUsage=HardwareBuffer::HBU_STATIC_WRITE_ONLY, HardwareBuffer::Usage indexBufferUsage=HardwareBuffer::HBU_STATIC_WRITE_ONLY, bool vertexShadowBuffer=true, bool indexShadowBuffer=true)
void mesh_manager_create_plane(const char* name,
                               const char* group_name,
                               CoiHandle plane_handle,
                               float width,
                               float height,
                               int xsegments,
                               int ysegments,
                               int normals,
                               unsigned short num_tex_coord_sets,
                               float u_tile,
                               float v_tile,
                               float upvector_x,
                               float upvector_y,
                               float upvector_z)
{
  Ogre::MeshManager::getSingletonPtr()->createPlane(name, group_name, *reinterpret_cast<Ogre::Plane*>(plane_handle), width, height, xsegments, ysegments, normals, num_tex_coord_sets, u_tile, v_tile, Ogre::Vector3(upvector_x, upvector_y, upvector_z));
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

virtual void 	unload (ResourceCoiHandle handle)

virtual void 	unloadAll (bool reloadableOnly=true)

virtual void 	reloadAll (bool reloadableOnly=true)

virtual void 	unloadUnreferencedResources (bool reloadableOnly=true)

virtual void 	reloadUnreferencedResources (bool reloadableOnly=true)

virtual void 	remove (ResourcePtr &r)

virtual void 	remove (const String &name)

virtual void 	remove (ResourceCoiHandle handle)

virtual void 	removeAll (void)

virtual void 	removeUnreferencedResources (bool reloadableOnly=true)

virtual ResourcePtr 	getByName (const String &name, const String &groupName=ResourceGroupManager::AUTODETECT_RESOURCE_GROUP_NAME)

virtual ResourcePtr 	getByCoiHandle (ResourceCoiHandle handle)

virtual bool 	resourceExists (const String &name)

virtual bool 	resourceExists (ResourceCoiHandle handle)

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
