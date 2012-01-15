// Ogre::ManualObject bindings

#include "ogre_prerequisites.h"
#include "ogre_manual_object.h"

#include <OgreRoot.h>
#include <OgreManualObject.h>
#include "ogre_interface_manager.h"

/* template
void manual_object_x(CoiHandle manual_object_handle)
{
  HANDLE(Ogre::ManualObject*, manual_object);
}
*/



// ManualObject (const String &name)
CoiHandle create_manual_object(CoiHandle scene_manager_handle, const char* name)
{
  Ogre::SceneManager* scene_manager = reinterpret_cast<Ogre::SceneManager*>(scene_manager_handle);
  return scene_manager->createManualObject(name);
}

// virtual ~ManualObject ()
void manual_object_delete(CoiHandle manual_object_handle)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  delete manual_object;
}

// virtual void clear (void)
void manual_object_clear(CoiHandle manual_object_handle)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->clear();
}

// virtual void 	estimateVertexCount (size_t vcount)
void manual_object_estimate_vertex_count(CoiHandle manual_object_handle, int vcount)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->estimateVertexCount(vcount);
}


// virtual void 	estimateIndexCount (size_t icount)
void manual_object_estimate_index_count(CoiHandle manual_object_handle, int icount)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->estimateIndexCount(icount);
}

// virtual void 	begin (const String &materialName, RenderOperation::OperationType opType=RenderOperation::OT_TRIANGLE_LIST, const String &groupName=ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME)
void manual_object_begin(CoiHandle manual_object_handle, const char* material_name, const int render_operation, const char* resource_group_name)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->begin(material_name, (Ogre::RenderOperation::OperationType)render_operation, resource_group_name);
}


// virtual void 	setDynamic (bool dyn)
void manual_object_set_dynamic(CoiHandle manual_object_handle, const int dyn)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->setDynamic(dyn);
}


// virtual bool 	getDynamic () const
int manual_object_get_dynamic(CoiHandle manual_object_handle)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  return manual_object->getDynamic();
}


// virtual void 	beginUpdate (size_t sectionIndex)
void manual_object_begin_update(CoiHandle manual_object_handle, const int section_index)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->beginUpdate(section_index);
}


// virtual void 	position (const Vector3 &pos)
// virtual void 	position (Real x, Real y, Real z)
void manual_object_position(CoiHandle manual_object_handle, float x, float y, float z)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->position(x, y, z);
}

// virtual void 	normal (const Vector3 &norm)
// virtual void 	normal (Real x, Real y, Real z)
void manual_object_normal(CoiHandle manual_object_handle, float x, float y, float z)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->normal(x, y, z);
}


// virtual void 	tangent (const Vector3 &tan)
// virtual void 	tangent (Real x, Real y, Real z)
void manual_object_tangent(CoiHandle manual_object_handle, float x, float y, float z)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->tangent(x, y, z);
}

// virtual void 	textureCoord (Real u)
void manual_object_texture_coord_u(CoiHandle manual_object_handle, float u)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->textureCoord(u);
}

// virtual void 	textureCoord (const Vector2 &uv)
// virtual void 	textureCoord (Real u, Real v)
void manual_object_texture_coord_uv(CoiHandle manual_object_handle, float u, float v)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->textureCoord(u, v);
}

// virtual void 	textureCoord (const Vector3 &uvw)
// virtual void 	textureCoord (Real u, Real v, Real w)
void manual_object_texture_coord_uvw(CoiHandle manual_object_handle, float u, float v, float w)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->textureCoord(u, v, w);
}

// virtual void 	textureCoord (const Vector4 &xyzw)
// virtual void 	textureCoord (Real x, Real y, Real z, Real w)
void manual_object_texture_coord_xyzw(CoiHandle manual_object_handle, float x, float y, float z, float w)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->textureCoord(x, y, z, w);
}

// virtual void 	colour (const ColourValue &col)
// virtual void 	colour (Real r, Real g, Real b, Real a=1.0f)
void manual_object_colour(CoiHandle manual_object_handle, float r, float g, float b, float a)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->colour(r, g, b, a);
}

// virtual void 	index (uint32 idx)
void manual_object_index(CoiHandle manual_object_handle, const unsigned int idx)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->index(idx);
}

// virtual void 	triangle (uint32 i1, uint32 i2, uint32 i3)
void manual_object_triangle(CoiHandle manual_object_handle, const unsigned int i1, const unsigned int i2, const unsigned int i3)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->triangle(i1, i2, i3);
}

// virtual void 	quad (uint32 i1, uint32 i2, uint32 i3, uint32 i4)
void manual_object_quad(CoiHandle manual_object_handle, const unsigned int i1, const unsigned int i2, const unsigned int i3, const unsigned int i4)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->quad(i1, i2, i3, i4);
}

// virtual ManualObjectSection * 	end (void)
CoiHandle manual_object_end(CoiHandle manual_object_handle)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  return manual_object->end();
}

// virtual void 	setMaterialName (size_t subindex, const String &name, const String &group=ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME)
void manual_object_set_material_name(CoiHandle manual_object_handle, const int sub_index, const char* name, const char* resource_group_name)
{
  HANDLE(Ogre::ManualObject*, manual_object);
  manual_object->setMaterialName(sub_index, name, resource_group_name);
}

// virtual MeshPtr 	convertToMesh (const String &meshName, const String &groupName=ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME)
// void 	setUseIdentityProjection (bool useIdentityProjection)
// bool 	getUseIdentityProjection (void) const
// void 	setUseIdentityView (bool useIdentityView)
// bool 	getUseIdentityView (void) const
// void 	setBoundingBox (const AxisAlignedBox &box)
// ManualObjectSection * 	getSection (unsigned int index) const
// unsigned int 	getNumSections (void) const
// void 	setKeepDeclarationOrder (bool keepOrder)
// bool 	getKeepDeclarationOrder () const
// const String & 	getMovableType (void) const
// const AxisAlignedBox & 	getBoundingBox (void) const
// Real 	getBoundingRadius (void) const
// void 	_updateRenderQueue (RenderQueue *queue)
// EdgeData * 	getEdgeList (void)
// bool 	hasEdgeList (void)
// ShadowRenderableListIterator 	getShadowVolumeRenderableIterator (ShadowTechnique shadowTechnique, const Light *light, HardwareIndexBufferSharedPtr *indexBuffer, bool extrudeVertices, Real extrusionDist, unsigned long flags=0)
// void 	visitRenderables (Renderable::Visitor *visitor, bool debugRenderables=false)
// virtual void 	_notifyCreator (MovableObjectFactory *fact)
// virtual MovableObjectFactory * 	_getCreator (void) const
// virtual void 	_notifyManager (SceneManager *man)
// virtual SceneManager * 	_getManager (void) const
// virtual const String & 	getName (void) const
// virtual Node * 	getParentNode (void) const
// virtual SceneNode * 	getParentSceneNode (void) const
// virtual bool 	isParentTagPoint () const
// virtual void 	_notifyAttached (Node *parent, bool isTagPoint=false)
// virtual bool 	isAttached (void) const
// virtual void 	detachFromParent (void)
// virtual bool 	isInScene (void) const
// virtual void 	_notifyMoved (void)
// virtual void 	_notifyCurrentCamera (Camera *cam)
// virtual const AxisAlignedBox & 	getWorldBoundingBox (bool derive=false) const
// virtual const Sphere & 	getWorldBoundingSphere (bool derive=false) const
// virtual void 	setVisible (bool visible)
// virtual bool 	getVisible (void) const
// virtual bool 	isVisible (void) const
// virtual void 	setRenderingDistance (Real dist)
// virtual Real 	getRenderingDistance (void) const
// virtual void 	setUserAny (const Any &anything)
// virtual const Any & 	getUserAny (void) const
// UserObjectBindings & 	getUserObjectBindings ()
// const UserObjectBindings & 	getUserObjectBindings () const
// virtual void 	setRenderQueueGroup (uint8 queueID)
// virtual void 	setRenderQueueGroupAndPriority (uint8 queueID, ushort priority)
// virtual uint8 	getRenderQueueGroup (void) const
// virtual const Matrix4 & 	_getParentNodeFullTransform (void) const
// virtual void 	setQueryFlags (uint32 flags)
// virtual void 	addQueryFlags (uint32 flags)
// virtual void 	removeQueryFlags (unsigned long flags)
// virtual uint32 	getQueryFlags (void) const
// virtual void 	setVisibilityFlags (uint32 flags)
// virtual void 	addVisibilityFlags (uint32 flags)
// virtual void 	removeVisibilityFlags (uint32 flags)
// virtual uint32 	getVisibilityFlags (void) const
// virtual void 	setListener (Listener *listener)
// virtual Listener * 	getListener (void) const
// virtual const LightList & 	queryLights (void) const
// virtual uint32 	getLightMask () const
// virtual void 	setLightMask (uint32 lightMask)
// virtual LightList * 	_getLightList ()
// const AxisAlignedBox & 	getLightCapBounds (void) const
// const AxisAlignedBox & 	getDarkCapBounds (const Light &light, Real dirLightExtrusionDist) const
// void 	setCastShadows (bool enabled)
// bool 	getCastShadows (void) const
// bool 	getReceivesShadows ()
// Real 	getPointExtrusionDistance (const Light *l) const
// virtual uint32 	getTypeFlags (void) const
// virtual void 	setDebugDisplayEnabled (bool enabled)
// virtual bool 	isDebugDisplayEnabled (void) const
// const StringVector & 	getAnimableValueNames (void) const
// virtual AnimableValuePtr 	createAnimableValue (const String &valueName)

