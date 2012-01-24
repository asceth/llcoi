/******************************************************************************
 * Ogre::SceneNode bindings
 ******************************************************************************/

#include "ogre_prerequisites.h"
#include "ogre_scene_node.h"

#include <OgreRoot.h>
#include <OgreSceneNode.h>
#include <OgreMovableObject.h>
#include <OgreEntity.h>
#include "ogre_interface_manager.h"

// SceneNode is going to carry Node's code - we'll duplicate for Bone
//TODO: need to have a function which translates to/from Ogre::Node::TransformSpace


// Maybe this would be enough? One could set position and orientation afterwards..
// Ogre::SceneNode::createChildSceneNode(Ogre::Vector3 const&, Ogre::Quaternion const&)
// Ogre::SceneNode::createChildSceneNode(std::string const&, Ogre::Vector3 const&, Ogre::Quaternion const&)
CoiHandle create_child_scene_node(CoiHandle scene_manager_handle, const char* node_name)
{
  HANDLE(Ogre::SceneManager*, scene_manager);
  return scene_manager->getRootSceneNode()->createChildSceneNode(node_name);
}

// Ogre::SceneNode::attachObject(Ogre::MovableObject*)
void scene_node_attach_entity(CoiHandle scene_node_handle, CoiHandle entity_handle)
{
  HANDLE(Ogre::Entity*, entity);
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->attachObject(entity);
}

// Ogre::SceneNode::_update(bool, bool)
void scene_node_update(CoiHandle scene_node_handle, int update_children, int parent_has_changed)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->_update(update_children, parent_has_changed);
}

// Ogre::SceneNode::_updateBounds()
void scene_node_update_bounds(CoiHandle scene_node_handle)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->_updateBounds();
}

// Ogre::SceneNode::getAttachedObject(unsigned short)
CoiHandle scene_node_get_attached_entity_int(CoiHandle scene_node_handle, int entity_index)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  return static_cast<Ogre::Entity*>(scene_node->getAttachedObject(entity_index));
}

// Ogre::SceneNode::getAttachedObject(std::string const&)
CoiHandle scene_node_get_attached_entity(CoiHandle scene_node_handle, const char* entity_name)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  return static_cast<Ogre::Entity*>(scene_node->getAttachedObject(entity_name));
}

//Ogre::SceneNode::numAttachedObjects() const
int scene_node_num_attached_objects(CoiHandle scene_node_handle)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  return scene_node->numAttachedObjects();
}

// Ogre::SceneNode::detachObject(unsigned short)
void scene_node_detach_entity_int(CoiHandle scene_node_handle, int entity_index)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->detachObject(entity_index);
}

// Ogre::SceneNode::detachObject(Ogre::MovableObject*)
void scene_node_detach_entity(CoiHandle scene_node_handle, CoiHandle entity_handle)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  HANDLE(Ogre::Entity*, entity);
  scene_node->detachObject(entity);
}

// Ogre::SceneNode::detachObject(std::string const&)
void scene_node_detach_entity_string(CoiHandle scene_node_handle, const char* entity_name)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->detachObject(entity_name);
}

// Ogre::SceneNode::detachAllObjects()
void scene_node_detach_all_objects(CoiHandle scene_node_handle)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->detachAllObjects();
}

// Ogre::SceneNode::isInSceneGraph() const
int scene_node_is_in_scenegraph(CoiHandle scene_node_handle)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  if(scene_node->isInSceneGraph())
    {
      return 1;
    }
  return 0;
}

// Ogre::SceneNode::_notifyRootNode()
void scene_node_notify_root_node(CoiHandle scene_node_handle)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->_notifyRootNode();
}

// Ogre::SceneNode::showBoundingBox(bool)
void scene_node_show_bounding_box(CoiHandle scene_node_handle, int show_boundingbox)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->showBoundingBox(show_boundingbox);
}

// Ogre::SceneNode::hideBoundingBox(bool)
void scene_node_hide_bounding_box(CoiHandle scene_node_handle, int hide_boundingbox)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->hideBoundingBox(hide_boundingbox);
}

// Ogre::SceneNode::getShowBoundingBox() const
int scene_node_get_show_bounding_box(CoiHandle scene_node_handle)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  if(scene_node->getShowBoundingBox())
    {
      return 1;
    }
  return 0;
}

// Ogre::SceneNode::getParentSceneNode() const
CoiHandle scene_node_get_parent_scene_node(CoiHandle scene_node_handle)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  return scene_node->getParentSceneNode();
}

// Ogre::SceneNode::setVisible(bool, bool)
void scene_node_set_visible(CoiHandle scene_node_handle, int visible)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->setVisible(visible);
}

// Ogre::SceneNode::setVisible(bool, bool)
void scene_node_set_visible_cascade(CoiHandle scene_node_handle, int visible, int cascade)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->setVisible(visible, cascade);
}

// Ogre::SceneNode::flipVisibility(bool)
void scene_node_flip_visibility(CoiHandle scene_node_handle)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->flipVisibility();
}

// Ogre::SceneNode::flipVisibility(bool)
void scene_node_flip_visibility_cascade(CoiHandle scene_node_handle, int cascade)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->flipVisibility(cascade);
}

// Ogre::SceneNode::setDebugDisplayEnabled(bool, bool)
void scene_node_set_debug_display_enabled(CoiHandle scene_node_handle, int enabled)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->setDebugDisplayEnabled(enabled);
}

// Ogre::SceneNode::setDebugDisplayEnabled(bool, bool)
void scene_node_set_debug_display_enabled_cascade(CoiHandle scene_node_handle, int enabled, int cascade)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->setDebugDisplayEnabled(enabled, cascade);
}

// Ogre::SceneNode::getCreator() const
CoiHandle scene_node_get_creator(CoiHandle scene_node_handle)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  return scene_node->getCreator();
}

// Ogre::SceneNode::setDirection(float, float, float, Ogre::Node::TransformSpace, Ogre::Vector3 const&)
void scene_node_set_direction(CoiHandle scene_node_handle, float x, float y, float z)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->setDirection(x, y, z);
}

// Ogre::Node::setOrientation(float, float, float, float)
// Ogre::Node::setOrientation(Ogre::Quaternion const&)
void scene_node_set_orientation(CoiHandle scene_node_handle, float w, float x, float y, float z)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->setOrientation(w, x, y, z);
}

// Ogre::Quaternion Ogre::Node::getOrientation() const
void scene_node_get_orientation(CoiHandle scene_node_handle, CoiHandle quaternion_output_handle)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  HANDLE(coiQuaternion*, quaternion_output);
  ogre_quaternion_to_llcoi_quaternion_output(scene_node->getOrientation(),
                                             quaternion_output);
}


//Ogre::Node::setPosition(float, float, float)
void scene_node_set_position(CoiHandle scene_node_handle, float x, float y, float z)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->setPosition(x, y, z);
}

// Ogre::SceneNode::yaw(Ogre::Radian const&, Ogre::Node::TransformSpace)
// Ogre::Node::yaw(Ogre::Radian const&, Ogre::Node::TransformSpace)
void scene_node_yaw(CoiHandle scene_node_handle, float radians)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->yaw(Ogre::Radian(radians));
}

// Ogre::Node::setScale(float, float, float)
void scene_node_set_scale(CoiHandle scene_node_handle, float x, float y, float z)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->setScale(x, y, z);
}

// Ogre::Node::scale(float, float, float)
void scene_node_scale(CoiHandle scene_node_handle, float x, float y, float z)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->scale(x, y, z);
}

// Ogre::Node::translate(float, float, float, Ogre::Node::TransformSpace)
void scene_node_translate(CoiHandle scene_node_handle, float x, float y, float z)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->translate(x, y, z);
}

// Ogre::Node::roll(Ogre::Radian const&, Ogre::Node::TransformSpace)
void scene_node_roll(CoiHandle scene_node_handle, float radians)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->roll(Ogre::Radian(radians));
}

// Ogre::Node::pitch(Ogre::Radian const&, Ogre::Node::TransformSpace)
void scene_node_pitch(CoiHandle scene_node_handle, float radians)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->pitch(Ogre::Radian(radians));
}

// Ogre::Node::removeChild(Ogre::Node*)
void scene_node_remove_child(CoiHandle scene_node_handle, CoiHandle child_handle)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  HANDLE(Ogre::SceneNode*, child);
  scene_node->removeChild(child);
}

// Ogre::Node::removeChild(unsigned short)
// Ogre::Node::removeChild(std::string const&)

// Ogre::Node::addChild(Ogre::Node*)
void scene_node_add_child(CoiHandle scene_node_handle, CoiHandle child_handle)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  HANDLE(Ogre::SceneNode*, child);
  scene_node->addChild(child);
}

//  Ogre::Node::createChild(Ogre::Vector3 const&, Ogre::Quaternion const&)
//  Ogre::Node::createChild(std::string const&, Ogre::Vector3 const&, Ogre::Quaternion const&)
CoiHandle scene_node_create_child_scene_node(CoiHandle scene_node_handle, float vx, float vy, float vz, float w, float x, float y, float z)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  scene_node->createChildSceneNode(Ogre::Vector3(vx, vy, vz), Ogre::Quaternion(w, x, y, z));
}

// void setAutoTracking (bool enabled, SceneNode *target=0, const Vector3 &localDirectionVector=Vector3::NEGATIVE_UNIT_Z, const Vector3 &offset=Vector3::ZERO)
void scene_node_set_auto_tracking(CoiHandle scene_node_handle, int enabled, CoiHandle target_handle)
{
  HANDLE(Ogre::SceneNode*, scene_node);
  HANDLE(Ogre::SceneNode*, target);
  scene_node->setAutoTracking((bool)enabled, target);
}


/*
  Ogre::SceneNode::operator=(Ogre::SceneNode const&)
  Ogre::SceneNode::SceneNode(Ogre::SceneNode const&)
  Ogre::SceneNode::SceneNode(Ogre::SceneManager*)
  Ogre::SceneNode::SceneNode(Ogre::SceneManager*, std::string const&)
  Ogre::SceneNode::~SceneNode()
  Ogre::SceneNode::_findVisibleObjects(Ogre::Camera*, Ogre::RenderQueue*, Ogre::VisibleObjectsBoundsInfo*, bool, bool, bool)
  Ogre::SceneNode::_getWorldAABB() const
  Ogre::SceneNode::getAttachedObjectIterator()
  Ogre::SceneNode::getAttachedObjectIterator() const
  Ogre::SceneNode::removeAndDestroyChild(std::string const&)
  Ogre::SceneNode::removeAndDestroyChild(unsigned short)
  Ogre::SceneNode::removeAndDestroyAllChildren()
  Ogre::SceneNode::_addBoundingBoxToQueue(Ogre::RenderQueue*)
  Ogre::SceneNode::findLights(Ogre::HashedVector<Ogre::Light*>&, float, unsigned int) const
  Ogre::SceneNode::setFixedYawAxis(bool, Ogre::Vector3 const&)
  Ogre::SceneNode::setDirection(Ogre::Vector3 const&, Ogre::Node::TransformSpace, Ogre::Vector3 const&)
  Ogre::SceneNode::lookAt(Ogre::Vector3 const&, Ogre::Node::TransformSpace, Ogre::Vector3 const&)
  Ogre::SceneNode::getAutoTrackTarget()
  Ogre::SceneNode::getAutoTrackOffset()
  Ogre::SceneNode::getAutoTrackLocalDirection()
  Ogre::SceneNode::_autoTrack()
  Ogre::SceneNode::getDebugRenderable()
*/
/*
  Ogre::Node::Listener
  Ogre::Node::DebugRenderable
  Ogre::Node::operator=(Ogre::Node const&)
  Ogre::Node::Node(Ogre::Node const&)
  Ogre::Node::Node()
  Ogre::Node::Node(std::string const&)
  Ogre::Node::~Node()
  Ogre::Node::getName() const
  Ogre::Node::getParent() const
  Ogre::Node::resetOrientation()
  Ogre::Node::setPosition(Ogre::Vector3 const&)
  Ogre::Node::getPosition() const
  Ogre::Node::setScale(Ogre::Vector3 const&)
  Ogre::Node::getScale() const
  Ogre::Node::setInheritOrientation(bool)
  Ogre::Node::getInheritOrientation() const
  Ogre::Node::setInheritScale(bool)
  Ogre::Node::getInheritScale() const
  Ogre::Node::scale(Ogre::Vector3 const&)
  Ogre::Node::translate(Ogre::Vector3 const&, Ogre::Node::TransformSpace)
  Ogre::Node::translate(Ogre::Matrix3 const&, Ogre::Vector3 const&, Ogre::Node::TransformSpace)
  Ogre::Node::translate(Ogre::Matrix3 const&, float, float, float, Ogre::Node::TransformSpace)
  Ogre::Node::rotate(Ogre::Vector3 const&, Ogre::Radian const&, Ogre::Node::TransformSpace)
  Ogre::Node::rotate(Ogre::Quaternion const&, Ogre::Node::TransformSpace)
  Ogre::Node::getLocalAxes() const
  Ogre::Node::createChild(Ogre::Vector3 const&, Ogre::Quaternion const&)
  Ogre::Node::createChild(std::string const&, Ogre::Vector3 const&, Ogre::Quaternion const&)
  Ogre::Node::numChildren() const
  Ogre::Node::getChild(unsigned short) const
  Ogre::Node::getChild(std::string const&) const
  Ogre::Node::getChildIterator()
  Ogre::Node::getChildIterator() const
  Ogre::Node::removeAllChildren()
  Ogre::Node::_setDerivedPosition(Ogre::Vector3 const&)
  Ogre::Node::_setDerivedOrientation(Ogre::Quaternion const&)
  Ogre::Node::_getDerivedOrientation() const
  Ogre::Node::_getDerivedPosition() const
  Ogre::Node::_getDerivedScale() const
  Ogre::Node::_getFullTransform() const
  Ogre::Node::_update(bool, bool)
  Ogre::Node::setListener(Ogre::Node::Listener*)
  Ogre::Node::getListener() const
  Ogre::Node::setInitialState()
  Ogre::Node::resetToInitialState()
  Ogre::Node::getInitialPosition() const
  Ogre::Node::convertWorldToLocalPosition(Ogre::Vector3 const&)
  Ogre::Node::convertLocalToWorldPosition(Ogre::Vector3 const&)
  Ogre::Node::convertWorldToLocalOrientation(Ogre::Quaternion const&)
  Ogre::Node::convertLocalToWorldOrientation(Ogre::Quaternion const&)
  Ogre::Node::getInitialOrientation() const
  Ogre::Node::getInitialScale() const
  Ogre::Node::getSquaredViewDepth(Ogre::Camera const*) const
  Ogre::Node::needUpdate(bool)
  Ogre::Node::requestUpdate(Ogre::Node*, bool)
  Ogre::Node::cancelUpdate(Ogre::Node*)
  Ogre::Node::getDebugRenderable(float)
  Ogre::Node::queueNeedUpdate(Ogre::Node*)
  Ogre::Node::processQueuedUpdates()
  Ogre::Node::setUserAny(Ogre::Any const&)
  Ogre::Node::getUserAny() const
  Ogre::Node::getUserObjectBindings()
  Ogre::Node::getUserObjectBindings() const
  Ogre::Node::Listener::operator=(Ogre::Node::Listener const&)
  Ogre::Node::Listener::Listener(Ogre::Node::Listener const&)
  Ogre::Node::Listener::Listener()
  Ogre::Node::Listener::~Listener()
  Ogre::Node::Listener::nodeUpdated(Ogre::Node const*)
  Ogre::Node::Listener::nodeDestroyed(Ogre::Node const*)
  Ogre::Node::Listener::nodeAttached(Ogre::Node const*)
  Ogre::Node::Listener::nodeDetached(Ogre::Node const*)
  Ogre::Node::DebugRenderable::operator=(Ogre::Node::DebugRenderable const&)
  Ogre::Node::DebugRenderable::DebugRenderable(Ogre::Node::DebugRenderable const&)
  Ogre::Node::DebugRenderable::DebugRenderable(Ogre::Node*)
  Ogre::Node::DebugRenderable::~DebugRenderable()
  Ogre::Node::DebugRenderable::getMaterial() const
  Ogre::Node::DebugRenderable::getRenderOperation(Ogre::RenderOperation&)
  Ogre::Node::DebugRenderable::getWorldTransforms(Ogre::Matrix4*) const
  Ogre::Node::DebugRenderable::getSquaredViewDepth(Ogre::Camera const*) const
  Ogre::Node::DebugRenderable::getLights() const
  Ogre::Node::DebugRenderable::setScaling(float)
*/
