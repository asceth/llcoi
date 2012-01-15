/******************************************************************************
 * Ogre::Viewport bindings
 ******************************************************************************/

#include "ogre_prerequisites.h"
#include "ogre_viewport.h"

#include <OgreRoot.h>
#include <OgreViewport.h>
#include <OgreRenderWindow.h>
#include <OgreCamera.h>
#include "ogre_interface_manager.h"

CoiHandle add_viewport(CoiHandle camera_handle)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    Ogre::RenderWindow* window = OgreManager::getSingletonPtr()->getActiveRenderWindow();
    Ogre::Viewport* viewport = window->addViewport(camera);
    return viewport;
}

void viewport_set_background_colour(CoiHandle viewport_handle, float r, float g, float b)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(viewport_handle);
    vp->setBackgroundColour(Ogre::ColourValue(r, g, b));
}

float viewport_get_width(CoiHandle viewport_handle)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(viewport_handle);
    return vp->getWidth();
}

float viewport_get_height(CoiHandle viewport_handle)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(viewport_handle);
    return vp->getHeight();
}

/*
Ogre::Viewport::Listener
Ogre::Viewport::operator=(Ogre::Viewport const&)
Ogre::Viewport::Viewport(Ogre::Viewport const&)
Ogre::Viewport::Viewport(Ogre::Camera*, Ogre::RenderTarget*, float, float, float, float, int)
Ogre::Viewport::~Viewport()
Ogre::Viewport::_updateDimensions()
Ogre::Viewport::update()
Ogre::Viewport::clear(unsigned int, Ogre::ColourValue const&, float, unsigned short)
Ogre::Viewport::getTarget() const
Ogre::Viewport::getCamera() const
Ogre::Viewport::setCamera(Ogre::Camera*)
Ogre::Viewport::getZOrder() const
Ogre::Viewport::getLeft() const
Ogre::Viewport::getTop() const
Ogre::Viewport::getWidth() const
Ogre::Viewport::getHeight() const
Ogre::Viewport::getActualLeft() const
Ogre::Viewport::getActualTop() const
Ogre::Viewport::getActualWidth() const
Ogre::Viewport::getActualHeight() const
Ogre::Viewport::setDimensions(float, float, float, float)
Ogre::Viewport::setOrientationMode(Ogre::OrientationMode, bool)
Ogre::Viewport::getOrientationMode() const
Ogre::Viewport::setDefaultOrientationMode(Ogre::OrientationMode)
Ogre::Viewport::getDefaultOrientationMode()
Ogre::Viewport::setBackgroundColour(Ogre::ColourValue const&)
Ogre::Viewport::getBackgroundColour() const
Ogre::Viewport::setDepthClear(float)
Ogre::Viewport::getDepthClear() const
Ogre::Viewport::setClearEveryFrame(bool, unsigned int)
Ogre::Viewport::getClearEveryFrame() const
Ogre::Viewport::getClearBuffers() const
Ogre::Viewport::setAutoUpdated(bool)
Ogre::Viewport::isAutoUpdated() const
Ogre::Viewport::setMaterialScheme(std::string const&)
Ogre::Viewport::getMaterialScheme() const
Ogre::Viewport::getActualDimensions(int&, int&, int&, int&) const
Ogre::Viewport::_isUpdated() const
Ogre::Viewport::_clearUpdatedFlag()
Ogre::Viewport::_getNumRenderedFaces() const
Ogre::Viewport::_getNumRenderedBatches() const
Ogre::Viewport::setOverlaysEnabled(bool)
Ogre::Viewport::getOverlaysEnabled() const
Ogre::Viewport::setSkiesEnabled(bool)
Ogre::Viewport::getSkiesEnabled() const
Ogre::Viewport::setShadowsEnabled(bool)
Ogre::Viewport::getShadowsEnabled() const
Ogre::Viewport::setVisibilityMask(unsigned int)
Ogre::Viewport::getVisibilityMask() const
Ogre::Viewport::setRenderQueueInvocationSequenceName(std::string const&)
Ogre::Viewport::getRenderQueueInvocationSequenceName() const
Ogre::Viewport::_getRenderQueueInvocationSequence()
Ogre::Viewport::pointOrientedToScreen(Ogre::Vector2 const&, int, Ogre::Vector2&)
Ogre::Viewport::pointOrientedToScreen(float, float, int, float&, float&)
Ogre::Viewport::addListener(Ogre::Viewport::Listener*)
Ogre::Viewport::removeListener(Ogre::Viewport::Listener*)
Ogre::Viewport::Listener::operator=(Ogre::Viewport::Listener const&)
Ogre::Viewport::Listener::Listener(Ogre::Viewport::Listener const&)
Ogre::Viewport::Listener::Listener()
Ogre::Viewport::Listener::~Listener()
Ogre::Viewport::Listener::viewportCameraChanged(Ogre::Viewport*)
Ogre::Viewport::Listener::viewportDimensionsChanged(Ogre::Viewport*)
Ogre::Viewport::Listener::viewportDestroyed(Ogre::Viewport*)
*/
