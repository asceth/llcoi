/******************************************************************************
 * Ogre::RenderSystem bindings
 ******************************************************************************/

#include "ogre_prerequisites.h"
#include "ogre_render_system.h"

#include <OgreRoot.h>
#include <OgreRenderSystem.h>

CoiHandle get_render_system_by_name(const char* render_system_name)
{
    return Ogre::Root::getSingletonPtr()->getRenderSystemByName(render_system_name);
}

CoiHandle get_render_system()
{
    return Ogre::Root::getSingletonPtr()->getRenderSystem();
}

void add_render_system(CoiHandle render_system_handle)
{
    Ogre::RenderSystem* rs = reinterpret_cast<Ogre::RenderSystem*>(render_system_handle);
    Ogre::Root::getSingletonPtr()->addRenderSystem(rs);
}

void set_render_system(CoiHandle render_system_handle)
{
    Ogre::RenderSystem* rs = reinterpret_cast<Ogre::RenderSystem*>(render_system_handle);
    Ogre::Root::getSingletonPtr()->setRenderSystem(rs);
}

void render_system_set_config_option(CoiHandle render_system_handle, const char* option, const char* value)
{
    Ogre::RenderSystem* rs = reinterpret_cast<Ogre::RenderSystem*>(render_system_handle);
    rs->setConfigOption(option, value);
}
