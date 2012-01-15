/******************************************************************************
 * Ogre::RenderWindow bindings
 ******************************************************************************/

#include "ogre_prerequisites.h"
#include "ogre_render_window.h"

#include <OgreRoot.h>
#include <OgreRenderWindow.h>

#include "ogre_interface_manager.h"

CoiHandle create_render_window(const char* name, const int width, const int height, const int full_screen)
{
  Ogre::RenderWindow* window = Ogre::Root::getSingletonPtr()->createRenderWindow(name, width, height, full_screen);
  OgreManager::getSingletonPtr()->setActiveRenderWindow(window);

  return window;
}

CoiHandle create_render_window_hwnd(const char* name, const int width, const int height, const int full_screen, unsigned long hwnd)
{
  Ogre::NameValuePairList misc;
  misc["parentWindowHandle"] = Ogre::StringConverter::toString(hwnd);
  Ogre::RenderWindow* window = Ogre::Root::getSingletonPtr()->createRenderWindow(name, width, height, full_screen, &misc);
  OgreManager::getSingletonPtr()->setActiveRenderWindow(window);

  window->setActive(true);
  return window;
}

CoiHandle create_render_window_gl_context(const char* name, const int width, const int height, const int full_screen)
{
  Ogre::NameValuePairList misc;
  // Tell Ogre to use the current GL context.  This works on Linux/GLX but
  // you *will* need something different on Windows or Mac.
  misc["currentGLContext"] = Ogre::String("True");
  Ogre::RenderWindow* window = Ogre::Root::getSingletonPtr()->createRenderWindow(name, width, height, full_screen, &misc);
  OgreManager::getSingletonPtr()->setActiveRenderWindow(window);

  return window;
}

void render_window_set_visible(CoiHandle render_window_handle, int visible)
{
    Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(render_window_handle);
    window->setActive(true);
    window->setVisible(visible);
}

unsigned int render_window_get_hwnd(CoiHandle render_window_handle)
{
	size_t windowHnd = 0;
	Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(render_window_handle);
	window->getCustomAttribute("WINDOW", &windowHnd);
	return windowHnd;
}

void render_window_update(CoiHandle render_window_handle, int swap_buffers)
{
  Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(render_window_handle);
  window->update(swap_buffers);
}

void render_window_resize(CoiHandle render_window_handle, unsigned int width, unsigned int height)
{
  Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(render_window_handle);
	window->resize(width, height);
}

void render_window_moved_or_resized(CoiHandle render_window_handle)
{
  Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(render_window_handle);
	window->windowMovedOrResized();
}

int render_window_closed(CoiHandle render_window_handle)
{
  Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(render_window_handle);
  if(window->isClosed())
    {
      return 1;
    }

  return 0;
}

/*
Ogre::RenderWindow::operator=(Ogre::RenderWindow const&)
Ogre::RenderWindow::RenderWindow(Ogre::RenderWindow const&)
Ogre::RenderWindow::RenderWindow()
Ogre::RenderWindow::create(std::string const&, unsigned int, unsigned int, bool, std::map<std::string, std::string, std::less<std::string>, Ogre::STLAllocator<std::pair<std::string const, std::string>, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > > const*)
Ogre::RenderWindow::setFullscreen(bool, unsigned int, unsigned int)
Ogre::RenderWindow::destroy()
Ogre::RenderWindow::resize(unsigned int, unsigned int)
Ogre::RenderWindow::windowMovedOrResized()
Ogre::RenderWindow::reposition(int, int)
Ogre::RenderWindow::isVisible() const
Ogre::RenderWindow::setVisible(bool)
Ogre::RenderWindow::isHidden() const
Ogre::RenderWindow::setHidden(bool)
Ogre::RenderWindow::setVSyncEnabled(bool)
Ogre::RenderWindow::isVSyncEnabled() const
Ogre::RenderWindow::setVSyncInterval(unsigned int)
Ogre::RenderWindow::getVSyncInterval() const
Ogre::RenderWindow::isActive() const
Ogre::RenderWindow::isClosed() const
Ogre::RenderWindow::isPrimary() const
Ogre::RenderWindow::isFullScreen() const
Ogre::RenderWindow::getMetrics(unsigned int&, unsigned int&, unsigned int&, int&, int&)
Ogre::RenderWindow::suggestPixelFormat() const
Ogre::RenderWindow::isDeactivatedOnFocusChange() const
Ogre::RenderWindow::setDeactivateOnFocusChange(bool)
Ogre::RenderWindow::~RenderWindow()
*/
