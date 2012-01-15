/******************************************************************************
 * Ogre::WindowEventListener bindings
 ******************************************************************************/

#include "ogre_prerequisites.h"
#include "ogre_window_event_listener.h"

#include <vector>
#include <OgreRoot.h>
#include <OgreWindowEventUtilities.h>

// this is a binding class, it has 1 function pointer for
// window event listening, it gets called if not null
// Only supports one window event listener - for now
class WindowEventListenerBind : public Ogre::WindowEventListener
{
public:
	WindowEventListenerBind(WindowListenerEvent wc)
		: windowClosedHandle(wc)
	{
	}

	void windowClosed(Ogre::RenderWindow* rw)
	{
		if (windowClosedHandle)
      {
        windowClosedHandle(rw);
      }
	}

	WindowListenerEvent windowClosedHandle;
};

WindowEventListenerBind *windowEventListener;

void add_window_listener(CoiHandle render_window_handle, WindowListenerEvent window_event)
{
	windowEventListener = new WindowEventListenerBind(window_event);

	Ogre::WindowEventUtilities::addWindowEventListener(reinterpret_cast<Ogre::RenderWindow*>(render_window_handle), windowEventListener);
}

void remove_window_listener(CoiHandle render_window_handle)
{
	Ogre::WindowEventUtilities::removeWindowEventListener(reinterpret_cast<Ogre::RenderWindow*>(render_window_handle), windowEventListener);
}

/*
Ogre::WindowEventUtilities::_msListeners
Ogre::WindowEventUtilities::_msWindows
Ogre::WindowEventUtilities::~WindowEventUtilities()
Ogre::WindowEventUtilities::operator=(Ogre::WindowEventUtilities const&)
Ogre::WindowEventUtilities::WindowEventUtilities(Ogre::WindowEventUtilities const&)
Ogre::WindowEventUtilities::WindowEventUtilities()
Ogre::WindowEventUtilities::messagePump()
Ogre::WindowEventUtilities::addWindowEventListener(Ogre::RenderWindow*, Ogre::WindowEventListener*)
Ogre::WindowEventUtilities::removeWindowEventListener(Ogre::RenderWindow*, Ogre::WindowEventListener*)
Ogre::WindowEventUtilities::_addRenderWindow(Ogre::RenderWindow*)
Ogre::WindowEventUtilities::_removeRenderWindow(Ogre::RenderWindow*)
*/
