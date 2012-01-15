/******************************************************************************
 * Ogre::FrameListener bindings
 ******************************************************************************/

#include "ogre_prerequisites.h"
#include "ogre_frame_listener.h"

#include <vector>
#include <OgreRoot.h>
#include <OgreFrameListener.h>

// this is a binding class, it has 3 function pointers for
// frame listening event, each gets called if not null
class FrameListenerBind : public Ogre::FrameListener
{
public:
	FrameListenerBind(FrameListenerEvent fs, FrameListenerEvent frq, FrameListenerEvent fe)
		: frameStartedHandle(fs), frameRenderingQueuedHandle(frq), frameEndedHandle(fe)
	{
	}

	bool frameStarted(const Ogre::FrameEvent& evt)
	{
		if (frameStartedHandle)
      {
        return (*frameStartedHandle) (evt.timeSinceLastEvent,evt.timeSinceLastFrame,EVENT_FRAME_STARTED);
      }
		return true;
	}

	bool frameRenderingQueued(const Ogre::FrameEvent& evt)
	{
		if (frameRenderingQueuedHandle)
      {
        return (*frameRenderingQueuedHandle) (evt.timeSinceLastEvent,evt.timeSinceLastFrame,EVENT_FRAME_RENDERING_QUEUED);
      }
		return true;
	}

	bool frameEnded(const Ogre::FrameEvent& evt)
	{
		if (frameEndedHandle)
      {
        return (*frameEndedHandle) (evt.timeSinceLastEvent,evt.timeSinceLastFrame,EVENT_FRAME_ENDED);
      }
		return true;
	}

	FrameListenerEvent frameStartedHandle;
	FrameListenerEvent frameEndedHandle;
	FrameListenerEvent frameRenderingQueuedHandle;
};

// list of frame listeners for deleting
std::vector<FrameListenerBind*> frameListenerList;

void add_frame_listener(FrameListenerEvent frame_event, int frame_event_type)
{
	FrameListenerBind *frameListener =
		new FrameListenerBind(
                          ( frame_event_type&EVENT_FRAME_STARTED ? frame_event : 0 ),
                          ( frame_event_type&EVENT_FRAME_RENDERING_QUEUED ? frame_event : 0 ),
                          ( frame_event_type&EVENT_FRAME_ENDED ? frame_event : 0 ));

	Ogre::Root::getSingletonPtr()->addFrameListener(frameListener);
	frameListenerList.push_back(frameListener);
}

void remove_frame_listener(FrameListenerEvent frame_event)
{
	// this loop handles multiple occurances of frame_event in vector
	// and deletes them all
	for(std::vector<FrameListenerBind*>::iterator it = frameListenerList.begin();it!=frameListenerList.end();)
	{
		if ((*it)->frameStartedHandle == frame_event ||
				(*it)->frameEndedHandle == frame_event ||
				(*it)->frameRenderingQueuedHandle == frame_event)
		{
			Ogre::Root::getSingletonPtr()->removeFrameListener(*it);
			delete *it;
			it = frameListenerList.erase(it);
		}	else {
			++it;
		}
	}
}
