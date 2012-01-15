/******************************************************************************
 * Ogre::FrameListener interface definition
 ******************************************************************************/

#pragma once

DLL void add_frame_listener(FrameListenerEvent frame_event, int frame_event_type);

DLL void remove_frame_listener(FrameListenerEvent frame_event);
