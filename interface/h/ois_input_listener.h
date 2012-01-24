// internal file, included by ois.cpp
// OIS::KeyListener, OIS::MouseListener

#pragma once

#define LISTENER_REMOVE(KLASS,ATTR)                                     \
  std::vector<KLASS>::iterator it = input_listener->ATTR.begin();       \
  std::vector<KLASS>::iterator end = input_listener->ATTR.end();        \
  while(it != end)                                                      \
    {                                                                   \
      if ((*it) == event)                                               \
        {                                                               \
          it = input_listener->ATTR.erase(it);                          \
        }                                                               \
      else                                                              \
        {                                                               \
          ++it;                                                         \
        }                                                               \
    }

class OISListenerBind : public OIS::KeyListener, public OIS::MouseListener
{
public:

  std::vector<KeyListenerEvent> key_pressed_listeners;
  std::vector<KeyListenerEvent> key_released_listeners;
  std::vector<MouseListenerEvent> mouse_pressed_listeners;
  std::vector<MouseListenerEvent> mouse_released_listeners;
  std::vector<MouseMovedListenerEvent> mouse_moved_listeners;

  bool keyPressed(const OIS::KeyEvent& e)
  {
    std::vector<KeyListenerEvent>::iterator it = key_pressed_listeners.begin();
    std::vector<KeyListenerEvent>::iterator end = key_pressed_listeners.end();

    while(it != end)
      {
        KeyListenerEvent func = *it;
        int result = (*func) ((int)e.key, e.text);
        if (result == 0)
          {
            return false;
          }
        ++it;
      }
    return true;
  }

  bool keyReleased(const OIS::KeyEvent& e)
  {
    std::vector<KeyListenerEvent>::iterator it = key_released_listeners.begin();
    std::vector<KeyListenerEvent>::iterator end = key_released_listeners.end();

    while(it != end)
      {
        KeyListenerEvent func = *it;
        int result = (*func) ((int)e.key, e.text);
        if (result == 0)
          {
            return false;
          }
        ++it;
      }
    return true;
  }

  bool mousePressed(const OIS::MouseEvent& e, OIS::MouseButtonID id)
  {
    MouseState ms = mouse_state_from_event(e);
    std::vector<MouseListenerEvent>::iterator it = mouse_pressed_listeners.begin();
    std::vector<MouseListenerEvent>::iterator end = mouse_pressed_listeners.end();

    while(it != end)
      {
        MouseListenerEvent func = *it;
        int result = (*func) (ms.width, ms.height, ms.x_rel, ms.x_abs, ms.x_abs_only, ms.y_rel, ms.y_abs, ms.y_abs_only, ms.z_rel, ms.z_abs, ms.z_abs_only, ms.buttons, (int)id);
        if (result == 0)
          {
            return false;
          }
        ++it;
      }
    return true;
  }

  bool mouseReleased(const OIS::MouseEvent& e, OIS::MouseButtonID id)
  {
    MouseState ms = mouse_state_from_event(e);
    std::vector<MouseListenerEvent>::iterator it = mouse_released_listeners.begin();
    std::vector<MouseListenerEvent>::iterator end = mouse_released_listeners.end();

    while(it != end)
      {
        MouseListenerEvent func = *it;
        int result = (*func) (ms.width, ms.height, ms.x_abs, ms.x_rel, ms.x_abs_only, ms.y_abs, ms.y_rel, ms.y_abs_only, ms.z_abs, ms.z_rel, ms.z_abs_only, ms.buttons, (int)id);
        if (result == 0)
          {
            return false;
          }
        ++it;
      }
    return true;
  }

  bool mouseMoved(const OIS::MouseEvent& e)
  {
    MouseState ms = mouse_state_from_event(e);
    std::vector<MouseMovedListenerEvent>::iterator it = mouse_moved_listeners.begin();
    std::vector<MouseMovedListenerEvent>::iterator end = mouse_moved_listeners.end();

    while(it != end)
      {
        MouseMovedListenerEvent func = *it;
        int result = (*func) (ms.width, ms.height, ms.x_abs, ms.x_rel, ms.x_abs_only, ms.y_abs, ms.y_rel, ms.y_abs_only, ms.z_abs, ms.z_rel, ms.z_abs_only, ms.buttons);
        if (result == 0)
          {
            return false;
          }
        ++it;
      }
    return true;
  }

  MouseState mouse_state_from_event(const OIS::MouseEvent& e)
  {
    MouseState ms;
    ms.width = e.state.width;
    ms.width = e.state.height;

    ms.x_abs = e.state.X.abs;
    ms.x_rel = e.state.X.rel;
    ms.x_abs_only = e.state.X.absOnly;

    ms.y_abs = e.state.Y.abs;
    ms.y_rel = e.state.Y.rel;
    ms.y_abs_only = e.state.Y.absOnly;

    ms.z_abs = e.state.Z.abs;
    ms.z_rel = e.state.Z.rel;
    ms.z_abs_only = e.state.Z.absOnly;

    ms.buttons = e.state.buttons;

    return ms;
  }
};

