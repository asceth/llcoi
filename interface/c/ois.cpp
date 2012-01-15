/******************************************************************************
 * OIS bindings
 ******************************************************************************/

#include "ogre_prerequisites.h"
#include "ois_interface.h"

#include <OISMouse.h>
#include <OISKeyboard.h>
#include <OISJoyStick.h>
#include <OISInputManager.h>

#include <sstream>
OIS::InputManager* input_manager = 0;

void create_input_system(unsigned int window_handle)
{
  OIS::ParamList params;
  std::ostringstream wnd;
  wnd << window_handle;
  params.insert(std::make_pair(std::string("WINDOW"), wnd.str()));

  // FIXME: make this configurable
  params.insert(std::make_pair(std::string("x11_keyboard_grab"), std::string("false")));
  params.insert(std::make_pair(std::string("x11_mouse_grab"), std::string("false")));
  params.insert(std::make_pair(std::string("XAutoRepeatOn"), std::string("true")));

	input_manager = OIS::InputManager::createInputSystem(params);
}

void destroy_input_system()
{
	OIS::InputManager::destroyInputSystem(input_manager);
}

CoiHandle create_mouse_object(int buffered)
{
	OIS::Mouse* mouse = static_cast<OIS::Mouse*>(input_manager->createInputObject( OIS::OISMouse, (bool)buffered ));
	return mouse;
}

CoiHandle create_keyboard_object(int buffered)
{
    OIS::Keyboard* keyboard = static_cast<OIS::Keyboard*>(input_manager->createInputObject( OIS::OISKeyboard, (bool)buffered ));
	return keyboard;
}

void destroy_mouse_object(CoiHandle mouse_handle)
{
	OIS::Mouse* mouse = reinterpret_cast<OIS::Mouse*>(mouse_handle);
	input_manager->destroyInputObject(mouse);
}

void destroy_keyboard_object(CoiHandle keyboard_handle)
{
	OIS::Keyboard* keyboard = reinterpret_cast<OIS::Keyboard*>(keyboard_handle);
	input_manager->destroyInputObject(keyboard);
}

int keyboard_is_key_down(CoiHandle keyboard_handle, enum KeyCode key_code)
{
    OIS::Keyboard* keyboard = reinterpret_cast<OIS::Keyboard*>(keyboard_handle);
    if(keyboard->isKeyDown((OIS::KeyCode)key_code))
      {
        return 1;
      }
    return 0;
}

int keyboard_is_modifier_down(CoiHandle keyboard_handle, Key_Modifier key_modifier)
{
    OIS::Keyboard* keyboard = reinterpret_cast<OIS::Keyboard*>(keyboard_handle);
    if(keyboard->isModifierDown((OIS::Keyboard::Modifier)key_modifier))
      {
        return 1;
      }
    return 0;
}

MouseState mouse_get_state(CoiHandle mouse_handle)
{
    OIS::Mouse* mouse = reinterpret_cast<OIS::Mouse*>(mouse_handle);
    OIS::MouseState state = mouse->getMouseState();
    MouseState out_state;
    out_state.buttons = state.buttons;
    out_state.height = state.height;
    out_state.width = state.width;
    out_state.X.abs = state.X.abs;
    out_state.X.rel = state.X.rel;
    out_state.X.absOnly = state.X.absOnly;
    out_state.Y.abs = state.Y.abs;
    out_state.Y.rel = state.Y.rel;
    out_state.Y.absOnly = state.Y.absOnly;
    out_state.Z.abs = state.Z.abs;
    out_state.Z.rel = state.Z.rel;
    out_state.Z.absOnly = state.Z.absOnly;
    return out_state;
}

void mouse_set_buffered(CoiHandle mouse_handle, int buffered)
{
    OIS::Mouse* mouse = reinterpret_cast<OIS::Mouse*>(mouse_handle);
    mouse->setBuffered((bool)buffered);
}

void keyboard_set_buffered(CoiHandle keyboard_handle, int buffered)
{
    OIS::Keyboard* keyboard = reinterpret_cast<OIS::Keyboard*>(keyboard_handle);
    keyboard->setBuffered((bool)buffered);
}

void keyboard_capture(CoiHandle keyboard_handle)
{
    OIS::Keyboard* keyboard = reinterpret_cast<OIS::Keyboard*>(keyboard_handle);
    keyboard->capture();
}

void mouse_capture(CoiHandle mouse_handle)
{
    OIS::Mouse* mouse = reinterpret_cast<OIS::Mouse*>(mouse_handle);
    mouse->capture();
}
