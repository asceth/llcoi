// OIS bindings

#include "ogre_prerequisites.h"
#include "ois_interface.h"

#include <OISMouse.h>
#include <OISKeyboard.h>
#include <OISJoyStick.h>
#include <OISInputManager.h>

#include "ois_input_listener.h"

#include <vector>
#include <sstream>
#include <map>
#include <string>

std::map<std::string, int> key_map;
std::map<int, std::string> key_reverse_map;

CoiHandle create_input_system(unsigned int window_handle)
{
  OIS::ParamList params;
  std::ostringstream wnd;
  wnd << window_handle;
  params.insert(std::make_pair(std::string("WINDOW"), wnd.str()));

  // FIXME: make this configurable
  params.insert(std::make_pair(std::string("x11_keyboard_grab"), std::string("false")));
  params.insert(std::make_pair(std::string("x11_mouse_grab"), std::string("false")));
  params.insert(std::make_pair(std::string("XAutoRepeatOn"), std::string("true")));

  default_key_mappings();

	return OIS::InputManager::createInputSystem(params);
}

void destroy_input_system(CoiHandle input_manager_handle)
{
  HANDLE(OIS::InputManager*, input_manager);
	OIS::InputManager::destroyInputSystem(input_manager);
}

CoiHandle create_mouse_object(CoiHandle input_manager_handle, int buffered)
{
  HANDLE(OIS::InputManager*, input_manager);
	return static_cast<OIS::Mouse*>(input_manager->createInputObject( OIS::OISMouse, (bool)buffered ));
}

CoiHandle create_keyboard_object(CoiHandle input_manager_handle, int buffered)
{
  HANDLE(OIS::InputManager*, input_manager);
  return static_cast<OIS::Keyboard*>(input_manager->createInputObject( OIS::OISKeyboard, (bool)buffered ));
}

void destroy_mouse_object(CoiHandle input_manager_handle, CoiHandle mouse_handle)
{
  HANDLE(OIS::InputManager*, input_manager);
  HANDLE(OIS::Mouse*, mouse);
	input_manager->destroyInputObject(mouse);
}

void destroy_keyboard_object(CoiHandle input_manager_handle, CoiHandle keyboard_handle)
{
  HANDLE(OIS::InputManager*, input_manager);
  HANDLE(OIS::Keyboard*, keyboard);
	input_manager->destroyInputObject(keyboard);
}

int keyboard_is_key_down(CoiHandle keyboard_handle, enum KeyCode key_code)
{
  HANDLE(OIS::Keyboard*, keyboard);
  if(keyboard->isKeyDown((OIS::KeyCode)key_code))
    {
      return 1;
    }
  return 0;
}

int keyboard_is_modifier_down(CoiHandle keyboard_handle, Key_Modifier key_modifier)
{
  HANDLE(OIS::Keyboard*, keyboard);
  if(keyboard->isModifierDown((OIS::Keyboard::Modifier)key_modifier))
    {
      return 1;
    }
  return 0;
}

MouseState mouse_get_state(CoiHandle mouse_handle)
{
  HANDLE(OIS::Mouse*, mouse);
  OIS::MouseState state = mouse->getMouseState();
  MouseState out_state;
  out_state.buttons = state.buttons;
  out_state.height = state.height;
  out_state.width = state.width;
  out_state.x_abs = state.X.abs;
  out_state.x_rel = state.X.rel;
  out_state.x_abs_only = state.X.absOnly;
  out_state.y_abs = state.Y.abs;
  out_state.y_rel = state.Y.rel;
  out_state.y_abs_only = state.Y.absOnly;
  out_state.z_abs = state.Z.abs;
  out_state.z_rel = state.Z.rel;
  out_state.z_abs_only = state.Z.absOnly;
  return out_state;
}

void mouse_set_buffered(CoiHandle mouse_handle, int buffered)
{
  HANDLE(OIS::Mouse*, mouse);
  mouse->setBuffered((bool)buffered);
}

void keyboard_set_buffered(CoiHandle keyboard_handle, int buffered)
{
  HANDLE(OIS::Keyboard*, keyboard);
  keyboard->setBuffered((bool)buffered);
}

void keyboard_capture(CoiHandle keyboard_handle)
{
  HANDLE(OIS::Keyboard*, keyboard);
  keyboard->capture();
}

void mouse_capture(CoiHandle mouse_handle)
{
  HANDLE(OIS::Mouse*, mouse);
  mouse->capture();
}


CoiHandle create_input_listener()
{
  return new OISListenerBind();
}

void attach_keyboard_listener(CoiHandle keyboard_handle, CoiHandle input_listener_handle)
{
  HANDLE(OIS::Keyboard*, keyboard);
  HANDLE(OISListenerBind*, input_listener);
  keyboard->setEventCallback(input_listener);
}

void attach_mouse_listener(CoiHandle mouse_handle, CoiHandle input_listener_handle)
{
  HANDLE(OIS::Mouse*, mouse);
  HANDLE(OISListenerBind*, input_listener);
  mouse->setEventCallback(input_listener);
}

void add_key_pressed_listener(CoiHandle input_listener_handle, KeyListenerEvent event)
{
  HANDLE(OISListenerBind*, input_listener);
  input_listener->key_pressed_listeners.push_back(event);
}

void add_key_released_listener(CoiHandle input_listener_handle, KeyListenerEvent event)
{
  HANDLE(OISListenerBind*, input_listener);
  input_listener->key_released_listeners.push_back(event);
}

void add_mouse_pressed_listener(CoiHandle input_listener_handle, MouseListenerEvent event)
{
  HANDLE(OISListenerBind*, input_listener);
  input_listener->mouse_pressed_listeners.push_back(event);
}

void add_mouse_released_listener(CoiHandle input_listener_handle, MouseListenerEvent event)
{
  HANDLE(OISListenerBind*, input_listener);
  input_listener->mouse_released_listeners.push_back(event);
}

void add_mouse_moved_listener(CoiHandle input_listener_handle, MouseMovedListenerEvent event)
{
  HANDLE(OISListenerBind*, input_listener);
  input_listener->mouse_moved_listeners.push_back(event);
}

void remove_key_pressed_listener(CoiHandle input_listener_handle, KeyListenerEvent event)
{
  HANDLE(OISListenerBind*, input_listener);
  LISTENER_REMOVE(KeyListenerEvent, key_pressed_listeners);
}

void remove_key_released_listener(CoiHandle input_listener_handle, KeyListenerEvent event)
{
  HANDLE(OISListenerBind*, input_listener);
  LISTENER_REMOVE(KeyListenerEvent, key_released_listeners);
}

void remove_mouse_pressed_listener(CoiHandle input_listener_handle, MouseListenerEvent event)
{
  HANDLE(OISListenerBind*, input_listener);
  LISTENER_REMOVE(MouseListenerEvent, mouse_pressed_listeners);
}

void remove_mouse_released_listener(CoiHandle input_listener_handle, MouseListenerEvent event)
{
  HANDLE(OISListenerBind*, input_listener);
  LISTENER_REMOVE(MouseListenerEvent, mouse_released_listeners);
}

void remove_mouse_moved_listener(CoiHandle input_listener_handle, MouseMovedListenerEvent event)
{
  HANDLE(OISListenerBind*, input_listener);
  LISTENER_REMOVE(MouseMovedListenerEvent, mouse_moved_listeners);
}

void default_key_mappings()
{
  key_map.clear();
  add_key_map("0", KC_0);
  add_key_map("1", KC_1);
  add_key_map("2", KC_2);
  add_key_map("3", KC_3);
  add_key_map("4", KC_4);
  add_key_map("5", KC_5);
  add_key_map("6", KC_6);
  add_key_map("7", KC_7);
  add_key_map("8", KC_8);
  add_key_map("9", KC_9);
  add_key_map("esc", KC_ESCAPE);
  add_key_map("-", KC_MINUS);          // - on main keyboard
  add_key_map("=", KC_EQUALS);
  add_key_map("backspace", KC_BACK);
  add_key_map("tab", KC_TAB);
  add_key_map("a", KC_A);
  add_key_map("b", KC_B);
  add_key_map("c", KC_C);
  add_key_map("d", KC_D);
  add_key_map("e", KC_E);
  add_key_map("f", KC_F);
  add_key_map("g", KC_G);
  add_key_map("h", KC_H);
  add_key_map("i", KC_I);
  add_key_map("j", KC_J);
  add_key_map("k", KC_K);
  add_key_map("l", KC_L);
  add_key_map("m", KC_M);
  add_key_map("n", KC_N);
  add_key_map("o", KC_O);
  add_key_map("p", KC_P);
  add_key_map("q", KC_Q);
  add_key_map("r", KC_R);
  add_key_map("s", KC_S);
  add_key_map("t", KC_T);
  add_key_map("u", KC_U);
  add_key_map("v", KC_V);
  add_key_map("w", KC_W);
  add_key_map("x", KC_X);
  add_key_map("y", KC_Y);
  add_key_map("z", KC_Z);
  add_key_map("mouse0", MB_Left);
  add_key_map("mouse1", MB_Right);
  add_key_map("mouse2", MB_Middle);
  add_key_map("mouse3", MB_Button3);
  add_key_map("[", KC_LBRACKET);
  add_key_map("]", KC_RBRACKET);
  add_key_map("return", KC_RETURN);
  add_key_map("lctrl", KC_LCONTROL);
  add_key_map(";", KC_SEMICOLON);
  add_key_map("'", KC_APOSTROPHE);
  add_key_map("`", KC_GRAVE);            // accent
  add_key_map("lshift", KC_LSHIFT);
  add_key_map("\\", KC_BACKSLASH);
  add_key_map(",", KC_COMMA);
  add_key_map(".", KC_PERIOD);           // . on main keyboard
  add_key_map("/", KC_SLASH);            // / on main keyboard
  add_key_map("rshift", KC_RSHIFT);
  add_key_map("*", KC_MULTIPLY);         // * on numeric keypad
  add_key_map("lalt", KC_LMENU);         // left Alt
  add_key_map("space", KC_SPACE);
  add_key_map("capslock", KC_CAPITAL);
  add_key_map("f1", KC_F1);
  add_key_map("f2", KC_F2);
  add_key_map("f3", KC_F3);
  add_key_map("f4", KC_F4);
  add_key_map("f5", KC_F5);
  add_key_map("f6", KC_F6);
  add_key_map("f7", KC_F7);
  add_key_map("f8", KC_F8);
  add_key_map("f9", KC_F9);
  add_key_map("f10", KC_F10);
  add_key_map("numlock", KC_NUMLOCK);
  add_key_map("scroll", KC_SCROLL);        // scroll lock
  add_key_map("num7", KC_NUMPAD7);
  add_key_map("num8", KC_NUMPAD8);
  add_key_map("num9", KC_NUMPAD9);
  add_key_map("num-", KC_SUBTRACT);        // - on numeric keypad
  add_key_map("num4", KC_NUMPAD4);
  add_key_map("num5", KC_NUMPAD5);
  add_key_map("num6", KC_NUMPAD6);
  add_key_map("num+", KC_ADD);             // + on numeric keypad
  add_key_map("num1", KC_NUMPAD1);
  add_key_map("num2", KC_NUMPAD2);
  add_key_map("num3", KC_NUMPAD3);
  add_key_map("renum0turn", KC_NUMPAD0);
  add_key_map("numdecimal", KC_DECIMAL);   // . on numeric keypad
  add_key_map("oem102", KC_OEM_102);       // < > | on UK/Germany keyboards
  add_key_map("f11", KC_F11);
  add_key_map("f12", KC_F12);
  add_key_map("f13", KC_F13);              // (NEC PC98)
  add_key_map("f14", KC_F14);              // (NEC PC98)
  add_key_map("f15", KC_F15);              // (NEC PC98)
  add_key_map("kana", KC_KANA);            // (Japanese keyboard)
  add_key_map("abntc1", KC_ABNT_C1);       // / ? on Portugese (Brazilian) keyboards
  add_key_map("convert", KC_CONVERT);      // (Japanese keyboard)
  add_key_map("noconvert", KC_NOCONVERT);  // (Japanese keyboard)
  add_key_map("yen", KC_YEN);              // (Japanese keyboard)
  add_key_map("abntc2", KC_ABNT_C2);       // Numpad . on Portugese (Brazilian) keyboards
  add_key_map("num=", KC_NUMPADEQUALS);    // = on numeric keypad (NEC PC98)
  add_key_map("prevtrack", KC_PREVTRACK);  // Previous Track (KC_CIRCUMFLEX on Japanese keyboard)
  add_key_map("at", KC_AT);                // (NEC PC98)
  add_key_map("colon", KC_COLON);          // (NEC PC98)
  add_key_map("underline", KC_UNDERLINE);  // (NEC PC98)
  add_key_map("kanji", KC_KANJI);          // (Japanese keyboard)
  add_key_map("stop", KC_STOP);            // (NEC PC98)
  add_key_map("ax", KC_AX);                // (Japan AX)
  add_key_map("unlabeled", KC_UNLABELED);  // (J3100)
  add_key_map("nexttrack", KC_NEXTTRACK);  // Next Track
  add_key_map("numenter", KC_NUMPADENTER); // Enter on numeric keypad
  add_key_map("rctrl", KC_RCONTROL);
  add_key_map("mute", KC_MUTE);            // Mute
  add_key_map("calc", KC_CALCULATOR);      // Calculator
  add_key_map("playpause", KC_PLAYPAUSE);  // Play / Pause
  add_key_map("mediastop", KC_MEDIASTOP);  // Media Stop
  add_key_map("volumedown", KC_VOLUMEDOWN);  // Volume -
  add_key_map("volumeup", KC_VOLUMEUP);      // Volume +
  add_key_map("webhome", KC_WEBHOME);        // Web home
  add_key_map("numcomma", KC_NUMPADCOMMA);   // , on numeric keypad (NEC PC98)
  add_key_map("numdivide", KC_DIVIDE);       // / on numeric keypad
  add_key_map("sysrq", KC_SYSRQ);
  add_key_map("ralt", KC_RMENU);             // right Alt
  add_key_map("pause", KC_PAUSE);            // Pause
  add_key_map("home", KC_HOME);              // Home on arrow keypad
  add_key_map("up", KC_UP);                  // UpArrow on arrow keypad
  add_key_map("pgup", KC_PGUP);              // PgUp on arrow keypad
  add_key_map("left", KC_LEFT);              // LeftArrow on arrow keypad
  add_key_map("right", KC_RIGHT);            // RightArrow on arrow keypad
  add_key_map("end", KC_END);                // End on arrow keypad
  add_key_map("down", KC_DOWN);              // DownArrow on arrow keypad
  add_key_map("pgdown", KC_PGDOWN);          // PgDn on arrow keypad
  add_key_map("insert", KC_INSERT);          // Insert on arrow keypad
  add_key_map("delete", KC_DELETE);          // Delete on arrow keypad
  add_key_map("lwin", KC_LWIN);              // Left Windows key
  add_key_map("rwin", KC_RWIN);              // Right Windows key
  add_key_map("apps", KC_APPS);              // AppMenu key
  add_key_map("power", KC_POWER);            // System Power
  add_key_map("sleep", KC_SLEEP);            // System Sleep
  add_key_map("wake", KC_WAKE);              // System Wake
  add_key_map("websearch", KC_WEBSEARCH);    // Web Search
  add_key_map("webfave", KC_WEBFAVORITES);   // Web Favorites
  add_key_map("webrefresh", KC_WEBREFRESH);  // Web Refresh
  add_key_map("webstop", KC_WEBSTOP);        // Web Stop
  add_key_map("webforward", KC_WEBFORWARD);  // Web Forward
  add_key_map("webback", KC_WEBBACK);        // Web Back
  add_key_map("mycomputer", KC_MYCOMPUTER);  // My Computer
  add_key_map("mail", KC_MAIL);              // Mail
  add_key_map("mediaselect", KC_MEDIASELECT);  // Media Select
}

void add_key_map(const char* key, int key_code)
{
  key_map.insert(std::pair<std::string, int>(std::string(key), key_code));
  key_reverse_map.insert(std::pair<int, std::string>(key_code, std::string(key)));
}

void remove_key_map(const char* key)
{
  key_reverse_map.erase(key_translate(key));
  key_map.erase(key);
}

int key_translate(const char* key)
{
  return key_map.find(key)->second;
}

const char* key_code_translate(int key)
{
  return key_reverse_map.find(key)->second.c_str();
}
