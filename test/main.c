#include <ogre_interface.h>

#include <allegro.h>
#include <allegro_opengl.h>
#include <allegro_windows.h>
#include <math.h>

#if defined( WIN32 ) || defined( _WINDOWS )
#   define WIN32_LEAN_AND_MEAN
#   include "windows.h"
#endif

CameraHandle myCamera;

float tiny_timer=0;

int frame_listener_test(float evt_time,float frame_time,int event_type)
{
	tiny_timer+=frame_time;
	camera_set_position(myCamera,cos(tiny_timer)*100,50,sin(tiny_timer)*100);
	camera_lookat(myCamera,0,0,0);
	return 1;
}

int main(int argc, char *argv[])
{
	/* C90 requires all vars to be declared at top of function */

	CameraHandle anotherHandle;
	EntityHandle entity;
	SceneNodeHandle node;
	LightHandle light;
	RenderSystemHandle rendersystem;
	RenderWindowHandle renderwindow;
	ViewportHandle viewport;
	HWND hwnd = NULL;
	int keep_going = 1;

	ALLEGRO_DISPLAY *display;
	//ALLEGRO_EVENT_QUEUE *event_queue;
	//ALLEGRO_EVENT event;
	ALLEGRO_KEYBOARD_STATE kbdstate;

	if (!al_init()) {
		return 1;
	}
	if (!al_install_keyboard()) {
		return 1;
	}
	if (!al_install_mouse()) {
		return 1;
	}

	//al_set_new_display_flags(ALLEGRO_OPENGL | ALLEGRO_RESIZABLE);
	display = al_create_display(800, 600);
	if (!display) {
		return 1;
	}
	al_set_window_title(display, "My window");

	al_hide_mouse_cursor(display);

	create_root("plugins.cfg", "ogre.cfg", "ogre.log");

	/*    load_ogre_plugin("RenderSystem_GL");

	rendersystem = get_render_system_by_name("OpenGL Rendering Subsystem");

	render_system_set_config_option(rendersystem, "Full Screen", "No");
	render_system_set_config_option(rendersystem, "VSync", "No");
	render_system_set_config_option(rendersystem, "Video Mode", "800 x 600 @ 32-bit");

	set_render_system(rendersystem);

	load_ogre_plugin("Plugin_OctreeSceneManager");*/

	if(!(restore_config() || show_config_dialog()))
	{
		return 1;
	}

	setup_resources("resources.cfg");

	root_initialise(0, "");

	hwnd = al_get_win_window_handle(display);

	renderwindow = create_render_window_hwnd("The RenderWindow", al_get_display_width(display), al_get_display_height(display), 0, hwnd);

	set_default_num_mipmaps(5);

	initialise_all_resourcegroups();

	create_scene_manager("OctreeSceneManager", "The SceneManager");

	myCamera = create_camera("mycam");

	camera_set_position(myCamera, 0, 0, 80);

	camera_lookat(myCamera, 0, 0, -300);

	camera_set_near_clip_distance(myCamera, 5);

	viewport = add_viewport(myCamera);

	viewport_set_background_colour(viewport, 0, 0, 0);

	camera_set_aspect_ratio(myCamera, viewport_get_width(viewport), viewport_get_height(viewport));

	entity = create_entity("OgreHead", "ogrehead.mesh");

	node = create_child_scenenode("headNode");

	attach_entity_to_scenenode(entity, node);

	set_ambient_light_rgb(0.5f, 0.5f, 0.5f);

	light = create_light("mainLight");

	light_set_position(light, 20, 80, 50);

	add_frame_listener(frame_listener_test,EVENT_FRAME_RENDERING_QUEUED|EVENT_FRAME_STARTED);

	//render_loop();

	//event_queue = al_create_event_queue();
	//if (!event_queue) {
	//   return 1;
	//}

	//al_register_event_source(event_queue, al_get_keyboard_event_source());
	//al_register_event_source(event_queue, al_get_display_event_source(display));

	while (keep_going) {
		/* Take the next event out of the event queue, and store it in `event'. */
		//al_wait_for_event(event_queue, &event);

		/* Check what type of event we got and act accordingly.  ALLEGRO_EVENT
		* is a union type and interpretation of its contents is dependent on
		* the event type, which is given by the 'type' field.
		*
		* Each event also comes from an event source and has a timestamp.
		* These are accessible through the 'any.source' and 'any.timestamp'
		* fields respectively, e.g. 'event.any.timestamp'
		*/
		al_get_keyboard_state(&kbdstate);
		if (al_key_down(&kbdstate, ALLEGRO_KEY_ESCAPE)) {
			keep_going = 0;
		}
		//   switch (event.type) {

		//      /* ALLEGRO_EVENT_KEY_DOWN - a keyboard key was pressed.
		//       */
		//      case ALLEGRO_EVENT_KEY_DOWN:
		//         if (event.keyboard.keycode == ALLEGRO_KEY_ESCAPE) {
		//            keep_going = 0;
		//         }
		//         break;

		//      /* ALLEGRO_EVENT_DISPLAY_CLOSE - the window close button was pressed.
		//       */
		//      case ALLEGRO_EVENT_DISPLAY_CLOSE:
		//         keep_going = 0;

		//      /* We received an event of some type we don't know about.
		//       * Just ignore it.
		//       */
		//      default:
		//         break;
		//   }
		pump_messages();
		render_one_frame();
	}

	release_engine();

	al_uninstall_system();

	return 0;
}
