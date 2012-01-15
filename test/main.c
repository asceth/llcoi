#include <ogre_interface.h>
#include <ois_interface.h>

#undef LLCOI_TEST_USE_OPENINPUT

#if defined(LLCOI_TEST_USE_OPENINPUT)
//#   include <openinput.h>
#endif

#if defined(LLCOI_TEST_USE_ALLEGRO)
#   include <allegro.h>
#endif

#include <math.h>
#include <stdio.h>

#if defined( WIN32 ) || defined( _WINDOWS )
#   define WIN32_LEAN_AND_MEAN
#   include "windows.h"
#else
#if defined(LLCOI_TEST_USE_OPENINPUT)
//#include <X11/Xlib.h>
#endif
#endif

CoiHandle myCamera;
CoiHandle keyboard;
CoiHandle mouse;
float tiny_timer=0;

void window_event_listener_test(CoiHandle window_handle)
{
	log_message("I was called when the window closed!");
}

int frame_listener_test(float evt_time,float frame_time,int event_type)
{
  tiny_timer+=frame_time;
  camera_set_position(myCamera,
                      cos(tiny_timer / 10) * 1700,
                      2000,
                      sin(tiny_timer / 10) * 2100);
  return 1;
}

#if defined(LLCOI_TEST_USE_OPENINPUT) && defined(PLATFORM_WIN)
INT WINAPI WinMain( HINSTANCE hInst, HINSTANCE hPrevInst, LPSTR strCmdLine, INT nCmdShow)
#else
  int main(int argc, char *argv[])
#endif
{
  /* C90 requires all vars to be declared at top of function */

  CoiHandle entity;
  CoiHandle node;
  CoiHandle light;
  CoiHandle rendersystem;
  CoiHandle renderwindow;
  CoiHandle viewport;

  CoiHandle plane;
  CoiHandle plane_entity;
  CoiHandle plane_node;

  CoiHandle terrain_group;
  CoiHandle terrain_iterator;
  CoiHandle import_data;
  CoiHandle image;

  float direction[3];
  float colour[4];

#if defined(LLCOI_TEST_USE_OPENINPUT)
  // Openinput
  oi_event evt;
  char openinput_window_params[100];
  unsigned int windowHnd = 0;

#if defined(PLATFORM_LINUX)
  Display *disp;
  Window win;
  unsigned int scrn;
#endif
#endif //LLCOI_TEST_USE_OPENINPUT

  int keep_going = 1;
  long loop_x = 0;
  long loop_y = 0;

  // setup
  create_root("plugins.cfg", "ogre.cfg", "ogre.log");

  if (!(restore_config() || show_config_dialog()))
    {
      return 1;
    }

	setup_resources("resources.cfg");

  renderwindow = root_initialise(1, "Ogre Renderwindow");

  set_default_num_mipmaps(5);

  initialise_all_resource_groups();

  create_scene_manager("OctreeSceneManager", "The SceneManager");

  myCamera = create_camera(get_scene_manager(), "mycam");

  camera_set_position(myCamera, 1683, 50, 2116);

  camera_lookat(myCamera, 1963, -50, 1660);

  camera_set_near_clip_distance(myCamera, 1);
  camera_set_far_clip_distance(myCamera, 50000);

  viewport = add_viewport(myCamera);

  viewport_set_background_colour(viewport, 0, 0, 0);

  camera_set_aspect_ratio(myCamera, 800, 600);


  // entities

  plane = create_plane_from_normal(0, 1, 0, 0);
  mesh_manager_create_plane("ground", "General", plane, 1500, 1500, 20, 20, 1, 1, 5, 5, 0, 0, 1);
  plane_entity = create_entity(get_scene_manager(), "plane", "ground");
  entity_set_material_name(plane_entity, "Dev/Red");
  plane_node = create_child_scene_node(get_scene_manager(), "planenode");
  scene_node_attach_entity(plane_node, plane_entity);

  entity = create_entity(get_scene_manager(), "OgreHead", "ogrehead.mesh");

  node = create_child_scene_node(get_scene_manager(), "headNode");

  scene_node_attach_entity(node, entity);

  scene_manager_set_ambient_light_rgb(get_scene_manager(), 0.5f, 0.5f, 0.5f);

  light = create_light(get_scene_manager(), "mainLight");

  light_set_position(light, 20, 80, 50);


  // terrain
  create_terrain_global_options();
  terrain_group = create_terrain_group(get_scene_manager(), ALIGN_X_Z, 513, 12000.0f);
  terrain_group_set_filename_convention(terrain_group, "BasicTutorial3Terrain", "dat");
  terrain_group_set_origin(terrain_group, 0, 0, 0);

  // terrain defaults
  terrain_global_options_set_max_pixel_error(8);
  terrain_global_options_set_composite_map_distance(3000);

  terrain_global_options_set_light_map_direction_vector3(light_get_derived_direction(light));

  terrain_global_options_set_composite_map_ambient_colour(scene_manager_get_ambient_light(get_scene_manager()));// sm

  terrain_global_options_set_composite_map_diffuse_colour(light_get_diffuse_colour(light));// light

  import_data = terrain_group_get_default_import_settings(terrain_group);
  terrain_group_import_data_set_terrain_size(import_data, 513);
  terrain_group_import_data_set_world_size(import_data, 12000.0f);
  terrain_group_import_data_set_input_scale(import_data, 600);
  terrain_group_import_data_set_min_batch_size(import_data, 33);
  terrain_group_import_data_set_max_batch_size(import_data, 65);

  terrain_group_import_data_resize_layers(import_data, 3);
  terrain_group_import_data_set_layer(import_data, 0, 100, "nvidia/dirt_grayrocky_diffusespecular.dds", "nvidia/dirt_grayrocky_normalheight.dds");
  terrain_group_import_data_set_layer(import_data, 1, 30, "nvidia/grass_green-01_diffusespecular.dds", "nvidia/grass_green-01_normalheight.dds");
  terrain_group_import_data_set_layer(import_data, 2, 200, "nvidia/growth_weirdfungus-03_diffusespecular.dds", "nvidia/growth_weirdfungus-03_normalheight.dds");


  // define terrains
  for (loop_x; loop_x <= 0; ++loop_x)
    {
      for (loop_y; loop_y <= 0; ++loop_y)
        {
          if (resource_exists(terrain_group_get_resource_group(terrain_group), terrain_group_generate_filename(terrain_group, loop_x, loop_y)))
            {
              terrain_group_define_terrain(terrain_group, loop_x, loop_y);
            }
          else
            {
              image = create_image();
              image_load(image, "terrain.png");
              if (loop_x % 2 != 0)
                {
                  image_flip_around_y(image);
                }
              if (loop_y % 2 != 0)
                {
                  image_flip_around_x(image);
                }

              terrain_group_define_terrain_image(terrain_group, loop_x, loop_y, image);
              image_delete(image);
              image = NULL;
            }

        }
    }


  terrain_group_load_all_terrains(terrain_group, 1);

  // blend maps
  terrain_iterator = terrain_group_get_terrain_iterator(terrain_group);
  while(terrain_iterator_has_more_elements(terrain_iterator))
    {
      CoiHandle terrain = terrain_iterator_get_next(terrain_iterator);

      int blend_map_size = terrain_get_layer_blend_map_size(terrain);
      CoiHandle blend_map_0 = terrain_get_layer_blend_map(terrain, 1);
      CoiHandle blend_map_1 = terrain_get_layer_blend_map(terrain, 2);

      float min_height_0 = 70;
      float fade_dist_0 = 40;
      float min_height_1 = 70;
      float fade_dist_1 = 15;

      /*
      float* blend_1 = terrain_layer_blend_map_get_blend_pointer(blend_map_1);

      for(unsigned int y = 0; y < blend_map_size; ++y)
        {
          for(unsigned int x = 0; x < blend_map_size; ++x)
            {
              float tx, ty;
              terrain_layer_blend_map_convert_image_to_terrain_space(x, y, &tx, &ty);
              float height = terrain_get_height_at_terrain_position(tx, ty);
              float val = (height - min_height_0) / fade_dist_0;
              val = math_clamp_f(val, 0.0f, 1.0f);
              val = (height - min_height_1) / fade_dist_1;
              val = math_clamp_f(val, 0.0f, 1.0f);

              *blend_1++ = val;
            }
        }

      terrain_layer_blend_map_dirty(blend_map_0);
      terrain_layer_blend_map_dirty(blend_map_1);
      terrain_layer_blend_map_update(blend_map_0);
      terrain_layer_blend_map_update(blend_map_1);
      */
    }

  terrain_group_free_temporary_resources(terrain_group);
  // listeners

  add_frame_listener(frame_listener_test,EVENT_FRAME_RENDERING_QUEUED|EVENT_FRAME_STARTED);

	add_window_listener(renderwindow, window_event_listener_test);

  create_input_system(render_window_get_hwnd(renderwindow));
  keyboard = create_keyboard_object(0);
  mouse = create_mouse_object(0);

  while(keep_going)
    {
      keyboard_capture(keyboard);
      mouse_capture(mouse);

      if(keyboard_is_key_down(keyboard, KC_ESCAPE))
        keep_going = 0;

      // Pump window messages for nice behaviour
      pump_messages();
      // Render a frame
      render_one_frame();

      if (render_window_closed(renderwindow))
        {
          keep_going = 0;
        }

    }

#if defined(LLCOI_TEST_USE_OPENINPUT)
  windowHnd = render_window_get_hwnd(renderwindow);

#if defined(PLATFORM_LINUX)
  disp = XOpenDisplay( NULL );
  scrn = DefaultScreen(disp);
  sprintf(openinput_window_params, "c:%u s:%u w:%u", (unsigned int)disp, (unsigned int)scrn, windowHnd);
#else
  sprintf(openinput_window_params, "c:%u s:%u w:%u", 0, 0, windowHnd);
#endif

  oi_init(openinput_window_params, 0);

  log_message("***************************");
  log_message("*** All Systems online! ***");
  log_message("***************************");

  //render_loop();
  while (keep_going)
    {
      // ask oi to wait for events
      oi_events_poll(&evt);
      switch(evt.type)
        {
        case OI_QUIT:
          // Quit
          keep_going = 0;
          break;

        case OI_KEYDOWN:
          // Keyboard button down
          //WTF?? Better way to check this, please..
          if(evt.key.keysym.sym == OIK_ESC)
            keep_going = 0;
          break;

        default:
          break;
        }
      // Pump window messages for nice behaviour
      pump_messages();
      // Render a frame
      render_one_frame();

      if (render_window_closed())
        {
          keep_going = 0;
        }
    }

  oi_close();
#endif //LLCOI_TEST_USE_OPENINPUT

	remove_window_listener(renderwindow);

  destroy_keyboard_object(keyboard);
  destroy_mouse_object(mouse);
  destroy_input_system();
  release_engine();

  return 0;
}
