/******************************************************************************
 * Ogre::Root interface definition
 ******************************************************************************/

#pragma once

// Root functions
DLL void release_engine();

DLL void default_engine_options(engine_options* options);

DLL void init_engine(const engine_options options);

DLL CoiHandle create_root(const char* pluginFileName, const char* configFileName, const char* logFileName);

DLL CoiHandle root_initialise(int auto_create_window, const char* render_window_title);

DLL int root_is_initialised();

DLL void save_config();

DLL int restore_config();

DLL int show_config_dialog();

DLL void load_ogre_plugin(const char * plugin);

DLL int render_one_frame();

DLL int render_one_frame_ex(float time_since_last_frame);

DLL int render_loop_once();

DLL void render_loop();

DLL void pump_messages();

DLL void log_message(const char* message);

DLL void set_default_num_mipmaps(int number);
