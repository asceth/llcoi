#pragma once

// Detect platform
#if defined( WINCE )
#   if !defined( PLATFORM_WIN_CE )
#       define PLATFORM_WIN_CE
#   endif
#elif defined( WIN32 ) || defined( _WINDOWS )
#   if !defined( PLATFORM_WIN )
#       define PLATFORM_WIN
#   endif
#elif defined( __APPLE__ ) || defined( __APPLE_CC__ )
#   if !defined( PLATFORM_MAC )
#      define PLATFORM_MAC
#   endif
#else
#   if !defined( PLATFORM_LINUX )
#       define PLATFORM_LINUX
#   endif
#endif

#if defined(LLCOI_BUILD_DYNAMIC)
#   if defined( WIN32 ) || defined( _WINDOWS )
#       ifndef llcoi_EXPORTS
#           define DLL __declspec(dllimport)
#       else
#           define DLL extern "C" __declspec(dllexport)
#       endif
#   else
#       ifndef llcoi_EXPORTS
#           define DLL
#       else
#           if defined( __GNUC__ ) && __GNUC__ >= 4
#               define DLL extern "C" __attribute__ ((visibility("default")))
#           else
#               define DLL extern "C"
#           endif
#       endif
#   endif
#else
#   if defined( LLCOI_BUILD_STATIC )
#       if defined( __GNUC__ ) && __GNUC__ >= 4
#           define DLL extern "C" __attribute__ ((visibility("default")))
#       else
#           define DLL extern "C"
#       endif
#   else
#       define DLL
#   endif
#endif

//defines

#define coiReal float

#define EVENT_FRAME_STARTED 1
#define EVENT_FRAME_RENDERING_QUEUED 2
#define EVENT_FRAME_ENDED 4

#define COI_DECLARE_HANDLE(name) struct name##__ { int unused; }; typedef struct name##__ *name

// COI_DECLARE_HANDLE(CameraHandle);
// COI_DECLARE_HANDLE(EntityHandle);
// COI_DECLARE_HANDLE(SceneNodeHandle);
// COI_DECLARE_HANDLE(LightHandle);
// COI_DECLARE_HANDLE(RenderWindowHandle);
// COI_DECLARE_HANDLE(RootHandle);
// COI_DECLARE_HANDLE(RenderSystemHandle);
// COI_DECLARE_HANDLE(SceneManagerHandle);
// COI_DECLARE_HANDLE(ViewportHandle);

#define CoiHandle void*
// don't see why we need these
// since functions should be self explanatory in argument naming
// and/or the function name itself
// #define CameraCoiHandle void*
// #define MovableObjectCoiHandle void*
// #define EntityCoiHandle void*
// #define SceneNodeCoiHandle void*
// #define LightCoiHandle void*
// #define RenderWindowCoiHandle void*
// #define RootCoiHandle void*
// #define RenderSystemCoiHandle void*
// #define SceneManagerCoiHandle void*
// #define ViewportCoiHandle void*
// #define PlaneCoiHandle void*
// #define MeshManagerCoiHandle void*
// #define MeshPtrCoiHandle void*


// listener typedefs
typedef int(*FrameListenerEvent)(float,float,int);
typedef void(*WindowListenerEvent)(CoiHandle);

typedef struct
{
  float w;
  float x;
  float y;
  float z;
} coiQuaternion;

typedef struct
{
  float x;
  float y;
  float z;
} coiVector3;

typedef struct
{
  float r;
  float g;
  float b;
  float a;
} coiColourValue;

typedef struct
{
  const char* renderer_s;
  const char* plugin_folder_s;
  const char* window_title;
  const char* log_name;
  int width, height, auto_window;
} engine_options;

