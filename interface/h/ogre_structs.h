#pragma once

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
