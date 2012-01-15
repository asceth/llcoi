// Ogre::ManualObject interface definition

#pragma once

DLL CoiHandle create_manual_object(CoiHandle scene_manager_handle, const char* name);

DLL void manual_object_delete(CoiHandle manual_object_handle);

DLL void manual_object_clear(CoiHandle manual_object_handle);

DLL void manual_object_estimate_vertex_count(CoiHandle manual_object_handle, int vcount);

DLL void manual_object_estimate_index_count(CoiHandle manual_object_handle, int icount);

DLL void manual_object_begin(CoiHandle manual_object_handle, const char* material_name, const int render_operation, const char* resource_group_name);

DLL void manual_object_set_dynamic(CoiHandle manual_object_handle, const int dyn);

DLL int manual_object_get_dynamic(CoiHandle manual_object_handle);

DLL void manual_object_begin_update(CoiHandle manual_object_handle, const int section_index);

DLL void manual_object_position(CoiHandle manual_object_handle, float x, float y, float z);

DLL void manual_object_normal(CoiHandle manual_object_handle, float x, float y, float z);

DLL void manual_object_tangent(CoiHandle manual_object_handle, float x, float y, float z);

DLL void manual_object_texture_coord_u(CoiHandle manual_object_handle, float u);

DLL void manual_object_texture_coord_uv(CoiHandle manual_object_handle, float u, float v);

DLL void manual_object_texture_coord_uvw(CoiHandle manual_object_handle, float u, float v, float w);

DLL void manual_object_texture_coord_xyzw(CoiHandle manual_object_handle, float x, float y, float z, float w);

DLL void manual_object_colour(CoiHandle manual_object_handle, float r, float g, float b, float a);

DLL void manual_object_index(CoiHandle manual_object_handle, const unsigned int idx);

DLL void manual_object_triangle(CoiHandle manual_object_handle, const unsigned int i1, const unsigned int i2, const unsigned int i3);

DLL void manual_object_quad(CoiHandle manual_object_handle, const unsigned int i1, const unsigned int i2, const unsigned int i3, const unsigned int i4);

DLL CoiHandle manual_object_end(CoiHandle manual_object_handle);

DLL void manual_object_set_material_name(CoiHandle manual_object_handle, const int sub_index, const char* name, const char* resource_group_name);
