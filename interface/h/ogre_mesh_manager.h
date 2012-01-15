/******************************************************************************
 * Ogre::MeshManager interface definition
 ******************************************************************************/

#pragma once

DLL CoiHandle get_mesh_manager();

DLL void  mesh_manager_create_plane(const char* name,
                                    const char* group_name,
                                    CoiHandle plane_handle,
                                    float width,
                                    float height,
                                    int xsegments,
                                    int ysegments,
                                    int normals,
                                    unsigned short num_tex_coord_sets,
                                    float u_tile,
                                    float v_tile,
                                    float upvector_x,
                                    float upvector_y,
                                    float upvector_z);
