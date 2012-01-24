// Ogre::Math interface definition

#pragma once

// math
DLL float math_clamp_f(float value, float min, float max);

DLL void multiply_quaternion_with_quaternion(CoiHandle quat1_handle, CoiHandle quat2_handle, CoiHandle quaternion_output_handle);

DLL void multiply_quaternion_with_vector3(CoiHandle quat_handle, CoiHandle vector_handle, CoiHandle vector_output_handle);

DLL void multiply_vector3_with_vector3(CoiHandle vector1_handle, CoiHandle vector2_handle, CoiHandle vector_output_handle);

