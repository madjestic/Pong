#version 430 core

layout(location = 0) in vec3 vPosition; // vertex position
layout(location = 1) in vec2 uvCoords;  // uv coordinate

uniform float fPPos;
uniform mat4  transform;

// Output data ; will be interpolated for each fragment.
out vec2  uv;
out float ppos;

void main()
{
   gl_Position = transform * vec4(vPosition, 1.0);
	 uv   = uvCoords;
}
