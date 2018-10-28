#version 450

// Input Data
in vec2 uv;
uniform float fPPos;
uniform vec2  vBPos;

uniform vec2  u_resolution;
uniform float u_time;

// Ouput Data
out vec4 fColor;

float stroke(float x, float s, float w)
{
  float d =
    step(s, x + w*.5) -
    step(s, x - w*.5);
  return clamp(d, 0., 1.);
}

void main(void)
{
  vec3 color = vec3(0.);
  vec3 cnv   = vec3(0.);
  vec2 pst   = gl_FragCoord.xy/u_resolution;

  // Paddle
  color     += vec3( stroke(pst.x - fPPos.x, .5, .2)
                   , stroke(pst.x - fPPos.x, .5, .2)
                   , stroke(pst.x - fPPos.x, .5, .2)) *
               vec3( stroke(pst.y, .05, .05)
                   , stroke(pst.y, .05, .05)
                   , stroke(pst.y, .05, .05));
  // Ball
  color     += vec3( stroke(pst.x - vBPos.x, .50, .04)
                   , stroke(pst.x - vBPos.x, .50, .04)
                   , stroke(pst.x - vBPos.x, .50, .04)) *
               vec3( stroke(pst.y, vBPos.y, .05)
                   , stroke(pst.y, vBPos.y, .05)
                   , stroke(pst.y, vBPos.y, .05));
  // color     += .5 * cnv;
  fColor     = vec4(color, 1.0);
}
