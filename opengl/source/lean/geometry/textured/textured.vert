uniform   mat4                  mvp_Matrix;


attribute vec3   aSite;
attribute vec2   aCoords;


varying   vec4   vColor;
varying   vec2   vCoords;



const float   c_zero      = 0.0;
const float   c_one       = 1.0;




void main()
{
   gl_Position = mvp_Matrix * vec4 (aSite, 1.0);
   
   vColor  = vec4 (1.0, 1.0, 1.0, 1.0);
   vCoords = aCoords;
}
