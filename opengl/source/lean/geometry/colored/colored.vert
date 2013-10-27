uniform   mat4   mvp_Matrix;


attribute vec4   aSite;
attribute vec4   aColor;


varying   vec4   vColor;



void main()
{
   gl_Position = mvp_Matrix * aSite;
//   gl_Position = aSite;
//   gl_Position = aSite * mvp_Matrix;
   vColor      = aColor;
}
