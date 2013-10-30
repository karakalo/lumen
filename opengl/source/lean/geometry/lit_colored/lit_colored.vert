struct directional_light
{
   vec3   direction;        // Normalized light direction in eye space.
   vec3   halfplane;        // Normalized half-plane vector.
   vec4   ambient_color;     
   vec4   diffuse_color;
   vec4   specular_color;
};


uniform   mat4                  mvp_Matrix;
uniform   mat3                  inv_modelview_Matrix;
uniform   directional_light     uLight;
uniform   vec3                  uScale;


attribute vec3   aSite;
attribute vec3   aNormal;
attribute vec4   aColor;


varying   vec4   vColor;



const float   c_zero      = 0.0;
const float   c_one       = 1.0;
const float   c_shininess = 200.0;



vec4                                      // Returns the computed color.
directional_light_color (vec3   normal)   // 'normal' has been transformed into eye space and normalized.
{
   vec4    computed_color = vec4 (c_zero, c_zero, c_zero, c_zero);
   float   NdotL;                         // Dot product of normal and light direction.
   float   NdotH;                         // Dot product of normal and half-plane vector.

   NdotL = max (c_zero,  dot (normal, uLight.direction));
   NdotH = max (c_zero,  dot (normal, uLight.halfplane));

   computed_color += (        uLight.ambient_color * aColor);
   computed_color += (NdotL * uLight.diffuse_color * aColor);
   
   if (NdotH > c_zero)
      computed_color += (pow (NdotH, c_shininess) * aColor * uLight.specular_color);

   return computed_color;
}




void main()
{ 
   vec3    a = aSite;     
   
   a.x = a.x * uScale.x;     
   a.y = a.y * uScale.y;     
   a.z = a.z * uScale.z;     
   
   gl_Position = mvp_Matrix * vec4 (a, 1.0); 
   
  
   vColor  = directional_light_color (inv_modelview_Matrix * normalize (aNormal * uScale));
}
