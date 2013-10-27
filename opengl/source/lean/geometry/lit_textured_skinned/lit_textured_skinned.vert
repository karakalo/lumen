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

uniform   mat4                  bone_Matrices[120];


attribute vec3   aSite;
attribute vec3   aNormal;
attribute vec4   aColor;
attribute vec2   aCoords;

attribute vec4   bone_Ids;
attribute vec4   bone_Weights;



varying   vec4   vColor;
varying   vec2   vCoords;



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
    vec4   transformedPosition = vec4 (0.0);
    vec3   transformedNormal   = vec3 (0.0);

    // bone 1
    //
        mat4   m44 = bone_Matrices[int(bone_Ids.x)-1];
        
        // transform the offset by bone 1
        transformedPosition += m44 * vec4 (aSite, c_one) * bone_Weights.x;

        mat3   m33 = mat3(m44[0].xyz,
                          m44[1].xyz,
                          m44[2].xyz);

        // transform normal by bone 1
        transformedNormal += m33 * aNormal * bone_Weights.x;


    // bone 2
    //
        m44 = bone_Matrices[int(bone_Ids.y)-1];
        
        // transform the offset by bone 2
        transformedPosition += m44 * vec4 (aSite, c_one) * bone_Weights.y;

        m33 = mat3(m44[0].xyz,
                   m44[1].xyz,
                   m44[2].xyz);

        // transform normal by bone 2
        transformedNormal += m33 * aNormal * bone_Weights.y;


    // bone 3
    //
        m44 = bone_Matrices[int(bone_Ids.z)-1];
        
        // transform the offset by bone 3
        transformedPosition += m44 * vec4 (aSite, c_one) * bone_Weights.z;

        m33 = mat3(m44[0].xyz,
                   m44[1].xyz,
                   m44[2].xyz);

        // transform normal by bone 3
        transformedNormal += m33 * aNormal * bone_Weights.z;


    // bone 4
    //
        m44 = bone_Matrices[int(bone_Ids.w)-1];
        
        // transform the offset by bone 4
        transformedPosition += m44 * vec4 (aSite, c_one) * bone_Weights.w;

        m33 = mat3(m44[0].xyz,
                   m44[1].xyz,
                   m44[2].xyz);

        // transform normal by bone 4
        transformedNormal += m33 * aNormal * bone_Weights.w;




    gl_Position       = mvp_Matrix * transformedPosition;
    
    transformedNormal = normalize (transformedNormal);
    vColor            = directional_light_color (inv_modelview_Matrix * transformedNormal);
    
    vCoords           = aCoords;
}
