with
     ada.Strings.fixed;


package body openGL.Program.lit_textured_skinned
is

   overriding
   procedure define  (Self : in out Item;   use_vertex_Shader   : in opengl.Shader.view;
                                            use_fragment_Shader : in opengl.Shader.view)
   is
      use ada.Strings,
          ada.Strings.fixed;
   begin
      openGL.Program.item (Self).define (use_vertex_Shader,
                                         use_fragment_Shader);   -- define base class

      for Each in Self.bone_transform_Uniforms'Range
      loop
         Self.bone_transform_Uniforms (Each).define (Self'Access,
                                                     "bone_Matrices[" & Trim (Integer'Image (Each - 1), Left) & "]");
      end loop;
   end define;



   overriding
   procedure set_Uniforms (Self : in Item)
   is
      the_inverse_modelview_matrix_Uniform : constant opengl.Variable.uniform.mat3
        := Self.uniform_Variable ("inv_modelview_Matrix");

      the_light_direction_Uniform          : constant opengl.Variable.uniform.vec3
        := Self.uniform_Variable ("uLight.direction");
      the_light_halfplane_Uniform          : constant opengl.Variable.uniform.vec3
        := Self.uniform_Variable ("uLight.halfplane");

      the_light_ambient_color_Uniform      : constant opengl.Variable.uniform.vec4
        := Self.uniform_Variable ("uLight.ambient_color");
      the_light_diffuse_color_Uniform      : constant opengl.Variable.uniform.vec4
        := Self.uniform_Variable ("uLight.diffuse_color");
      the_light_specular_color_Uniform     : constant opengl.Variable.uniform.vec4
        := Self.uniform_Variable ("uLight.specular_color");

      the_Light : openGL.Light.directional.item renames Self.directional_Light;
   begin
      Self.set_mvp_Uniform;

      the_inverse_modelview_matrix_Uniform.Value_is (Self.inverse_modelview_Matrix);

      the_light_direction_Uniform.Value_is (the_Light.Direction);
      the_light_halfplane_Uniform.Value_is (the_Light.halfplane_Vector);

      the_light_ambient_color_Uniform .Value_is (the_Light.ambient_Color);
      the_light_diffuse_color_Uniform .Value_is (the_Light.diffuse_Color);
      the_light_specular_color_Uniform.Value_is (the_Light.specular_Color);

      declare
         the_sampler_Uniform : constant opengl.Variable.uniform.int := Self.uniform_Variable ("sTexture");
      begin
         the_sampler_Uniform.Value_is (0);
      end;
   end set_Uniforms;



   procedure bone_Transform_is (Self : in Item;   Which : in Integer;
                                                  Now   : in Matrix_4x4)
   is
   begin
      Self.bone_transform_Uniforms (Which).Value_is (Now);
   end bone_Transform_is;


end openGL.Program.lit_textured_skinned;
