package body openGL.Program.lit_colored
is

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

      the_scale_Uniform                    : constant opengl.Variable.uniform.vec3
        := Self.uniform_Variable ("uScale");

      the_Light                            : openGL.Light.directional.item renames Self.directional_Light;
   begin
      Self.set_mvp_Uniform;

      the_inverse_modelview_matrix_Uniform
                                      .Value_is (Self.inverse_modelview_Matrix);

      the_light_direction_Uniform     .Value_is (the_Light.Direction);
      the_light_halfplane_Uniform     .Value_is (the_Light.halfplane_Vector);

      the_light_ambient_color_Uniform .Value_is (the_Light.ambient_Color);
      the_light_diffuse_color_Uniform .Value_is (the_Light.diffuse_Color);
      the_light_specular_color_Uniform.Value_is (the_Light.specular_Color);

      the_scale_Uniform               .Value_is (Self.Scale);
   end set_Uniforms;


end openGL.Program.lit_colored;
