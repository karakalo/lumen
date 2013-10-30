package openGL.Program.lit_textured_skinned
--
--  Provides a program for lit, textured, skinned GL vertices.
--
is

   type Item is new opengl.Program.item with private;
   type View is access all Item'Class;


   overriding
   procedure define  (Self : in out Item;   use_vertex_Shader   : in opengl.Shader.view;
                                            use_fragment_Shader : in opengl.Shader.view);
   overriding
   procedure set_Uniforms      (Self : in Item);

   procedure bone_Transform_is (Self : in Item;   Which : in Integer;
                                                  Now   : in Matrix_4x4);


private

   type bone_transform_Uniforms is array (1 .. 120) of opengl.Variable.uniform.mat4;

   type Item is new opengl.Program.item with
      record
         bone_transform_Uniforms : lit_textured_skinned.bone_transform_Uniforms;
      end record;

end openGL.Program.lit_textured_skinned;
