package openGL.Program.textured
--
--  Provides a program for textured GL vertices.
--
is

   type Item is new opengl.Program.item with null record;
   type View is access all Item'Class;

   overriding
   procedure set_Uniforms (Self : in Item);

end openGL.Program.textured;
