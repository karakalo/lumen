package openGL.Program.colored
--
--  Provides a program for colored GL vertices.
--
is

   type Item is new opengl.Program.item with null record;
   type View is access all Item'Class;

   overriding
   procedure set_Uniforms (Self : in Item);

end openGL.Program.colored;
