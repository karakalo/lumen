package body openGL.Program.textured
is

   overriding
   procedure set_Uniforms (Self : in Item)
   is
   begin
      Self.set_mvp_Uniform;
   end set_Uniforms;

end openGL.Program.textured;
