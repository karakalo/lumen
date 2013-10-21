package body opengl.surface_Profile.privvy
is

   function to_eGL (Self : in Item'Class) return egl.EGLConfig
   is
   begin
      return Self.egl_Config;
   end to_eGL;

end opengl.surface_Profile.privvy;
