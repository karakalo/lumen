package body opengl.Surface.privvy
is

   function to_eGL (Self : in Surface.item'Class) return egl.EGLSurface
   is
   begin
      return Self.egl_Surface;
   end to_eGL;

end opengl.Surface.privvy;
