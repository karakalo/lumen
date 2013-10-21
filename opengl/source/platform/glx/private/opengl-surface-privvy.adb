package body opengl.Surface.privvy
is

   function to_glx (Self : in Surface.item'Class) return glx.GLXDrawable
   is
   begin
      return Self.glx_Surface;
   end to_glx;

end opengl.Surface.privvy;
