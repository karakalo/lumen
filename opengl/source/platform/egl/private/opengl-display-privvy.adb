package body opengl.Display.privvy
is

   function to_eGL (Self : in Display.item'Class) return eGL.EGLDisplay
   is
   begin
      return Self.Thin;
   end to_eGL;

end opengl.Display.privvy;


