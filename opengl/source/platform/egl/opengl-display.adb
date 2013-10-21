with eGL.Binding,
     eGL.Pointers,

     System;


package body openGL.Display
is
   use eGL,
       eGL.Binding,
       eGL.Pointers;


   function Default return Item
   is
      use type System.Address,  eGL.EGLBoolean;

      the_Display : Display.item;
      Success     : EGLBoolean;
      Status      : EGLBoolean;

   begin
      the_Display.Thin := eglGetDisplay (Display_Pointer (EGL_DEFAULT_DISPLAY));

      if the_Display.Thin = egl_NO_DISPLAY then
         raise openGL.Error with "Failed to open the default Display with eGL.";
      end if;


      Success := eglInitialize (the_Display.Thin, the_Display.Version_major'Unchecked_Access,
                                                  the_Display.Version_minor'Unchecked_Access);
      if Success = egl_False then
         raise openGL.Error with "Failed to initialise eGL using the default Display.";
      end if;

      Status := eglBindAPI (EGL_OPENGL_ES_API);

      return the_Display;
   end Default;


end openGL.Display;


