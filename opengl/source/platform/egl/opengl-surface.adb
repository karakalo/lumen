with
     opengl.surface_Profile.privvy,
     opengl.Display        .privvy,

     eGL.Binding,
     interfaces.c.Strings,
     System;


package body opengl.Surface
is
   use eGL.Binding;


   --  Forge
   --
   procedure define (Self : in out Item;   surface_Profile : in opengl.surface_Profile.item'Class;
                                           Display         : in opengl.Display.Item;
                                           Window_Id       : in Natural)
   is
      use opengl.Display        .privvy,
          opengl.surface_Profile.privvy,
          System;
   begin
      Self.egl_Surface := eglCreateWindowSurface (to_eGL (Display),
                                                  to_eGL (surface_Profile),
                                                  egl.NativeWindowType (Window_Id),
                                                  null); -- const EGLint *attribList);

      if self.egl_Surface = EGL_NO_SURFACE then
         raise opengl.Error with "unable to create an EGL surface for a window";
      end if;

      Self.Display := Display;
   end define;




   --  Operations
   --
   procedure swap_Buffers (Self : in Item)
   is
      use      openGL.Display.privvy,
               eGL;
      use type EGLBoolean;

      Success : egl.EGLBoolean;
   begin
      Success := eglSwapBuffers (to_eGL (Self.Display),
                                 Self.egl_Surface);
      if Success = EGL_FALSE then
         raise opengl.Error with "unable to swap egl buffers";
      end if;
   end;


end opengl.Surface;
