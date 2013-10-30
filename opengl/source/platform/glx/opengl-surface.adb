with
     openGL.Context,
     opengl.surface_Profile.privvy,
     opengl.Display        .privvy,

     glx.Binding,

     Interfaces.C,
     System;



package body opengl.Surface
--
--
--
is
   use Glx,
       GLX.Binding,
       Interfaces;


   visual_attribs : array (Positive range <>) of aliased C.int := (GLX_X_RENDERABLE, 1,
                                                                   GLX_DRAWABLE_TYPE,   GLX_WINDOW_BIT,
                                                                   GLX_RENDER_TYPE,     GLX_RGBA_BIT,
                                                                   GLX_X_VISUAL_TYPE,   GLX_TRUE_COLOR,
                                                                   GLX_RED_SIZE,        8,
                                                                   GLX_GREEN_SIZE,      8,
                                                                   GLX_BLUE_SIZE,       8,
                                                                   GLX_ALPHA_SIZE,      8,
                                                                   GLX_DEPTH_SIZE,      24,
                                                                   GLX_STENCIL_SIZE,    8,
                                                                   GLX_DOUBLEBUFFER,    1,
                                                                   -- GLX_SAMPLE_BUFFERS  , 1,
                                                                   -- GLX_SAMPLES         , 4,
                                                                   0
                                                                  );


   procedure define (Self : in out Item;   surface_Profile : in opengl.surface_Profile.item'Class;
                                           Display         : in opengl.Display.Item;
                                           Window_Id       : in Natural)
   is
      use opengl.Display        .privvy,
          opengl.surface_Profile.privvy;

      the_surface_Profile : constant opengl.surface_Profile.item'Class := surface_Profile;
--        the_Visual          : access GLX.XVisualInfo
--           := glXGetVisualFromFBConfig (dpy    => to_xcb (Display),
--                                        config => to_glx (the_surface_Profile));
   begin

--        Self.glx_Surface := glXCreateWindow (dpy        => to_xcb (Display),
--                                             config     => to_glx (the_surface_Profile),
--                                             win        => System.null_Address, -- c.unsigned_long (Window_Id),
--                                             attribList => null);
      Self.Display := Display;
   end define;



   --  Operations
   --
   procedure swap_Buffers (Self : in Item)
   is
      use openGL.Display.privvy;
   begin
      null;
--        glXSwapBuffers (dpy      => to_Xcb (Self.Display),
--                        drawable => Self.glx_Surface);
   end swap_Buffers;


end opengl.Surface;
