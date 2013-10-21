with opengl.Display        .privvy,
     opengl.surface_Profile.privvy,
     opengl.Surface        .privvy,

     glx.Binding,
     glx.Pointers,

     System,
     ada.Text_IO;


package body openGL.Context
is
   use System,
       Ada.Text_IO;


   procedure define (Self : in out Item;   the_Display         : access opengl.Display.item'Class;
                                           the_surface_Profile : in     opengl.surface_Profile.item'Class)
--                                             the_Surface         : in     openGL.Surface.view)

   is
      use opengl.Display.privvy,
          opengl.Surface.privvy,
          opengl.surface_Profile.privvy,
          Glx,
          glx.Binding,
          glx.Pointers;

--        contextAttribs : EGLint_array := (EGL_CONTEXT_CLIENT_VERSION, 2,
--                                          EGL_NONE);

--        Cookie : xcb.xcb_void_cookie_t.item;
   begin
--        Self.glx_Context := glXCreateNewContext (to_xcb (the_Display.all),
--                                                 to_glx (the_surface_Profile),
--                                                 GLX_RGBA_TYPE,
--                                                 null,
--                                                 1);

--           context := glXCreateNewContext (to_xcb (the_Display.all),
--                                           fb_config,
--                                           GLX_RGBA_TYPE,
--                                           null_Address,
--                                           1);

      if Self.glx_Context = null
      then
         put_Line ("NO CONTEXT");
         raise Program_Error;
      end if;

--        Cookie := xcb_glx_create_context (c          => to_xcb (the_Display.all),
--                                          context    => Self.glx_Context,
--                                          visual     => to_glx (the_Surface.all),
--                                          screen     => Unsigned_32 (the_Display.screen_Id),
--                                          share_list => 0,
--                                          is_direct  => 0);

--        self.egl_Context := eglCreateContext (to_eGL (the_Display.all),
--                                              to_eGL (the_surface_Profile),
--                                              EGL_NO_CONTEXT,
--                                              contextAttribs (contextAttribs'first)'unchecked_access);
--
--        if self.egl_Context = EGL_NO_CONTEXT then
--           raise opengl.Error with "unable to create an EGL Context";
--        end if;

      Self.Display := the_Display;
   end define;



   procedure make_Current (Self : in Item;   read_Surface  : in opengl.Surface.item;
                                             write_Surface : in opengl.Surface.item)
   is
      pragma Unreferenced (write_Surface);

      use opengl.Display.privvy,
          opengl.Surface.privvy,
          glx.Binding;
--        use type EGLBoolean;

--        Cookie : xcb.xcb_glx_make_current_cookie_t.item;

      Success : glx.Bool;
      pragma Unreferenced (Success);

--        Success : EGLBoolean := eglmakeCurrent (to_eGL (Self.Display.all),
--                                                to_eGL (read_Surface), to_eGL (write_Surface),
--                                                Self.egl_Context);
   begin
--        Success := glXMakeCurrent (dpy      => to_xcb (Self.Display.all),
--                                   drawable => to_glx (read_Surface),
--                                   ctx      => Self.glx_Context);

--        Cookie := xcb_glx_make_current (c               => to_xcb (Self.Display.all),
--                                        drawable        => to_glx (read_Surface),
--                                        context         => Self.glx_Context,
--                                        old_context_tag => 0);


--        if Success = EGL_FALSE then
--           raise openGL.Error with "unable to make egl Context current";
--        end if;
      null;
   end make_Current;



   function glx_Context_debug (Self : in Item'Class) return GLX.GLXContext.Item
   is
   begin
      return self.glx_Context;
   end glx_Context_debug;


end openGL.Context;
