with
     openGL.Display.privvy,
     glx.Binding,

     interfaces.C,
     ada.Unchecked_Conversion;


package body opengl.surface_Profile
is
   use openGL.Display.privvy,
       Interfaces,
       GLX,
       glx.Binding;


   visual_attribs : array (Positive range <>) of aliased C.int := (GLX_X_RENDERABLE,    1,
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


   procedure define (Self : in out Item;   the_Display : access opengl.Display.item'Class;
                                           Screen      : access opengl.Screen.item'Class;
                                           Desired     : in     Qualities          := default_Qualities)
   is
      pragma Unreferenced (Desired);

      use openGL.Screen,
          glx.Pointers;

      default_screen : constant C.int := the_Display.screen_Id; -- x11_DefaultScreen (to_xcb (the_Display.all));
      num_fb_configs : aliased  C.int := 0;

--        fb_configs     : constant GLXFBConfig_Pointer
--          := GLXFBConfig_Pointer (glXGetFBConfigs (dpy       => to_xcb (the_Display.all),
--                                                   screen    => default_screen,
--                                                   nelements => num_fb_configs'Unchecked_Access));
--        fb_configs     : constant System.Address -- GLXFBConfig_Pointer
--          := glXGetFBConfigs (dpy       => to_xcb (the_Display.all),
--                                                   screen    => default_screen,
--                                                   nelements => num_fb_configs'Unchecked_Access);
--        fb_config      : constant GLXFBConfig
--          := Value (fb_configs) (0);
--
      visual_Id      : aliased C.int;
--        colormap       :         xcb.xcb_colormap_t;

--        cookie         :         xcb.xcb_void_cookie_t.item;
--        pragma Unreferenced (cookie);
      unused         :         C.int;
      pragma Unreferenced (unused);

--        function to_Unsigned_8 is new ada.Unchecked_Conversion (xcb.xcb_colormap_alloc_t, Unsigned_8);

   begin
--        unused   := glXGetFBConfigAttrib (to_xcb (the_Display.all),
--                                          fb_config,
--                                          GLX_VISUAL_ID,
--                                          visual_ID'Unchecked_Access);

--        colormap := xcb_generate_id (get_connection (the_Display.all));
--
--        cookie   := xcb_create_colormap (get_connection (the_Display.all),
--                                         to_Unsigned_8 (xcb.XCB_COLORMAP_ALLOC_NONE),
--                                         colormap,
--                                         Thin (Screen.all).root,
--                                         Unsigned_32 (visual_ID));

--        Self.glx_Config := fb_config;
      Self.Display    := the_Display;
   end define;



   function get_Visual (Self : in Item) return access GLX.XVisualInfo
   is
   begin
      return Self.Visual;
   end get_Visual;



   function fetch_All (the_Display : access opengl.Display.item'class) return surface_Profile.items
   is
   begin
      raise Program_Error with "TBD";
      return (1 .. 0 =>  <>);
   end fetch_All;



   function Quality (Self : in Item) return Qualities
   is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "TBD";
      return (others => <>);
   end Quality;



   function value_Image (Value : in Natural) return String
   is
   begin
      if Value = Irrelevant then
         return "Irrelevant";
      else
         return Natural'Image (Value);
      end if;
   end value_Image;



   function Image (Self : in color_Buffer) return String
   is
   begin
      return
            "Bits_red =>"        & value_Image (Self.Bits_red)
        & "  Bits_green =>"      & value_Image (Self.Bits_green)
        & "  Bits_blue =>"       & value_Image (Self.Bits_blue)
        & "  Bits_luminence =>"  & value_Image (Self.Bits_luminence)
        & "  Bits_alpha =>"      & value_Image (Self.Bits_alpha)
        & "  Bits_alpha_mask =>" & value_Image (Self.Bits_alpha_mask);
   end Image;



   function Image (Self : in Qualities) return String
   is
   begin
      return
          Image (Self.color_Buffer)
        & "  depth_buffer_Bits =>"    & value_Image (Self.depth_buffer_Bits)
        & "  stencil_buffer_Bits => " & value_Image (Self.stencil_buffer_Bits);
   end Image;


end opengl.surface_Profile;
