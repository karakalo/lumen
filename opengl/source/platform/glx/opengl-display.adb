with
     Interfaces.C.Strings;


package body opengl.Display
is

--     use eGL,  eGL.Binding,  eGL.Pointers;
--     use xcb.Binding;


   function Default return Item
   is
--        use type system.Address,  eGL.EGLBoolean;

      Self    : Display.item;
--        Success     : EGLBoolean;

--        the_Connection :         xcb_connection_t_Pointer;

--        void_Cookie    :         xcb.xcb_void_cookie_t.item;
--        Status         :         C.int;
--
--        Mask           : swig.uint32_t;
--        Values         : access swig.uint32_t_array := new swig.uint32_t_array (1 .. 2);
--
--
--        procedure free is new ada.Unchecked_Deallocation (xcb.xcb_generic_event_t.item,
--                                                          xcb.xcb_generic_event_t.item_Pointer);
--        function to_Flag is new ada.Unchecked_Conversion (xcb.xcb_event_mask_t,          swig.uint32_t);
--        function to_Flag is new ada.Unchecked_Conversion (xcb.xcb_cw_t,                  swig.uint32_t);

   begin
      --  tbd: Use the DISPLAY environment variable as the default display name ?
--        Self.xcb := x11_XOpenDisplay (display_name => interfaces.c.Strings.Null_Ptr);

--        Self.Connection := xcb_connection_t_Pointer (xcb_connect (c.strings.null_Ptr,
--                                                                  Self.screen_Id));
--        Self.Connection    := x11_XGetXCBConnection (Self.Xcb);
--        Self.screen_Id.all := x11_DefaultScreen (Self.Xcb);

      return Self;
   end Default;



   function screen_Id (Self : in Item) return interfaces.c.int
   is
   begin
      return Self.screen_Id.all;
   end screen_Id;


end opengl.Display;
