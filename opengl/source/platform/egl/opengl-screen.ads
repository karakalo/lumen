package openGL.Screen
--
--  Models an openGL screen.
--
is

   type Item is tagged limited private;


--     function  Thin    (Self : in     Item)     return xcb.xcb_screen_t.Pointer;
--     procedure Thin_is (Self : in out Item;   Now : in xcb.xcb_screen_t.Pointer);



private

   type Item is tagged limited
      record
         null; -- Thin : xcb.xcb_screen_t.Pointer;
      end record;

   procedure dummy;

end openGL.Screen;
