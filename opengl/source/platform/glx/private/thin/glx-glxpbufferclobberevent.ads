with
     Interfaces.C,
     System;

package GLX.GLXPbufferClobberEvent
is

   type Item is
      record
         event_type  : aliased Interfaces.C.int;
         draw_type   : aliased Interfaces.C.int;
         serial      : aliased Interfaces.C.unsigned_long;
         send_event  : aliased GLX.Bool;
         display     :         System.Address;   -- access xcb.Display;
         drawable    : aliased GLX.GLXDrawable;
         buffer_mask : aliased Interfaces.C.unsigned;
         aux_buffer  : aliased Interfaces.C.unsigned;
         x           : aliased Interfaces.C.int;
         y           : aliased Interfaces.C.int;
         width       : aliased Interfaces.C.int;
         height      : aliased Interfaces.C.int;
         count       : aliased Interfaces.C.int;
      end record;

   type Pointer         is access all Item;
   type Pointer_Pointer is access all Pointer;

   type Items    is array (C.size_t range <>) of aliased Item;
   type Pointers is array (C.size_t range <>) of aliased Pointer;

end GLX.GLXPbufferClobberEvent;
