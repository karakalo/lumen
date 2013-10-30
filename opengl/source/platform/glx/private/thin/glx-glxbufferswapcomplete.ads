with
--   xcb,
     Interfaces.C.Pointers,
     Interfaces.C.Strings,
     System;

package GLX.GLXBufferSwapComplete is

   -- Item
   --

   type Item is record
      the_type   : aliased Interfaces.C.int;
      serial     : aliased Interfaces.C.unsigned_long;
      send_event : aliased GLX.Bool;
      display    :         System.Address;  -- access xcb.Display;
      drawable   : aliased GLX.GLXDrawable;
      event_type : aliased Interfaces.C.int;
      ust        : aliased Interfaces.Integer_64;
      msc        : aliased Interfaces.Integer_64;
      sbc        : aliased Interfaces.Integer_64;
   end record;

   -- Items
   --
   type Items is
     array (Interfaces.C.size_t range <>)
            of aliased GLX.GLXBufferSwapComplete.Item;

   -- Pointer
   --
   type Pointer is access all GLX.GLXBufferSwapComplete.Item;

   -- Pointers
   --
   type Pointers is
     array (Interfaces.C.size_t range <>)
            of aliased GLX.GLXBufferSwapComplete.Pointer;

   -- Pointer_Pointer
   --
   type Pointer_Pointer is access all GLX.GLXBufferSwapComplete.Pointer;

   function construct return  GLX.GLXBufferSwapComplete.Item;

private

   pragma Import (C, construct, "Ada_new_GLXBufferSwapComplete");

end GLX.GLXBufferSwapComplete;
