with GLX.GLXBufferSwapComplete;
with GLX.GLXPbufferClobberEvent;
with Swig;

package GLX.GLXEvent
is

   type Item_variant is (glxpbufferclobber_variant,
                         glxbufferswapcomplete_variant,
                         pad_variant);

   type Item (union_Variant : Item_variant := Item_variant'First) is
      record
         case union_Variant is
         when glxpbufferclobber_variant     =>   glxpbufferclobber     : aliased GLX.GLXPbufferClobberEvent.Item;
         when glxbufferswapcomplete_variant =>   glxbufferswapcomplete : aliased GLX.GLXBufferSwapComplete.Item;
         when pad_variant                   =>   pad                   : aliased Swig.long_Array (0 .. 23);
         end case;
      end record;
   pragma Unchecked_Union (Item);


   type Pointer         is access all Item;
   type Pointer_Pointer is access all Pointer;

   type Items    is array (C.size_t range <>) of aliased Item;
   type Pointers is array (C.size_t range <>) of aliased Pointer;

end GLX.GLXEvent;
