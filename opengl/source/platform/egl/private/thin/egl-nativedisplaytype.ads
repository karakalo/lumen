with
     eGL.Pointers;

package eGL.NativeDisplayType
is
   subtype Item          is eGL.Pointers.Display_Pointer;
   type    Item_array    is array (C.size_t range <>) of aliased Item;

   type    Pointer       is access all eGL.NativeDisplayType.Item;
   type    Pointer_array is array (C.size_t range <>) of aliased Pointer;

end eGL.NativeDisplayType;
