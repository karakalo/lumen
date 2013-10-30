with
     freetype_c.Pointers;

package freetype_c.FT_Size 
is
   use Interfaces;

   subtype Item       is Pointers.FT_SizeRec_Pointer;
   type    Item_array is array (C.Size_t range <>) of aliased FT_Size.Item;


   type Pointer       is access all freetype_c.FT_Size.Item;
   type Pointer_array is array (C.Size_t range <>) of aliased FT_Size.Pointer;

   type pointer_Pointer is access all FT_Size.Pointer;

end freetype_c.FT_Size;
