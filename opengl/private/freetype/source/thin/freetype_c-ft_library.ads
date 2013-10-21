with
     freetype_c.Pointers;

package freetype_c.FT_Library 
is
   use Interfaces;

   subtype Item       is Pointers.FT_LibraryRec_Pointer;
   type    Item_array is array (C.Size_t range <>) of aliased FT_Library.Item;


   type Pointer       is access all freetype_c.FT_Library.Item;
   type Pointer_array is array (interfaces.C.Size_t range <>) of aliased freetype_c.FT_Library.Pointer;

   type pointer_Pointer is access all freetype_c.FT_Library.Pointer;

end freetype_c.FT_Library;
