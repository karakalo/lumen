with
     freetype_c.FT_CharMapRec;

package freetype_c.FT_CharMap 
is
   use Interfaces;

   subtype Item       is FT_CharMapRec.Pointer;
   type    Item_array is array (interfaces.C.Size_t range <>) of aliased FT_CharMap.Item;

   type Pointer       is access all FT_CharMap.Item;
   type Pointer_array is array (C.Size_t range <>) of aliased FT_CharMap.Pointer;

   type pointer_Pointer is access all freetype_c.FT_CharMap.Pointer;

end freetype_c.FT_CharMap;
