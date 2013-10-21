with 
     freetype_c.Pointers;

package freetype_c.FT_Face 
is
   use Interfaces;

   subtype Item       is Pointers.FT_FaceRec_Pointer;
   type    Item_array is array (C.Size_t range <>) of aliased FT_Face.Item;


   type Pointer is access all FT_Face.Item;
   type Pointer_array is array (C.Size_t range <>) of aliased FT_Face.Pointer;
   
   type pointer_Pointer is access all FT_Face.Pointer;

end freetype_c.FT_Face;
