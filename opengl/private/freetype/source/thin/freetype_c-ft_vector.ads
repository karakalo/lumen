package freetype_c.FT_Vector 
is
   use Interfaces;

   type Item is 
      record
         x : aliased FT_Pos;
         y : aliased FT_Pos;
      end record;

   type Item_array is array (C.Size_t range <>) of aliased FT_Vector.Item;


   type Pointer       is access all FT_Vector.Item;
   type Pointer_array is array (C.Size_t range <>) of aliased FT_Vector.Pointer;


   type pointer_Pointer is access all freetype_c.FT_Vector.Pointer;

end freetype_c.FT_Vector;
