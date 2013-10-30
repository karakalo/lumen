package freetype_c.FT_BBox 
is
   use Interfaces;
      
   type Item is 
      record
         xMin : aliased FT_Pos;
         yMin : aliased FT_Pos;
         xMax : aliased FT_Pos;
         yMax : aliased FT_Pos;
      end record;

   type Item_array is array (C.Size_t range <>) of aliased FT_BBox.Item;


   type Pointer       is access all FT_BBox.Item;
   type Pointer_array is array (C.Size_t range <>) of aliased FT_BBox.Pointer;

   type pointer_Pointer is access all FT_BBox.Pointer;

end freetype_c.FT_BBox;
