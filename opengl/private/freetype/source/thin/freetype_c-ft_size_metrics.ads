package freetype_c.FT_Size_Metrics 
is
   use Interfaces;

   type Item is 
      record
         x_ppem      : aliased FT_UShort;
         y_ppem      : aliased FT_UShort;
         x_scale     : aliased FT_Fixed;
         y_scale     : aliased FT_Fixed;
         ascender    : aliased FT_Pos;
         descender   : aliased FT_Pos;
         height      : aliased FT_Pos;
         max_advance : aliased FT_Pos;
      end record;

   type Item_array is array (C.Size_t range <>) of aliased FT_Size_Metrics.Item;


   type Pointer       is access all freetype_c.FT_Size_Metrics.Item;
   type Pointer_array is array (interfaces.C.Size_t range <>) of aliased freetype_c.FT_Size_Metrics.Pointer;

   type pointer_Pointer is access all freetype_c.FT_Size_Metrics.Pointer;

end freetype_c.FT_Size_Metrics;
