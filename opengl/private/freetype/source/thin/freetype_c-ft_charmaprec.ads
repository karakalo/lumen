package freetype_c.FT_CharMapRec 
is
   use Interfaces;

   type Item is 
      record
         face        : access  FT_FaceRec;
         encoding    : aliased FT_Encoding;
         platform_id : aliased FT_UShort;
         encoding_id : aliased FT_UShort;
      end record;

   type Item_array is array (C.Size_t range <>) of aliased FT_CharMapRec.Item;


   type Pointer is access all FT_CharMapRec.Item;
   type Pointer_array is array (C.Size_t range <>) of aliased FT_CharMapRec.Pointer;

   type pointer_Pointer is access all FT_CharMapRec.Pointer;

end freetype_c.FT_CharMapRec;
