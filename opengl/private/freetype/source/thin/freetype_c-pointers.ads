package freetype_c.Pointers 
is
   use Interfaces;

   type FT_UShort_Pointer       is access all FT_UShort;
   type FT_Int_Pointer          is access all FT_Int;
   type FT_UInt_Pointer         is access all FT_UInt;
   type FT_Long_Pointer         is access all FT_Long;
   type FT_ULong_Pointer        is access all FT_ULong;
   type FT_Fixed_Pointer        is access all FT_Fixed;
   type FT_Pos_Pointer          is access all FT_Pos;
   type FT_Error_Pointer        is access all FT_Error;
   type FT_Encoding_Pointer     is access all FT_Encoding;
   type FT_Int32_Pointer        is access all FT_Int32;
   type FT_F26Dot6_Pointer      is access all FT_F26Dot6;
   type FT_UInt32_Pointer       is access all FT_UInt32;
   type FT_Render_Mode_Pointer  is access all FT_Render_Mode;
   type FT_Outline_Pointer      is access all FT_Outline;
   type FT_LibraryRec_Pointer   is access all FT_LibraryRec;
   type FT_GlyphSlotRec_Pointer is access all FT_GlyphSlotRec;
   type FT_FaceRec_Pointer      is access all FT_FaceRec;
   type FT_Kerning_Mode_Pointer is access all FT_Kerning_Mode;
   type FT_SizeRec_Pointer      is access all FT_SizeRec;
   
   
   type FT_UShort_Pointer_array       is array (C.Size_t range <>) of aliased FT_UShort_Pointer;
   type FT_Int_Pointer_array          is array (C.Size_t range <>) of aliased FT_Int_Pointer;
   type FT_UInt_Pointer_array         is array (C.Size_t range <>) of aliased FT_UInt_Pointer;
   type FT_Long_Pointer_array         is array (C.Size_t range <>) of aliased FT_Long_Pointer;
   type FT_ULong_Pointer_array        is array (C.Size_t range <>) of aliased FT_ULong_Pointer;
   type FT_Fixed_Pointer_array        is array (C.Size_t range <>) of aliased FT_Fixed_Pointer;
   type FT_Pos_Pointer_array          is array (C.Size_t range <>) of aliased FT_Pos_Pointer;
   type FT_Error_Pointer_array        is array (C.Size_t range <>) of aliased FT_Error_Pointer;
   type FT_Encoding_Pointer_array     is array (C.Size_t range <>) of aliased FT_Encoding_Pointer;
   type FT_F26Dot6_Pointer_array      is array (C.Size_t range <>) of aliased FT_F26Dot6_Pointer;
   type FT_Int32_Pointer_array        is array (C.Size_t range <>) of aliased FT_Int32_Pointer;
   type FT_UInt32_Pointer_array       is array (C.Size_t range <>) of aliased FT_UInt32_Pointer;
   type FT_Render_Mode_Pointer_array  is array (C.Size_t range <>) of aliased FT_Render_Mode_Pointer;
   type FT_Outline_Pointer_array      is array (C.Size_t range <>) of aliased FT_Outline_Pointer;
   type FT_LibraryRec_Pointer_array   is array (C.Size_t range <>) of aliased FT_LibraryRec_Pointer;
   type FT_GlyphSlotRec_Pointer_array is array (C.Size_t range <>) of aliased FT_GlyphSlotRec_Pointer;
   type FT_FaceRec_Pointer_array      is array (C.Size_t range <>) of aliased FT_FaceRec_Pointer;
   type FT_Kerning_Mode_Pointer_array is array (C.Size_t range <>) of aliased FT_Kerning_Mode_Pointer;
   type FT_SizeRec_Pointer_array      is array (C.Size_t range <>) of aliased FT_SizeRec_Pointer;

end freetype_c.Pointers;
