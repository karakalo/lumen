with
     freetype_c.Pointers;

package freetype_c.pointer_Pointers 
is
   use freetype_c.Pointers;

   type FT_UShort_Pointer_Pointer       is access all FT_UShort_Pointer;
   type FT_Int_Pointer_Pointer          is access all FT_Int_Pointer;
   type FT_UInt_Pointer_Pointer         is access all FT_UInt_Pointer;
   type FT_Long_Pointer_Pointer         is access all FT_Long_Pointer;
   type FT_ULong_Pointer_Pointer        is access all FT_ULong_Pointer;
   type FT_Fixed_Pointer_Pointer        is access all FT_Fixed_Pointer;
   type FT_Pos_Pointer_Pointer          is access all FT_Pos_Pointer;
   type FT_Error_Pointer_Pointer        is access all FT_Error_Pointer;
   type FT_Encoding_Pointer_Pointer     is access all FT_Encoding_Pointer;
   type FT_F26Dot6_Pointer_Pointer      is access all FT_F26Dot6_Pointer;
   type FT_Int32_Pointer_Pointer        is access all FT_Int32_Pointer;
   type FT_UInt32_Pointer_Pointer       is access all FT_UInt32_Pointer;
   type FT_Render_Mode_Pointer_Pointer  is access all FT_Render_Mode_Pointer;
   type FT_Outline_Pointer_Pointer      is access all FT_Outline_Pointer;
   type FT_LibraryRec_Pointer_Pointer   is access all FT_LibraryRec_Pointer;
   type FT_GlyphSlotRec_Pointer_Pointer is access all FT_GlyphSlotRec_Pointer;
   type FT_FaceRec_Pointer_Pointer      is access all FT_FaceRec_Pointer;
   type FT_Kerning_Mode_Pointer_Pointer is access all FT_Kerning_Mode_Pointer;
   type FT_SizeRec_Pointer_Pointer      is access all FT_SizeRec_Pointer;

end freetype_c.pointer_Pointers;
