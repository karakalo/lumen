with
     freetype_c.FT_BBox,
     freetype_c.FT_Face,
     freetype_c.FT_Bitmap,
     freetype_c.FT_Library,
     freetype_c.FT_Size_Metrics,
     freetype_c.FT_CharMapRec,
     freetype_c.FT_Size,
     freetype_c.FT_Vector,
     freetype_c.FT_GlyphSlot,
     freetype_c.FT_CharMap,
     freetype_c.Pointers,

     interfaces.c.Pointers,
     interfaces.c.Strings,

     System;


package freetype_c.Binding
--
--  Provides the Freetype library functions.
--
is
   use Interfaces,
       freetype_c.Pointers;


   --  unsigned_char_Pointer
   --
   type    unsigned_char_Array      is array (C.size_t range <>) of aliased C.unsigned_Char;
   package c_unsigned_char_Pointers is new interfaces.c.Pointers (Index              => C.size_t,
                                                                  Element            => C.unsigned_Char,
                                                                  element_Array      => unsigned_char_Array,
                                                                  default_Terminator => 0);
   subtype unsigned_char_Pointer is c_unsigned_char_Pointers.Pointer;



   --  Subprograms
   --

   procedure FT_Outline_Get_CBox (outline     : in FT_Outline_Pointer;
                                  acbox       : in FT_BBox.Pointer);

   function  FT_Init_FreeType    (alibrary    : in FT_Library.Pointer)   return FT_Error;

   function  FT_Done_FreeType    (alibrary    : in FT_Library.Item)      return FT_Error;

   function  FT_Render_Glyph     (slot        : in FT_GlyphSlot.Item;
                                  render_mode : in FT_Render_Mode)       return FT_Error;

   function  FT_Set_Char_Size    (face            : in FT_Face.Item;
                                  char_width      : in FT_F26Dot6;
                                  char_height     : in FT_F26Dot6;
                                  horz_resolution : in FT_UInt;
                                  vert_resolution : in FT_UInt)          return FT_Error;

   function  FT_Done_Face        (face         : in FT_Face.Item)        return FT_Error;

   function  FT_Attach_File      (face         : in FT_Face.Item;
                                  filepathname : in C.strings.chars_ptr) return FT_Error;

   function  FT_Set_Charmap      (face         : in FT_Face.Item;
                                  charmap      : in FT_CharMap.Item)     return FT_Error;

   function  FT_Select_Charmap   (face         : in FT_Face.Item;
                                  encoding     : in FT_Encoding)         return FT_Error;

   function  FT_Get_Char_Index   (face         : in FT_Face.Item;
                                  charcode     : in FT_ULong)            return FT_UInt;

   function  FT_Get_Kerning      (face         : in FT_Face.Item;
                                  left_glyph   : in FT_UInt;
                                  right_glyph  : in FT_UInt;
                                  kern_mode    : in FT_UInt;
                                  akerning     : in FT_Vector.Pointer)   return FT_Error;

   function  FT_Load_Glyph       (face         : in FT_Face.Item;
                                  glyph_index  : in FT_UInt;
                                  load_flags   : in FT_Int32)            return FT_Error;

   function  FT_GlyphSlot_Get_Outline     (Self : in FT_GlyphSlot.Item) return access FT_Outline;
   function  FT_GlyphSlot_Get_Advance     (Self : in FT_GlyphSlot.Item) return FT_Vector.Item;
   function  FT_GlyphSlot_Get_Bitmap      (Self : in FT_GlyphSlot.Item) return FT_Bitmap.Item;
   function  FT_GlyphSlot_Get_bitmap_left (Self : in FT_GlyphSlot.Item) return FT_Int;
   function  FT_GlyphSlot_Get_bitmap_top  (Self : in FT_GlyphSlot.Item) return FT_Int;
   function  FT_GlyphSlot_Get_Format      (Self : in FT_GlyphSlot.Item) return C.unsigned;
   function  FT_Size_Get_Metrics          (Self : in FT_Size.Item)      return FT_Size_Metrics.Item;

   function  new_FT_Face              (Library           : in FT_Library.Item;
                                       fontFilePath      : in C.strings.chars_ptr) return access FT_FaceRec;

   function  new_FT_Memory_Face       (Library           : in FT_Library.Item;
                                       pBufferBytes      : in unsigned_char_Pointer;
                                       bufferSizeInBytes : in C.int) return access FT_FaceRec;

   function  FT_Face_Get_Size         (Self : in FT_Face.Item) return access FT_SizeRec;
   function  FT_Face_IS_SCALABLE      (Self : in FT_Face.Item) return FT_Long;
   function  FT_Face_HAS_KERNING      (Self : in FT_Face.Item) return FT_Long;
   function  FT_Face_Get_BBox         (Self : in FT_Face.Item) return FT_BBox.Item;
   function  FT_Face_Get_units_per_EM (Self : in FT_Face.Item) return FT_UShort;
   function  FT_Face_Get_num_glyphs   (Self : in FT_Face.Item) return FT_Long;

   function  FT_Face_Get_charmap      (Self : in FT_Face.Item)                     return  access FT_CharMapRec.Item;
   function  FT_Face_Get_charmap_at   (Self : in FT_Face.Item;   index : in C.int) return  access FT_CharMapRec.Item;

   function  FT_Face_Get_num_charmaps (Self : in FT_Face.Item) return FT_Int;
   function  FT_Face_Get_glyph        (Self : in FT_Face.Item) return access FT_GlyphSlotRec;

   function  FT_Face_Attach_Stream    (Self : in FT_Face.Item;   pBufferBytes      : in unsigned_char_Pointer;
                                                                 bufferSizeInBytes : in C.size_t) return FT_Error;

   function  get_FT_GLYPH_FORMAT_NONE         return C.unsigned;
   function  get_FT_GLYPH_FORMAT_COMPOSITE    return C.unsigned;
   function  get_FT_GLYPH_FORMAT_BITMAP       return C.unsigned;
   function  get_FT_GLYPH_FORMAT_OUTLINE      return C.unsigned;
   function  get_FT_GLYPH_FORMAT_PLOTTER      return C.unsigned;

   function  FT_ENCODING_NONE_enum            return FT_Encoding;
   function  FT_ENCODING_MS_SYMBOL_enum       return FT_Encoding;
   function  FT_ENCODING_UNICODE_enum         return FT_Encoding;
   function  FT_ENCODING_SJIS_enum            return FT_Encoding;
   function  FT_ENCODING_GB2312_enum          return FT_Encoding;
   function  FT_ENCODING_BIG5_enum            return FT_Encoding;
   function  FT_ENCODING_WANSUNG_enum         return FT_Encoding;
   function  FT_ENCODING_JOHAB_enum           return FT_Encoding;
   function  FT_ENCODING_ADOBE_STANDARD_enum  return FT_Encoding;
   function  FT_ENCODING_ADOBE_EXPERT_enum    return FT_Encoding;
   function  FT_ENCODING_ADOBE_CUSTOM_enum    return FT_Encoding;
   function  FT_ENCODING_ADOBE_LATIN_1_enum   return FT_Encoding;
   function  FT_ENCODING_OLD_LATIN_2_enum     return FT_Encoding;
   function  FT_ENCODING_APPLE_ROMAN_enum     return FT_Encoding;

   function  FT_LOAD_DEFAULT_flag             return C.unsigned;
   function  FT_LOAD_NO_SCALE_flag            return C.unsigned;
   function  FT_LOAD_NO_HINTING_flag          return C.unsigned;
   function  FT_LOAD_RENDER_flag              return C.unsigned;
   function  FT_LOAD_NO_BITMAP_flag           return C.unsigned;
   function  FT_LOAD_VERTICAL_LAYOUT_flag     return C.unsigned;
   function  FT_LOAD_FORCE_AUTOHINT_flag      return C.unsigned;
   function  FT_LOAD_CROP_BITMAP_flag         return C.unsigned;
   function  FT_LOAD_PEDANTIC_flag            return C.unsigned;
   function  FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH_flag
                                              return C.unsigned;
   function  FT_LOAD_NO_RECURSE_flag          return C.unsigned;
   function  FT_LOAD_IGNORE_TRANSFORM_flag    return C.unsigned;
   function  FT_LOAD_MONOCHROME_flag          return C.unsigned;
   function  FT_LOAD_LINEAR_DESIGN_flag       return C.unsigned;
   function  FT_LOAD_NO_AUTOHINT_flag         return C.unsigned;



private

   pragma Import (C, FT_Outline_Get_CBox,             "FT_Outline_Get_CBox");
   pragma Import (C, FT_Init_FreeType,                "FT_Init_FreeType");
   pragma Import (C, FT_Done_FreeType,                "FT_Done_FreeType");
   pragma Import (C, FT_Render_Glyph,                 "FT_Render_Glyph");
   pragma Import (C, FT_Set_Char_Size,                "FT_Set_Char_Size");
   pragma Import (C, FT_Done_Face,                    "FT_Done_Face");
   pragma Import (C, FT_Attach_File,                  "FT_Attach_File");
   pragma Import (C, FT_Set_Charmap,                  "FT_Set_Charmap");
   pragma Import (C, FT_Select_Charmap,               "FT_Select_Charmap");
   pragma Import (C, FT_Get_Char_Index,               "FT_Get_Char_Index");
   pragma Import (C, FT_Get_Kerning,                  "FT_Get_Kerning");
   pragma Import (C, FT_Load_Glyph,                   "FT_Load_Glyph");
   pragma Import (C, FT_GlyphSlot_Get_Outline,        "FT_GlyphSlot_Get_Outline");
   pragma Import (C, FT_GlyphSlot_Get_Advance,        "FT_GlyphSlot_Get_Advance");
   pragma Import (C, FT_GlyphSlot_Get_Bitmap,         "FT_GlyphSlot_Get_Bitmap");
   pragma Import (C, FT_GlyphSlot_Get_bitmap_left,    "FT_GlyphSlot_Get_bitmap_left");
   pragma Import (C, FT_GlyphSlot_Get_bitmap_top,     "FT_GlyphSlot_Get_bitmap_top");
   pragma Import (C, FT_GlyphSlot_Get_Format,         "FT_GlyphSlot_Get_Format");
   pragma Import (C, FT_Size_Get_Metrics,             "FT_Size_Get_Metrics");
   pragma Import (C, new_FT_Face,                     "new_FT_Face");
   pragma Import (C, new_FT_Memory_Face,              "new_FT_Memory_Face");
   pragma Import (C, FT_Face_Get_Size,                "FT_Face_Get_Size");
   pragma Import (C, FT_Face_IS_SCALABLE,             "FT_Face_IS_SCALABLE");
   pragma Import (C, FT_Face_HAS_KERNING,             "FT_Face_HAS_KERNING");
   pragma Import (C, FT_Face_Get_BBox,                "FT_Face_Get_BBox");
   pragma Import (C, FT_Face_Get_units_per_EM,        "FT_Face_Get_units_per_EM");
   pragma Import (C, FT_Face_Get_num_glyphs,          "FT_Face_Get_num_glyphs");
   pragma Import (C, FT_Face_Get_charmap,             "FT_Face_Get_charmap");
   pragma Import (C, FT_Face_Get_charmap_at,          "FT_Face_Get_charmap_at");
   pragma Import (C, FT_Face_Get_num_charmaps,        "FT_Face_Get_num_charmaps");
   pragma Import (C, FT_Face_Get_glyph,               "FT_Face_Get_glyph");
   pragma Import (C, FT_Face_Attach_Stream,           "FT_Face_Attach_Stream");
   pragma Import (C, get_FT_GLYPH_FORMAT_NONE,        "get_FT_GLYPH_FORMAT_NONE");
   pragma Import (C, get_FT_GLYPH_FORMAT_COMPOSITE,   "get_FT_GLYPH_FORMAT_COMPOSITE");
   pragma Import (C, get_FT_GLYPH_FORMAT_BITMAP,      "get_FT_GLYPH_FORMAT_BITMAP");
   pragma Import (C, get_FT_GLYPH_FORMAT_OUTLINE,     "get_FT_GLYPH_FORMAT_OUTLINE");
   pragma Import (C, get_FT_GLYPH_FORMAT_PLOTTER,     "get_FT_GLYPH_FORMAT_PLOTTER");
   pragma Import (C, FT_ENCODING_NONE_enum,           "FT_ENCODING_NONE_enum");
   pragma Import (C, FT_ENCODING_MS_SYMBOL_enum,      "FT_ENCODING_MS_SYMBOL_enum");
   pragma Import (C, FT_ENCODING_UNICODE_enum,        "FT_ENCODING_UNICODE_enum");
   pragma Import (C, FT_ENCODING_SJIS_enum,           "FT_ENCODING_SJIS_enum");
   pragma Import (C, FT_ENCODING_GB2312_enum,         "FT_ENCODING_GB2312_enum");
   pragma Import (C, FT_ENCODING_BIG5_enum,           "FT_ENCODING_BIG5_enum");
   pragma Import (C, FT_ENCODING_WANSUNG_enum,        "FT_ENCODING_WANSUNG_enum");
   pragma Import (C, FT_ENCODING_JOHAB_enum,          "FT_ENCODING_JOHAB_enum");
   pragma Import (C, FT_ENCODING_ADOBE_STANDARD_enum, "FT_ENCODING_ADOBE_STANDARD_enum");
   pragma Import (C, FT_ENCODING_ADOBE_EXPERT_enum,   "FT_ENCODING_ADOBE_EXPERT_enum");
   pragma Import (C, FT_ENCODING_ADOBE_CUSTOM_enum,   "FT_ENCODING_ADOBE_CUSTOM_enum");
   pragma Import (C, FT_ENCODING_ADOBE_LATIN_1_enum,  "FT_ENCODING_ADOBE_LATIN_1_enum");
   pragma Import (C, FT_ENCODING_OLD_LATIN_2_enum,    "FT_ENCODING_OLD_LATIN_2_enum");
   pragma Import (C, FT_ENCODING_APPLE_ROMAN_enum,    "FT_ENCODING_APPLE_ROMAN_enum");
   pragma Import (C, FT_LOAD_DEFAULT_flag,            "FT_LOAD_DEFAULT_flag");
   pragma Import (C, FT_LOAD_NO_SCALE_flag,           "FT_LOAD_NO_SCALE_flag");
   pragma Import (C, FT_LOAD_NO_HINTING_flag,         "FT_LOAD_NO_HINTING_flag");
   pragma Import (C, FT_LOAD_RENDER_flag,             "FT_LOAD_RENDER_flag");
   pragma Import (C, FT_LOAD_NO_BITMAP_flag,          "FT_LOAD_NO_BITMAP_flag");
   pragma Import (C, FT_LOAD_VERTICAL_LAYOUT_flag,    "FT_LOAD_VERTICAL_LAYOUT_flag");
   pragma Import (C, FT_LOAD_FORCE_AUTOHINT_flag,     "FT_LOAD_FORCE_AUTOHINT_flag");
   pragma Import (C, FT_LOAD_CROP_BITMAP_flag,        "FT_LOAD_CROP_BITMAP_flag");
   pragma Import (C, FT_LOAD_PEDANTIC_flag,           "FT_LOAD_PEDANTIC_flag");
   pragma Import (C, FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH_flag,
                                                      "FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH_flag");
   pragma Import (C, FT_LOAD_NO_RECURSE_flag,         "FT_LOAD_NO_RECURSE_flag");
   pragma Import (C, FT_LOAD_IGNORE_TRANSFORM_flag,   "FT_LOAD_IGNORE_TRANSFORM_flag");
   pragma Import (C, FT_LOAD_MONOCHROME_flag,         "FT_LOAD_MONOCHROME_flag");
   pragma Import (C, FT_LOAD_LINEAR_DESIGN_flag,      "FT_LOAD_LINEAR_DESIGN_flag");
   pragma Import (C, FT_LOAD_NO_AUTOHINT_flag,        "FT_LOAD_NO_AUTOHINT_flag");

end freetype_c.Binding;
