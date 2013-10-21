with
     interfaces.C.Pointers,
     interfaces.C.Strings,
     System;


package Freetype_C
--
--  Provides core types for the Freetype C library.
--
is
   use Interfaces;


   -- FT_UShort
   --
   subtype FT_UShort       is C.unsigned_short;
   type    FT_UShort_array is array (C.Size_t range <>) of aliased FT_UShort;

   -- FT_Int
   --
   subtype FT_Int       is C.int;
   type    FT_Int_array is array (C.Size_t range <>) of aliased FT_Int;

   -- FT_UInt
   --
   subtype FT_UInt       is C.unsigned;
   type    FT_UInt_array is array (C.Size_t range <>) of aliased FT_UInt;

   -- FT_Long
   --
   subtype FT_Long       is C.long;
   type    FT_Long_array is array (C.Size_t range <>) of aliased FT_Long;

   -- FT_ULong
   --
   subtype FT_ULong       is C.unsigned_long;
   type    FT_ULong_array is array (C.Size_t range <>) of aliased FT_ULong;

   -- FT_Fixed
   --
   subtype FT_Fixed       is C.long;
   type    FT_Fixed_array is array (C.Size_t range <>) of aliased FT_Fixed;

   -- FT_Pos
   --
   subtype FT_Pos       is C.long;
   type    FT_Pos_array is array (C.Size_t range <>) of aliased FT_Pos;

   -- FT_Error
   --
   subtype FT_Error       is C.int;
   type    FT_Error_array is array (C.Size_t range <>) of aliased FT_Error;

   -- FT_Encoding
   --
   subtype FT_Encoding       is C.unsigned;
   type    FT_Encoding_array is array (C.Size_t range <>) of aliased FT_Encoding;

   -- FT_F26Dot6
   --
   subtype FT_F26Dot6       is C.long;
   type    FT_F26Dot6_array is array (C.Size_t range <>) of aliased FT_F26Dot6;

   -- FT_Int32
   --
   subtype FT_Int32       is C.int;
   type    FT_Int32_array is array (C.Size_t range <>) of aliased FT_Int32;

   -- FT_UInt32
   --
   subtype FT_UInt32       is C.unsigned;
   type    FT_UInt32_array is array (C.Size_t range <>) of aliased FT_UInt32;

   -- FT_Render_Mode
   --
   type FT_Render_Mode is (FT_RENDER_MODE_NORMAL,
                           FT_RENDER_MODE_LIGHT,
                           FT_RENDER_MODE_MONO,
                           FT_RENDER_MODE_LCD,
                           FT_RENDER_MODE_LCD_V,
                           FT_RENDER_MODE_MAX);
   type FT_Render_Mode_array is array (C.Size_t range <>) of aliased FT_Render_Mode;

   -- FT_Outline
   --
   subtype FT_Outline       is System.Address;
   type    FT_Outline_array is array (C.Size_t range <>) of aliased FT_Outline;

   -- FT_LibraryRec
   --
   subtype FT_LibraryRec       is System.Address;
   type    FT_LibraryRec_array is array (C.Size_t range <>) of aliased FT_LibraryRec;

   -- FT_GlyphSlotRec
   --
   subtype FT_GlyphSlotRec       is System.Address;
   type    FT_GlyphSlotRec_array is array (C.Size_t range <>) of aliased FT_GlyphSlotRec;

   -- FT_FaceRec
   --
   subtype FT_FaceRec       is System.Address;
   type    FT_FaceRec_array is array (C.Size_t range <>) of aliased FT_FaceRec;


   -- FT_Kerning_Mode
   --
   type FT_Kerning_Mode is (FT_KERNING_DEFAULT,
                            FT_KERNING_UNFITTED,
                            FT_KERNING_UNSCALED);
   type FT_Kerning_Mode_array is array (C.Size_t range <>) of aliased FT_Kerning_Mode;


   -- FT_SizeRec
   --
   subtype FT_SizeRec       is System.Address;
   type    FT_SizeRec_array is array (C.Size_t range <>) of aliased FT_SizeRec;



private

   for FT_Render_Mode use (FT_RENDER_MODE_NORMAL => 0,
                           FT_RENDER_MODE_LIGHT  => 1,
                           FT_RENDER_MODE_MONO   => 2,
                           FT_RENDER_MODE_LCD    => 3,
                           FT_RENDER_MODE_LCD_V  => 4,
                           FT_RENDER_MODE_MAX    => 5);
   pragma Convention (C, FT_Render_Mode);


   for FT_Kerning_Mode use (FT_KERNING_DEFAULT  => 0,
                            FT_KERNING_UNFITTED => 1,
                            FT_KERNING_UNSCALED => 2);
   pragma Convention (C, FT_Kerning_Mode);


end Freetype_C;
