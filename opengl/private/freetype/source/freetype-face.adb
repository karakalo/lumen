with
     freetype_C.Binding,
     freetype_C.FT_Library,
     freetype_C.FT_Vector,
     freetype_C.Pointers,

     interfaces.C.Strings,
     ada.unchecked_Conversion,
     ada.unchecked_Deallocation;


package body freetype.Face
is
   use Freetype_C,
       Interfaces;


   --- Globals
   --

   the_FT_Library : aliased FT_Library.item;



   --- Utility
   --

   function  to_Flag is new ada.Unchecked_Conversion   (FT_Kerning_Mode, C.unsigned);
   procedure free    is new ada.Unchecked_Deallocation (float_Array,     float_Array_view);



   --- Forge
   --

   function to_Face (fontFilePath      : in String;
                     precomputeKerning : in Boolean) return Face.item
   is
      use freetype_c.Binding, freetype_c.Pointers,
          Interfaces.C.Strings;
      use type freetype_c.FT_Long;

      Self               : Face.Item;
      the_font_Path      : chars_ptr := new_String (fontFilePath);
      DEFAULT_FACE_INDEX : constant  := 0;
   begin
      Self.numGlyphs := 0;
      Self.err       := 0;

      Self.ftFace := new_FT_Face (the_FT_Library, the_font_Path).all'Access;

      free (the_font_Path);

      if Self.ftFace = null then
         raise freetype.Error with "failed to create a freeType Face";
      end if;

      Self.numGlyphs       := Integer (FT_Face_Get_num_glyphs (Self.ftFace));
      Self.hasKerningTable := FT_Face_HAS_KERNING (Self.ftFace) /= 0;

      if         Self.hasKerningTable
        and then precomputeKerning
      then
         Self.BuildKerningCache;
      end if;

      return Self;
   end to_Face;



   function to_Face (pBufferBytes      : access interfaces.c.unsigned_char;
                     bufferSizeInBytes : in     Positive;
                     precomputeKerning : in     Boolean) return Face.item
   is
      use freetype_c.Binding,
          freetype_c.Pointers;
      use type FT_Long;

      Self               : Face.Item;
      DEFAULT_FACE_INDEX : constant := 0;
   begin
      Self.numGlyphs := 0;
      Self.err       := 0;

      Self.ftFace := new_FT_Memory_Face (the_FT_Library,
                                         pBufferBytes.all'Access,
                                         C.int (bufferSizeInBytes)).all'Access;
      if Self.ftFace = null then
         raise freetype.Error with "failed to create a freeType memory Face";
      end if;

      Self.numGlyphs       := Integer (FT_Face_Get_num_glyphs (Self.ftFace));
      Self.hasKerningTable := FT_Face_HAS_KERNING (Self.ftFace) /= 0;

      if    Self.hasKerningTable
        and precomputeKerning
      then
         Self.BuildKerningCache;
      end if;

      return Self;
   end to_Face;



   procedure destruct (Self : in out Item)
   is
      use freetype_c.Binding;
      use type Pointers.FT_FaceRec_Pointer;
   begin
      if Self.kerningCache /= null then
         free (Self.kerningCache);
      end if;

      if Self.ftFace /= null then
         Self.err    := FT_Done_Face (Self.ftFace);
         Self.ftFace := null;
      end if;
   end destruct;



   function attach (Self : access Item;   fontFilePath : in String) return Boolean
   is
      use freetype_c.Binding,
          Interfaces.C.Strings;
      use type FT_Error;

      the_font_Path : chars_ptr := new_String (fontFilePath);
   begin
      Self.err := FT_Attach_File (Self.ftFace,  the_font_Path);
      free (the_font_Path);

      return Self.err = 0;
   end attach;



   function Attach (Self : access Item;   pBufferBytes      : access C.unsigned_char;
                                          bufferSizeInBytes : in     Positive)
                    return Boolean
   is
      use freetype_c.Binding;
      use type FT_Error;
   begin
      Self.err := FT_Face_Attach_Stream (Self.ftFace,
                                         pBufferBytes.all'Access,
                                         C.size_t (bufferSizeInBytes));
      return Self.err = 0;
   end Attach;



   function freetype_Face (Self : in Item) return FT_Face.item
   is
   begin
      return Self.ftFace;
   end freetype_Face;



   function Size (Self : access Item;   size         : in Natural;
                                        x_res, y_res : in Natural)
                  return freetype.face_Size.item
   is
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      Success  := Self.charSize.CharSize (Self.ftFace,
                                          size,
                                          x_res, y_res);
      Self.err := Self.charSize.Error;

      return Self.charSize;
   end Size;



   function CharMapCount (Self : in Item) return Natural
   is
      use freetype_C.Binding;
   begin
      return Natural (FT_Face_Get_num_charmaps (Self.ftFace));
   end CharMapCount;



   function CharMapList  (Self : access Item) return FT_Encodings_view
   is
      use freetype_C.Binding;
   begin
      if Self.fontEncodingList = null
      then
         Self.fontEncodingList := new FT_Encodings (1 .. Self.CharMapCount);

         for i in 1 .. Self.CharMapCount
         loop
            Self.fontEncodingList (i) := FT_Face_Get_charmap_at (Self.ftFace,
                                                                 C.int (i)).encoding;
         end loop;
      end if;

      return Self.fontEncodingList;
   end CharMapList;



   function KernAdvance (Self : access Item;   index1 : in Natural;
                                               index2 : in Natural) return Vector_3
   is
      use freetype_c.Binding;
      use type FT_Error;

      x, y        :         Float;
      kernAdvance : aliased FT_Vector.item;

   begin
      if   not Self.hasKerningTable
        or index1 = 0
        or index2 = 0
      then
         return (0.0, 0.0, 0.0);
      end if;

      if    Self.kerningCache /= null
        and index1 < MAX_PRECOMPUTED
        and index2 < MAX_PRECOMPUTED
      then
         declare
            max_Index         : C.ptrdiff_t      := C.ptrdiff_t (2 * (index2 * MAX_PRECOMPUTED + index1) + 1);
--              the_kerning_Cache : float_Array := Value (Self.kerningCache, max_Index);
         begin
            x := Float (Self.kerningCache (C.size_t (2 * (index2 * MAX_PRECOMPUTED + index1))));
            y := Float (Self.kerningCache (C.size_t (2 * (index2 * MAX_PRECOMPUTED + index1) + 1)));

            return (x, y, 0.0);
         end;
      end if;

      kernAdvance.x := 0;
      kernAdvance.y := 0;

      Self.err := FT_Get_Kerning (Self.ftFace,
                                  C.unsigned (index1),
                                  C.unsigned (index2),
                                  to_Flag (ft_kerning_unfitted),
                                  kernAdvance'unchecked_Access);
      if Self.err /= 0 then
         return (0.0, 0.0, 0.0);
      end if;

      x := Float (kernAdvance.x) / 64.0;
      y := Float (kernAdvance.y) / 64.0;

      return (x, y, 0.0);
   end KernAdvance;



   function GlyphCount  (Self : in Item) return Natural
   is
   begin
      return Self.numGlyphs;
   end GlyphCount;



   function Glyph (Self : access Item;   index      : in freetype.charMap.glyphIndex;
                                         load_flags : in freetype_c.FT_Int) return FT_GlyphSlot.item
   is
      use freetype_C.Binding;
      use type FT_Error,
               FT_Face.item;
   begin
      if Self.ftFace = null then
         raise Program_Error;   -- todo: Ese better exception.
      end if;

      Self.err := FT_Load_Glyph (Self.ftFace,  FT_UInt (index),  load_flags);

      if Self.err /= 0 then
         return null;
      end if;

      return FT_GlyphSlot.item (FT_Face_Get_glyph (Self.ftFace));
   end Glyph;



   function Error (Self : in Item) return FT_Error
   is
   begin
      return Self.err;
   end Error;



   procedure BuildKerningCache (Self : in out Item)
   is
      use freetype_c.Binding;
      use type FT_UInt,
               FT_Error,
               C.C_float;

      max_Index   : constant C.ptrdiff_t   := C.ptrdiff_t (MAX_PRECOMPUTED * MAX_PRECOMPUTED * 2);
      kernAdvance : aliased  FT_Vector.item;
   begin
      kernAdvance.x := 0;
      kernAdvance.y := 0;

      Self.kerningCache := new float_Array' (1 .. C.size_t (max_Index) => <>);

      for j in 1 .. FT_UInt'(MAX_PRECOMPUTED)
      loop
         for i in 1 .. FT_UInt'(MAX_PRECOMPUTED)
         loop
            Self.err := FT_Get_Kerning (Self.ftFace,
                                        i, j,
                                        to_Flag (ft_kerning_unfitted),
                                        kernAdvance'Unchecked_Access);
            if Self.err /= 0 then
               free (Self.kerningCache);
               return;
            end if;

            Self.kerningCache (C.size_t (2 * (j * MAX_PRECOMPUTED + i)    )) := C.C_float (kernAdvance.x) / 64.0;
            Self.kerningCache (C.size_t (2 * (j * MAX_PRECOMPUTED + i) + 1)) := C.C_float (kernAdvance.y) / 64.0;
         end loop;
      end loop;

   end BuildKerningCache;



   --------------------------
   --  Package Initialisation
   --
   use freeType_c.Binding;

   Status : FT_Error;
   pragma Unreferenced (Status);

begin
   Status := FT_Init_FreeType (the_FT_Library'Access);
end freetype.Face;
