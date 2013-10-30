with
     freetype.Face,
     freetype_C.Binding,
     freetype_C.FT_CharMapRec;


package body freetype.charMap
is
   use FreeType_c;


   -----------
   --  Utility
   --

   function to_characterCode (From : in Character) return characterCode
   is
   begin
--        if From = Character'Val (0) then
--           return 0;
--        else
         return Character'Pos (From) + 1;
--        end if;
   end to_characterCode;




   ---------
   --  Forge
   --

   function to_charMap (parent_Face : access Face.item'Class) return freetype.charMap.item
   is
      use freetype_c.Binding;
      use type FT_int,
               FT_CharMapRec.item;

      Self : freetype.charMap.item;
   begin
      Self.ftFace := parent_Face.freetype_Face;
      Self.err    := 0;

      if FT_Face_Get_charmap (Self.ftFace) = null
      then
         if FT_Face_Get_num_charmaps (Self.ftFace) = 0
         then
            Self.err := 16#96#;
            return Self;
         end if;

         Self.err := FT_Set_Charmap (Self.ftFace,
                                     FT_Face_Get_charmap_at (Self.ftFace, 0).all'Access);
      end if;


      Self.ftEncoding := FT_Face_Get_charmap (Self.ftFace).encoding;

      for i in characterCode'(1) .. MAX_PRECOMPUTED
      loop
         Self.charIndexCache (i) := FT_Get_Char_Index (Self.ftFace,  FT_ULong (i - 1));
      end loop;

      return Self;
   end to_charMap;



   procedure destruct (Self : in out Item)
   is
   begin
      Self.charMap.clear;
   end destruct;



   --------------
   --  Attributes
   --

   function Encoding (Self : in Item) return FT_Encoding
   is
   begin
      return Self.ftEncoding;
   end Encoding;



   function CharMap (Self : access Item;   encoding : in FT_Encoding)
                     return Boolean
   is
      use freetype_c.Binding;
      use type FT_Encoding,
               FT_Error;
   begin
      if Self.ftEncoding = encoding then
         Self.err := 0;
         return True;
      end if;

      Self.err := FT_Select_Charmap (Self.ftFace, encoding);

      if Self.err = 0 then
         Self.ftEncoding := encoding;
         Self.charMap.clear;
      end if;

      return Self.err = 0;
   end CharMap;



   function GlyphListIndex (Self : in Item;   character : in CharacterCode) return GlyphIndex
   is
   begin
      return Self.charMap.Element (character);

   exception
      when Constraint_Error =>
         return -1;
   end GlyphListIndex;



   function FontIndex (Self : in Item;   character : in characterCode) return GlyphIndex
   is
      use freetype_C.Binding;
   begin
      if character < MAX_PRECOMPUTED then
         return GlyphIndex (Self.charIndexCache (character));
      end if;

      return GlyphIndex (FT_Get_Char_Index (Self.ftFace,
                                            character));
   end FontIndex;



   procedure InsertIndex (Self : in out Item;   character      : in characterCode;
                                                containerIndex : in ada.Containers.Count_type)
   is
   begin
      Self.charMap.insert (character,
                           GlyphIndex (containerIndex));
   end InsertIndex;



   function Error (Self : in Item) return FT_Error
   is
   begin
      return Self.err;
   end Error;


end freetype.charMap;
