with
     Freetype_C,
     interfaces.C,
     ada.Containers.hashed_Maps;

limited
with
     freetype.Face;

private
with
     freetype_c.FT_Face,
     ada.unchecked_Conversion;


package freetype.charMap
--
--   'charMap' takes care of specifying the encoding for a font and mapping
--   character codes to glyph indices.
--
--   It doesn't preprocess all indices, only on an as needed basis. This may
--   seem like a performance penalty but it is quicker than using the 'raw'
--   freetype calls and will save significant amounts of memory when dealing
--   with unicode encoding
--
is

   type Item is tagged private;


   ---------
   --  Types
   --
   subtype GlyphIndex    is interfaces.c.long;
   subtype CharacterCode is interfaces.c.unsigned_long;

   function to_characterCode (From : in Character) return characterCode;



   ---------
   --  Forge
   --

   function  to_charMap (parent_Face : access freetype.Face.item'Class) return charMap.Item;
   procedure destruct   (Self        : in out Item);


   --------------
   --  Attributes
   --

   function Encoding (Self : in Item) return freeType_c.FT_Encoding;   -- Returns the current character map code.
   --
   --  Queries for the current character map code.



   function CharMap (Self : access Item;   encoding : in freeType_c.FT_Encoding)   -- The Freetype encoding symbol.
                     return Boolean;
   --
   --  Sets the character map for the face. If an error occurs the object is not modified.
   --
   --  Valid encodings as at Freetype 2.0.4
   --  - ft_encoding_none
   --  - ft_encoding_symbol
   --  - ft_encoding_unicode
   --  - ft_encoding_latin_2
   --  - ft_encoding_sjis
   --  - ft_encoding_gb2312
   --  - ft_encoding_big5
   --  - ft_encoding_wansung
   --  - ft_encoding_johab
   --  - ft_encoding_adobe_standard
   --  - ft_encoding_adobe_expert
   --  - ft_encoding_adobe_custom
   --  - ft_encoding_apple_roman
   --
   --  Returns True if charmap was valid and set correctly.


   function GlyphListIndex (Self : in Item;   character : in CharacterCode)
                            return GlyphIndex;
   --
   --  Get the Glyph Container index of the input character.
   --  Returns the FTGlyphContainer index for the character or zero if it wasn't found.
   --  'character'     The character code of the requested glyph in the current encoding (eg apple roman).


   function FontIndex (Self : in Item;   character : in characterCode)
                       return GlyphIndex;
   --
   --  Get the font glyph index of the input character.
   --  Returns the glyph index for the character.
   --  'character'     The character code of the requested glyph in the current encoding (eg apple roman).


   procedure InsertIndex (Self : in out Item;   character      : in characterCode;
                                                containerIndex : in ada.Containers.Count_type);
   --
   --  Set the FTGlyphContainer index of the character code.
   --  'character'          The character code of the requested glyph in the current encoding eg apple roman.
   --  'containerIndex'     The index into the Glyph Container of the character code.



   function Error (Self : in Item) return freeType_c.FT_Error;
   --
   --  Queries for errors.
   --  Returna the current error code. Zero means no error.



private

   function Hash is new ada.Unchecked_Conversion (CharacterCode, ada.containers.Hash_Type);
   use type CharacterCode, GlyphIndex;


   package char_Maps_of_glyph_index is new ada.containers.hashed_Maps (CharacterCode, GlyphIndex,
                                                                       Hash,          "=");
   --
   --  A structure that maps glyph indices to character codes


   MAX_PRECOMPUTED : constant := 128;

   type Cache is array (characterCode range 1 .. MAX_PRECOMPUTED) of freeType_c.FT_UInt;


   type Item is tagged
      record
         ftEncoding     : freeType_c.FT_Encoding;              -- Current character map code.
         ftFace         : freeType_c.FT_Face.item;             -- The current Freetype face.

         charMap        : char_Maps_of_glyph_index.Map;
         charIndexCache : Cache;                               -- Precomputed font indices.

         err            : freeType_c.FT_Error;                 -- Current error code.
      end record;

end freetype.charMap;

