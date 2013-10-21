with
     openGL.Math,
     ada.Unchecked_Deallocation,
     Ada.Text_IO,
     Interfaces;


package body openGL.Glyph.Container
is
   use Ada.Text_IO;


   --- Forge
   --

   function  to_glyph_Container (parent_Face : access freetype.Face.Item'Class) return openGL.glyph.Container.item
   is
      Self : opengl.glyph.Container.item;
   begin
      Self.face    := parent_Face;
      Self.err     := 0;
      Self.charMap := new freetype.charMap.Item' (freetype.charMap.to_charMap (Self.face));
--        Self.Glyphs.append (null);

      return Self;
   end to_glyph_Container;



   procedure destruct (Self : in out Item)
   is
      use glyph_Vectors;

      procedure free is new ada.unchecked_Deallocation (openGL  .Glyph  .item'Class, Glyph_view);
      procedure free is new ada.unchecked_Deallocation (freetype.charMap.item'Class, charMap_view);

      Cursor    : glyph_Vectors.Cursor := Self.Glyphs.First;
      the_Glyph : Glyph_view;
   begin
      while has_Element (Cursor)
      loop
         the_Glyph := Element (Cursor);
         free (the_Glyph);
         next (Cursor);
      end loop;

      Self.Glyphs.clear;
      free (Self.charMap);
   end destruct;



   --------------
   --  Attributes
   --

   function CharMap (Self : access Item;   encoding : in freeType_c.FT_Encoding) return Boolean
   is
      result : constant Boolean := Self.charMap.CharMap (encoding);
   begin
      Self.err := Self.charMap.Error;
      return result;
   end CharMap;



   function FontIndex (Self : in Item;   character : in freetype.charMap.characterCode) return Natural
   is
   begin
      return Natural (Self.charMap.FontIndex (character));
   end FontIndex;



   procedure Add (Self : in out Item;   glyph     : in Glyph_view;
                                        character : in freetype.charMap.characterCode)
   is
   begin
      Self.glyphs.append       (Glyph);
      Self.charMap.InsertIndex (character, Self.glyphs.length);
   end Add;



   function  Glyph (Self : in Item;   character : in freetype.charMap.characterCode) return Glyph_view
   is
      use type freetype.charMap.glyphIndex;
      index : constant freetype.charMap.glyphIndex := Self.charMap.GlyphListIndex (character);
   begin
      if index = -1 then
         return null;
      else
         return Self.glyphs.Element (Integer (index));
      end if;
   end Glyph;



   function BBox (Self : in Item;   character : in freetype.charMap.characterCode) return openGL.Bounds
   is
   begin
      return Self.Glyph (character).BBox;
   end BBox;



   function Advance (Self : in Item;   character         : in freetype.charMap.characterCode;
                                       nextCharacterCode : in freetype.charMap.characterCode) return Real
   is
      use type Real;
      left  : constant freetype.charMap.glyphIndex := Self.charMap.FontIndex (character);
      right : constant freetype.charMap.glyphIndex := Self.charMap.FontIndex (nextCharacterCode);
   begin
      return Real (Self.face.KernAdvance (Integer (left),
                                          Integer (right)) (1)  +  Float (Self.Glyph (character).Advance));
   end Advance;



   function Error (Self : in Item) return freetype_c.FT_Error
   is
   begin
      return Self.err;
   end Error;



   --------------
   --  Operations
   --

   function Render (Self : access Item;   character         : in freetype.charMap.characterCode;
                                          nextCharacterCode : in freetype.charMap.characterCode;
                                          penPosition       : in Vector_3;
                                          renderMode        : in Integer) return Vector_3
   is
      use      openGL.Math;
      use type freetype_c.FT_Error,
               freetype.charMap.glyphIndex;

      left           : constant freetype.charMap.glyphIndex := Self.charMap.FontIndex (character)         - 0;
      right          : constant freetype.charMap.glyphIndex := Self.charMap.FontIndex (nextCharacterCode) - 0;

      ft_kernAdvance :          freetype.Vector_3           := Self.face.KernAdvance (Integer (left),
                                                                                      Integer (right));
      kernAdvance    :          Vector_3                    := (Real (ft_kernAdvance (1)),
                                                                Real (ft_kernAdvance (2)),
                                                                Real (ft_kernAdvance (3)));
      index          : freetype.charMap.glyphIndex;
   begin
      if Self.face.Error = 0
      then
         index       := Self.charMap.GlyphListIndex (character);
         kernAdvance := kernAdvance + Self.glyphs.Element (Integer (index)).Render (penPosition,
                                                                                    renderMode);
      else
         put_Line ("FACE ERROR");
      end if;

      return kernAdvance;
   end Render;


end openGL.Glyph.Container;
