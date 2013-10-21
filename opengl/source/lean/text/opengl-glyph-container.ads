with
     freetype.Face,
     freetype.charMap;

private
with
     ada.Containers.Vectors;


package openGL.Glyph.Container
--
--  Contains the post processed Glyph objects.
--
is

   type Item       is tagged private;
   type Glyph_view is access all openGL.Glyph.item'Class;


   ---------
   --  Forge
   --

   function  to_glyph_Container (parent_Face : access freetype.Face.Item'Class)     --  The Freetype face.
                                 return glyph.Container.item;

   procedure destruct (Self : in out Item);



   --------------
   --  Attributes
   --

   function CharMap   (Self : access Item;   encoding : in freeType_c.FT_Encoding)   -- The Freetype encoding symbol.
                       return Boolean;
   --
   --  Sets the character map for the face.
   --  Returns True if charmap was valid and set correctly.


   function FontIndex (Self : in Item;   character : in freetype.charMap.characterCode)
                       return Natural;
   --
   --  Get the font index of the input character.
   --  Returns the font index for the character.
   --  'character'     The character code of the requested glyph in the current encoding (eg apple roman).


   procedure Add      (Self : in out Item;   glyph     : in Glyph_view;
                                             character : in freetype.charMap.characterCode);
   --
   --  Adds a glyph to this glyph list.
   --  'glyph'         The FTGlyph to be inserted into the container.
   --  'character'     The char code of the glyph NOT the glyph index.


   function  Glyph    (Self : in Item;   character : in freetype.charMap.characterCode)
                       return Glyph_view;
   --
   --  Get a glyph from the glyph list.
   --  Returns a Glyph or null is it hasn't been loaded.
   --  'character'     The char code of the glyph NOT the glyph index.


   function BBox (Self : in Item;   character : in freetype.charMap.characterCode)
                  return openGL.Bounds;
   --
   --  Get the bounding box for a character.
   --  'character'     The char code of the glyph NOT the glyph index.


   function Advance (Self : in Item;   character         : in freetype.charMap.characterCode;
                                       nextCharacterCode : in freetype.charMap.characterCode)
                     return Real;
   --
   --  Returns the kerned advance width for a glyph.
   --  'character'             Glyph index of the character.
   --  'nextCharacterCode'     The next glyph in a string.


   function Error (Self : in Item) return freetype_c.FT_Error;
   --
   --  Queries the glyph container for errors.
   --  Returns the current error code.



   --------------
   --  Operations
   --

   function Render (Self : access Item;   character         : in freetype.charMap.characterCode;
                                          nextCharacterCode : in freetype.charMap.characterCode;
                                          penPosition       : in Vector_3;
                                          renderMode        : in Integer)
                    return Vector_3;
   --
   --  Renders a character.
   --  Returns the distance to advance the pen position after rendering,
   --  'character'             The glyph to be Rendered.
   --  'nextCharacterCode'     The next glyph in the string. Used for kerning.
   --  'penPosition'           The position to Render the glyph.
   --  'renderMode'            Render mode to display.



private

   type    charMap_view  is access all freetype.charMap.item'class;
   package glyph_Vectors is new ada.Containers.Vectors (Positive, Glyph_view);


   type Item is tagged
      record
         face    : access freetype.Face.item'Class;    -- The FTGL face.
         charMap : charMap_view;                       -- The Character Map object associated with the current face.

         Glyphs  : glyph_Vectors.Vector;               -- A structure to hold the glyphs.
         err     : freeType_c.FT_Error;                -- Current error code. Zero means no error.
      end record;

end openGL.Glyph.Container;
