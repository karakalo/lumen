with
     openGL.Texture,
     openGL.GlyphImpl.Texture,
     freetype_c.FT_GlyphSlot;


package openGL.Glyph.Texture
--
--  A specialisation of Glyph for creating texture glyphs.
--
is

   type Item is new Glyph.item with private;


   -----------
   --   Forge
   --

   function to_Glyph (glyth_Slot       : in freetype_c.FT_GlyphSlot.item;
                      texture_id       : in openGL.Texture.texture_Name;
                      xOffset, yOffset : in Integer;
                      width, height    : in Integer)
                      return Glyph.Texture.item;
   --
   --  'glyth_Slot'           The Freetype glyph to be processed.
   --  'texture_id'           The id of the texture that this glyph will be drawn in.
   --  'xOffset, yOffset'     The x,y offset into the parent texture to draw this glyph.
   --  'width, height'        The width and height (number of rows) of the parent texture.


   function new_Glyph (glyth_Slot       : in freetype_c.FT_GlyphSlot.item;
                       texture_id       : in openGL.Texture.texture_Name;
                       xOffset, yOffset : in Integer;
                       width, height    : in Integer)
                       return access Glyph.Texture.item'Class;
   --
   --  'glyth_Slot'           The Freetype glyph to be processed.
   --  'texture_id'           The id of the texture that this glyph will be drawn in.
   --  'xOffset, yOffset'     The x,y offset into the parent texture to draw this glyph.
   --  'width, height'        The width and height (number of rows) of the parent texture.


   --------------
   --  Attributes
   --

   function Quad (Self : in     Item;   Pen : in Vector_3)  return openGL.GlyphImpl.Texture.Quad_t;



   ---------------
   --  Operations
   --

   overriding
   function render (Self : in     Item;   Pen        : in Vector_3;   -- The current pen position.
                                          renderMode : in Integer)    -- Render mode to display.
                    return Vector_3;
   --
   --  Render this glyph at the current pen position.
   --  Returns the advance distance for this glyph.



private

   type Item is new Glyph.item with
      record
         null;
      end record;

end openGL.Glyph.Texture;
