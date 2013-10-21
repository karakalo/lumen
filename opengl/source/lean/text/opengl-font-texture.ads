with
     openGL.Texture,
     openGL.GlyphImpl.texture;


package openGL.Font.texture
--
--  A Texture Font is a specialisation of the Font class for handling Texture mapped fonts.
--
is

   type Item is new Font.item with private;
   type View is access all Item'Class;


   ---------
   --  Forge
   --

   function to_Font_texture  (fontFilePath : in     String) return Font.texture.item;
   --
   --
   --  Open and read a font file. Sets Error flag.


   function new_Font_texture (fontFilePath : in     String) return Font.texture.view;


   function to_Font_texture  (pBufferBytes      : in     swig.Pointers.unsigned_char_Pointer;
                              bufferSizeInBytes : in     Natural)
                              return Font.texture.item;
   --
   --  Open and read a font from a buffer in memory. Sets Error flag.
   --
   --  The buffer is owned by the client and is NOT copied by FTGL. The
   --  pointer must be valid while using FTGL.
   --
   --  'pBufferBytes'          The in-memory buffer.
   --  'bufferSizeInBytes'     The length of the buffer in bytes.

   overriding
   procedure destruct (Self : in out Item);


   --------------
   --  Attributes
   --

   function gl_Texture (Self : in Item)                                 return opengl.Texture.texture_Name;
   function Quad       (Self : in Item;   for_Character : in Character) return opengl.GlyphImpl.Texture.Quad_t;



private

   type Item is new Font.item with null record;


   overriding
   function MakeGlyph (Self : access Item;   slot : in freetype_c.FT_GlyphSlot.item)
                       return glyph.Container.Glyph_view;
   --
   --  Construct a glyph of the correct type.
   --
   --  Clients must override the function and return their specialised FTGlyph.
   --  Returns an FTGlyph or null on failure.
   --  'slot'     A FreeType glyph slot.

end openGL.Font.texture;
