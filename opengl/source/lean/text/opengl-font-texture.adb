with
     openGL.FontImpl.Texture;


package body openGL.Font.texture
is

   ---------
   --  Forge
   --

   function to_Font_texture (fontFilePath : in     String) return Font.texture.item
   is
   begin
      return Self : Font.texture.item
      do
         Self.define (fontImpl.Texture.new_FontImpl_texture (Self'Access,
                                                             fontFilePath).all'Access);
      end return;
   end to_Font_texture;



   function new_Font_texture (fontFilePath : in     String) return Font.texture.view
   is
      Self : constant Font.texture.view := new Font.texture.item;
   begin
      Self.define (fontImpl.Texture.new_FontImpl_texture (Self,
                                                          fontFilePath).all'Access);
      return Self;
   end new_Font_texture;



   function to_Font_texture (pBufferBytes      : in     unsigned_char_Pointer;
                             bufferSizeInBytes : in     Natural)
     return Font.texture.item
   is
   begin
      return Self : Font.texture.item
      do
         Self.define (fontImpl.Texture.new_FontImpl_texture (Self'Access,
                                                             pBufferBytes,
                                                             bufferSizeInBytes).all'Access);
      end return;
   end to_Font_texture;



   overriding
   procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;



   --------------
   --  Attributes
   --

   function gl_Texture (Self : in Item) return opengl.Texture.texture_Name
   is
   begin
      return fontImpl.texture.view (Self.Impl).gl_Texture;
   end gl_Texture;



   function Quad (Self : in Item;   for_Character : in Character) return opengl.GlyphImpl.Texture.Quad_t
   is
   begin
      return fontImpl.Texture.View (Self.Impl).Quad (for_Character);
   end Quad;



   --------------
   --  Operations
   --

   overriding
   function MakeGlyph (Self : access Item;   slot : in freetype_c.FT_GlyphSlot.item) return glyph.Container.Glyph_view
   is
      type FontImpl_texture_view is access all FontImpl.texture.Item'Class;

      myimpl : constant FontImpl_texture_view := FontImpl_texture_view (Self.impl);
   begin
      if myimpl = null then
         return null;
      end if;

      return myimpl.MakeGlyphImpl (slot).all'Access;
   end MakeGlyph;


end openGL.Font.texture;
