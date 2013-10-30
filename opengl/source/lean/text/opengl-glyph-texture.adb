package body openGL.Glyph.texture
is

   ---------
   --  Forge
   --

   function to_Glyph (glyth_Slot       : in freetype_c.FT_GlyphSlot.item;
                      texture_id       : in openGL.Texture.texture_Name;
                      xOffset, yOffset : in Integer;
                      width, height    : in Integer) return Glyph.Texture.item
   is
      Self : Glyph.Texture.item;
      Impl : constant GlyphImpl.Texture.view := GlyphImpl.texture.new_GlyphImpl (glyth_Slot,
                                                                                 texture_id,
                                                                                 xOffset, yOffset,
                                                                                 width,   height);
   begin
      Self.define (Impl.all'Access);
      return Self;
   end to_Glyph;



   function new_Glyph (glyth_Slot       : in freetype_c.FT_GlyphSlot.item;
                       texture_id       : in openGL.Texture.texture_Name;
                       xOffset, yOffset : in Integer;
                       width, height    : in Integer) return access Glyph.Texture.item'Class
   is
   begin
      return new Glyph.texture.item'(to_Glyph (glyth_Slot, texture_id, xOffset, yOffset, width, height));
   end new_Glyph;




   --------------
   --  Attributes
   --

   function Quad (Self : in Item;   Pen : in Vector_3) return openGL.GlyphImpl.Texture.Quad_t
   is
   begin
      return opengl.GlyphImpl.texture.view (Self.Impl).Quad (Pen);
   end Quad;



   --------------
   --  Operations
   --

   overriding function render (Self : in Item;   Pen        : in Vector_3;
                                                 renderMode : in Integer) return Vector_3
   is
   begin
      return GlyphImpl.Texture.view (Self.Impl).renderImpl (Pen, renderMode);
   end render;


end openGL.Glyph.Texture;
