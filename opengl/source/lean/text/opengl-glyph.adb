with
     ada.Unchecked_Deallocation;

package body openGL.Glyph
is

   ---------
   --  Forge
   --

   procedure define (Self : in out Item;   glyth_Slot : in freetype_c.FT_GlyphSlot.item)
   is
   begin
      Self.Impl := new GlyphImpl.item;
      Self.Impl.define (glyth_Slot);
   end define;



   procedure define (Self : in out Item;   pImpl : in openGL.GlyphImpl.view)
   is
   begin
      Self.Impl := pImpl;
   end define;



   procedure destruct (Self : in out Item)
   is
      procedure deallocate is new ada.Unchecked_Deallocation (openGL.GlyphImpl.item'Class, openGL.GlyphImpl.view);
   begin
      deallocate (Self.Impl);
   end destruct;



   --------------
   --  Attributes
   --

   function Advance (Self : in Item) return Real
   is
   begin
      return Self.Impl.Advance;
   end Advance;



   function BBox    (Self : in Item) return Bounds
   is
   begin
      return Self.Impl.BBox;
   end BBox;



   function Error (Self : in Item) return GlyphImpl.Error_Kind
   is
   begin
      return Self.Impl.Error;
   end Error;


end openGL.Glyph;
