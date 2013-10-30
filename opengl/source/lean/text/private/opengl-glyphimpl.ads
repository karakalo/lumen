with
     freetype_c.FT_GlyphSlot;

package openGL.GlyphImpl
--
--  Implements an openGL glyph.
--
is

   type Item is tagged private;
   type View is access all Item'Class;


   ---------
   --  Types
   --
   type Error_Kind is (no_Error);


   ---------
   --  Forge
   --
   procedure define (Self : in out Item;   glyth_Slot : in freetype_c.FT_GlyphSlot.item);
   --
   --  'glyth_Slot' is the Freetype glyph to be processed.


   --------------
   --  Attributes
   --
   function Advance (Self : in Item) return Real;         -- The advance distance for this glyph.
   function BBox    (Self : in Item) return Bounds;       -- Return the bounding box for this glyph.
   function Error   (Self : in Item) return Error_Kind;   -- Return the current error code.



private

   type Item is tagged
      record
         advance : Vector_3;
         bBox    : Bounds;
         err     : error_Kind;
      end record;

   procedure destruct (Self : in out Item);

end openGL.GlyphImpl;
