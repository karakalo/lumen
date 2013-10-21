with
     freeType_c.FT_GlyphSlot,
     openGL.GlyphImpl;


package openGL.Glyph
--
--  Glyph is the base class for openGL glyphs.
--
--  It provides the interface between Freetype glyphs and their openGL
--  renderable counterparts.
--
--  This is an abstract class and derived classes must implement the 'Render' function.
--
is

   type Item is abstract tagged private;


   --  Forge
   --
   procedure destruct (Self : in out Item);


   --  Attributes
   --
   function Advance (Self : in Item) return Real;                   -- Return the advance width for this glyph.
   function BBox    (Self : in Item) return Bounds;                 -- Return the bounding box for this glyph.
   function Error   (Self : in Item) return GlyphImpl.Error_Kind;   -- Return the current error code.


   --- Operations
   --
   function render (Self : in     Item;   Pen        : in Vector_3;                  -- The current pen position.
                                          renderMode : in Integer)                   -- Render mode to display.
                                                                   return Vector_3
                    is abstract;
   --
   --  Renders this glyph at the current pen position.
   --  Returns the advance distance for this glyph.



private

   type Item is abstract tagged
      record
         Impl : GlyphImpl.view;    -- Internal FTGL FTGlyph implementation object. For private use only.
      end record;


   procedure define (Self : in out Item;   glyth_Slot : in freetype_c.FT_GlyphSlot.item);
   --
   --  'glyth_Slot' is the Freetype glyph to be processed.


   procedure define (Self : in out Item;   pImpl : in openGL.GlyphImpl.view);
   --
   --  Internal FTGL FTGlyph constructor. For private use only.
   --  'pImpl' is an internal implementation object. Will be destroyed upon FTGlyph deletion.

end openGL.Glyph;
