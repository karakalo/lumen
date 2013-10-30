with
     freetype.face_Size,
     freetype.charMap,

     freetype_c.FT_Face,
     freetype_c.FT_GlyphSlot,

     interfaces.C;


package freetype.Face
--
--  The Face class provides an abstraction layer for the Freetype Face.
--
is

   type Item is tagged private;



   ---------
   --  Types
   --
   type FT_Encodings is array (Positive range <>) of freetype_c.FT_Encoding;
   type FT_Encodings_view is access all FT_Encodings;



   ---------
   --  Forge
   --

   function to_Face (fontFilePath      : in String;
                     precomputeKerning : in Boolean) return Face.item;
   --
   --  Opens and reads a face file. Error is set.


   function to_Face (pBufferBytes      : access interfaces.c.unsigned_char;   -- The in-memory buffer.
                     bufferSizeInBytes : in     Positive;                     -- The length of the buffer in bytes.
                     precomputeKerning : in     Boolean) return Face.item;
   --
   --  Read face data from an in-memory buffer. Error is set.


   procedure destruct (Self : in out Item);   -- Disposes of the current Freetype Face.



   --------------
   --  Attributes
   --

   function Attach (Self : access Item;   fontFilePath : in String)    -- Auxilliary font file path.
                    return Boolean;                                    -- Returns true if file has opened successfully.
   --
   --  Attach auxilliary file to font (e.g., font metrics).


   function Attach (Self : access Item;   pBufferBytes      : access interfaces.c.unsigned_char;
                                          bufferSizeInBytes : in     Positive)
                    return Boolean;
   --
   --  Attach auxilliary data to font (e.g., font metrics) from memory.
   --  Returns true if file has opened successfully.
   --  'pBufferBytes'          The in-memory buffer.
   --  'bufferSizeInBytes'     The length of the buffer in bytes.


   function freetype_Face (Self : in Item) return freetype_c.FT_Face.item;
   --
   --  Get the freetype face object.
   --  Returns a pointer to an FT_Face.


   function Size (Self : access Item;   size         : in Natural;   -- The face size in points (1/72 inch)
                                        x_res, y_res : in Natural)   -- The resolution of the target device.
                  return freetype.face_Size.item;
   --
   --  Sets the char size for the current face.
   --
   --  This doesn't guarantee that the size was set correctly. Clients should check errors.
   --  Returns FTSize object.


   function CharMapCount (Self : in Item) return Natural;            -- Return character map count.
   --
   --  Get the number of character maps in this face.


   function CharMapList  (Self : access Item) return FT_Encodings_view;
   --
   --  Get a list of character maps in this face.
   --  Returns a pointer to the first encoding.


   function KernAdvance (Self : access Item;   index1 : in Natural;
                                               index2 : in Natural) return Vector_3;
   --
   --  Gets the kerning vector between two glyphs.


   function GlyphCount (Self : in Item) return Natural;
   --
   --  Gets the number of glyphs in the current face.


   function Glyph (Self : access Item;   index      : in freetype.charMap.glyphIndex;
                                         load_flags : in freetype_c.FT_Int) return freetype_c.FT_GlyphSlot.item;


   function Error (Self : in Item) return freetype_c.FT_Error;   -- return  The current error code.
   --
   --  Queries for errors.



private

   use Freetype_C,
       Interfaces;

   type float_Array      is array (C.size_t range <>) of aliased C.c_Float;
   type float_Array_view is access all float_Array;

   type Item is tagged
      record
        ftFace           :         FT_Face  .item;                  -- The Freetype face
        charSize         : aliased face_Size.item;                  -- The size object associated with this face
        numGlyphs        :         Natural;                         -- The number of glyphs in this face
        fontEncodingList :         FT_Encodings_view;
        hasKerningTable  :         Boolean;                         -- This face has kerning tables
        kerningCache     :         float_Array_view;                -- If this face has kerning tables, we can cache them.
        err              :         FT_Error;                        -- Current error code. Zero means no error.
      end record;


      MAX_PRECOMPUTED : constant := 128;

      procedure BuildKerningCache (Self : in out Item);


end freetype.Face;
