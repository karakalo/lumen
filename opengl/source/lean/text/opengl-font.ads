with
     openGL.Glyph.Container,
     openGL.Math,
     openGL.FontImpl,

     freetype.Face,
     freetype_c.FT_GlyphSlot,

     Swig.Pointers;


package openGL.Font
--
--  Specific font classes are derived from this class. It uses the helper
--  classes freetype_c.Face and dreetype_c.FTSize to access the Freetype library.
--
--  This class is abstract and deriving classes must implement the protected
--  MakeGlyph function to create glyphs of the appropriate type.
--
is

   type Item is abstract tagged limited private;
   type View is access all Item'Class;


   --  Forge
   --

   procedure destruct (Self : in out Item);


   --  Attributes
   --

   function CharMap (Self : in Item;   encoding : in freetype_c.FT_Encoding)    -- Freetype enumerate for char map code.
                     return Boolean;
   --
   --  Set the character map for the face.
   --  Returns True if charmap was valid and set correctly.


   function CharMapCount (Self : in Item) return Natural;
   --
   --  Get the number of character maps in this face.
   --  Returns the character map count.


   function  CharMapList  (Self : access Item) return freetype.face.FT_Encodings_view;
   --
   --  Get a list of character maps in this face.
   --  Returns aceess to the array of encodings.


   function  Ascender   (Self : in     Item) return Real;
   --
   --  Get the global ascender height for the face.
   --  Returns the Ascender height.


   function  Descender  (Self : in     Item) return Real;
   --
   --  Gets the global descender height for the face.
   --  Returns the Descender height.


   function  LineHeight (Self : in     Item) return Real;
   --
   --  Gets the line spacing for the font.
   --  Returns the Line height.


   function  FaceSize (Self : access Item;   size         : in Natural;
                                             x_res, y_res : in Natural)
                       return Boolean;
   --
   --  Set the char size for the current face.
   --  Returns True if size was set correctly.


   function  FaceSize (Self : in     Item) return Natural;
   --
   --  Get the current face size in points (1/72 inch).
   --  Returns the face size.



   procedure Depth  (Self : in out Item;   depth : in Real);   -- The extrusion distance.
   --
   --  Set the extrusion distance for the font. Only implemented by FTExtrudeFont.


   procedure Outset (Self : in out Item;   outset : in Real);   -- The outset distance.
   --
   --  Set the outset distance for the font. Only implemented by FTOutlineFont, FTPolygonFont and FTExtrudeFont.


   procedure Outset (Self : in out Item;   front  : in Real;    -- The front outset distance.
                                           back   : in Real);   -- The back outset distance.
   --
   --  Set the front and back outset distances for the font. Only implemented by FTExtrudeFont.


   function  BBox    (Self : access Item;   s        : in String;
                                            len      : in Integer  := -1;
                                            Position : in Vector_3 := math.Origin_3d;
                                            Spacing  : in Vector_3 := math.Origin_3d)
                      return Bounds;
   --
   --  Get the bounding box for a string.
   --  Returns the corresponding bounding box.
   --  's'            A char buffer.
   --  'len'          The length of the string. If < 0 then all characters will be checked until
   --                 a null character is encountered
   --  'Position'     The pen position of the first character.
   --  'Spacing'      A displacement vector to add after each character has been checked.


   function Error (Self : in Item) return freetype_c.FT_Error;
   --
   --  Queries the Font for errors.
   --  Returns the current error code.



   --------------
   --  Operations
   --

   function Attach (Self : in Item;   fontFilePath : in String)    -- The auxilliary font file path.
                    return Boolean;
   --
   --  Attach auxilliary file to font e.g font metrics.
   --  Returns True if file has been attached successfully.
   --
   --  Note: not all font formats implement this function.


   function Attach (Self : in Item;   pBufferBytes      : in swig.Pointers.unsigned_char_Pointer;
                                      bufferSizeInBytes : in Natural)
                    return Boolean;
   --
   --  Attach auxilliary data to font e.g font metrics, from memory.
   --  Returns True if file has been attached successfully.
   --
   --  Note: not all font formats implement this function.
   --
   --  'pBufferBytes'          The in-memory buffer.
   --  'bufferSizeInBytes'     The length of the buffer in bytes.


   procedure GlyphLoadFlags (Self : in out Item;   flags : in freetype_c.FT_Int);      -- The glyph loading flags.
   --
   --  Set the glyph loading flags. By default, fonts use the most
   --  sensible flags when loading a font's glyph using FT_Load_Glyph().
   --  This function allows to override the default flags.


   function  Advance (Self : access Item;   s        : in String;
                                            len      : in Integer  := -1;
                                            Spacing  : in Vector_3 := math.Origin_3d)
                      return Real;
   --
   --  Get the advance for a string.
   --  Returns the string's advance width.
   --  's'           String to be checked.
   --  'len'         The length of the string. If < 0 then all characters will be checked until
   --                a null character is encountered
   --  'Spacing'     A displacement vector to add after each character has been checked.


   function  kern_Advance (Self : in Item;   From, To : in Character) return openGL.Real;

   function  x_PPEM       (Self : in Item) return openGL.Real;
   function  x_Scale      (Self : in Item) return openGL.Real;
   function  y_Scale      (Self : in Item) return openGL.Real;


   function  check_Glyphs  (Self : access Item;   s          : in String;
                                                  len        : in Integer             := -1;
                                                  Position   : in Vector_3            := math.Origin_3d;
                                                  Spacing    : in Vector_3            := math.Origin_3d;
                                                  Mode       : in fontImpl.RenderMode := fontImpl.RENDER_ALL)
                      return Vector_3;
   --
   --  Render a string of characters.
   --  Returns the new pen position after the last character was output.
   --  's'            String to be output.
   --  'len'          The length of the string. If < 0 then all characters will be displayed until
   --                 a null character is encountered
   --  'Position'     The pen position of the first character.
   --  'Spacing'      A displacement vector to add after each character has been displayed
   --  'Mode'         Render mode to use for display.


   function MakeGlyph (Self : access Item;   slot : in freetype_c.FT_GlyphSlot.item)     -- A FreeType glyph slot.
                       return glyph.Container.Glyph_view
                       is abstract;
   --
   --  Construct a glyph of the correct type.
   --
   --  Clients must override the function and return their specialised Glyph.
   --
   --  Returns an FTGlyph or null on failure.



private

   type Item is abstract tagged limited
      record
         Impl : FontImpl.view;      -- Internal FTGL FTFont implementation object. For private use only.
      end record;


   procedure define (Self : in out Item;   fontFilePath : in String);
   --
   --  Open and read a font file. Sets Error flag.


   procedure define (Self : in out Item;   pBufferBytes      : in swig.Pointers.unsigned_char_Pointer;
                                           bufferSizeInBytes : in Natural);
   --
   --  Open and read a font from a buffer in memory. Sets Error flag.
   --  The buffer is owned by the client and is NOT copied by FTGL. The pointer must be valid while using FTGL.


   procedure define (Self : in out Item;   pImpl : in FontImpl.view);
   --
   --  Internal FTGL FTFont constructor. For private use only.
   --
   --  'pImpl' is an internal implementation object, which will be destroyed upon FTFont deletion.


end openGL.Font;
