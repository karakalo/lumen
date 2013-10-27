with
     ada.Unchecked_Deallocation,
     ada.Unchecked_Conversion;


package body openGL.Font
is

   ---------
   --  Forge
   --

   procedure define (Self : in out Item;   fontFilePath : in String)
   is
   begin
      Self.Impl := new FontImpl.item;
      Self.Impl.define (Self'Access, fontFilePath);
   end define;



   procedure define (Self : in out Item;   pBufferBytes      : in unsigned_char_Pointer;
                                           bufferSizeInBytes : in Natural)
   is
   begin
      Self.Impl := new FontImpl.item;
      Self.Impl.define (Self'Access, pBufferBytes, bufferSizeInBytes);
   end define;



   procedure define (Self : in out Item;   pImpl : in FontImpl.view)
   is
   begin
      Self.Impl := pImpl;
   end define;



   procedure destruct (Self : in out Item)
   is
      procedure free is new ada.Unchecked_Deallocation (FontImpl.item'Class,  FontImpl.view);
   begin
      free (Self.Impl);
   end destruct;



   --------------
   --  Attributes
   --

   function CharMap (Self : in Item;   encoding : in freetype_c.FT_Encoding)
                     return Boolean
   is
   begin
      return Self.impl.CharMap (encoding);
   end CharMap;



   function CharMapCount (Self : in Item) return Natural
   is
   begin
      return Self.impl.CharMapCount;
   end CharMapCount;



   function  CharMapList (Self : access Item) return freetype.face.FT_Encodings_view
   is
   begin
      return Self.impl.CharMapList;
   end CharMapList;



   function  Ascender   (Self : in     Item) return Real
   is
   begin
      return Self.impl.Ascender;
   end Ascender;



   function  Descender  (Self : in     Item) return Real
   is
   begin
      return Self.impl.Descender;
   end Descender;



   function  LineHeight (Self : in     Item) return Real
   is
   begin
      return Self.impl.LineHeight;
   end LineHeight;



   function  FaceSize (Self : access Item;   size          : in Natural;
                                             x_res, y_res  : in Natural)
                       return Boolean
   is
   begin
      return Self.impl.FaceSize (size, x_res, y_res);
   end FaceSize;



   function  FaceSize (Self : in     Item) return Natural
   is
   begin
      return Self.impl.FaceSize;
   end FaceSize;



   procedure Depth  (Self : in out Item;   depth  : in Real)
   is
   begin
      Self.impl.Depth (depth);
   end Depth;



   procedure Outset (Self : in out Item;   outset : in Real)
   is
   begin
      Self.impl.Outset (outset);
   end Outset;



   procedure Outset (Self : in out Item;   front  : in Real;
                                           back   : in Real)
   is
   begin
      Self.impl.Outset (front, back);
   end Outset;



   function  BBox    (Self : access Item;   s        : in String;
                                            len      : in Integer  := -1;
                                            Position : in Vector_3 := math.Origin_3d;
                                            Spacing  : in Vector_3 := math.Origin_3d)
                      return Bounds
   is
   begin
      return Self.impl.BBox (s, len, position, spacing);
   end BBox;



   function Error (Self : in Item) return freetype_c.FT_Error
   is
   begin
      return Self.impl.err;
   end Error;



   --------------
   --  Operations
   --

   function Attach (Self : in Item;   fontFilePath : in String)
                    return Boolean
   is
   begin
      return Self.Impl.Attach (fontFilePath);
   end Attach;



   function Attach (Self : in Item;   pBufferBytes      : in unsigned_char_Pointer;
                                      bufferSizeInBytes : in Natural)
                    return Boolean
   is
   begin
      return Self.Impl.Attach (pBufferBytes, bufferSizeInBytes);
   end Attach;



   procedure GlyphLoadFlags (Self : in out Item;   flags : in freetype_c.FT_Int)
   is
   begin
      Self.impl.GlyphLoadFlags (flags);
   end GlyphLoadFlags;



   function  Advance (Self : access Item;   s        : in String;
                                            len      : in Integer  := -1;
                                            Spacing  : in Vector_3 := math.Origin_3d)
                      return Real
   is
   begin
      return Self.impl.Advance (s, len, spacing);
   end Advance;



   function  kern_Advance (Self : in Item;   From, To : in Character) return openGL.Real
   is
   begin
      return Self.impl.kern_Advance (From, To);
   end kern_Advance;



   function x_PPEM (Self : in Item) return openGL.Real
   is
   begin
      return Self.impl.x_ppem;
   end x_PPEM;



   function x_Scale (Self : in Item) return openGL.Real
   is
   begin
      return Self.impl.x_Scale;
   end x_Scale;


   function y_Scale (Self : in Item) return openGL.Real
   is
   begin
      return Self.impl.y_Scale;
   end y_Scale;



   function check_Glyphs  (Self : access Item;   s          : in String;
                                                 len        : in Integer             := -1;
                                                 Position   : in Vector_3            := math.Origin_3d;
                                                 Spacing    : in Vector_3            := math.Origin_3d;
                                                 Mode       : in fontImpl.RenderMode := fontImpl.RENDER_ALL)
                           return Vector_3
   is
      function to_Integer is new ada.Unchecked_Conversion (fontImpl.RenderMode, Integer);
   begin
      return Self.impl.Render (s, len,
                               position, spacing,
                               to_Integer (Mode));
   end check_Glyphs;


end openGL.Font;
