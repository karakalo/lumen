with
     GL.Lean,
     GL.Pointers,

     interfaces.c.Pointers,
     ada.unchecked_Conversion,
     ada.unchecked_Deallocation;


package body GLU
--
--  This is a direct port of parts of Mesa GLU 'mipmap.c' file.
--
--  Only declarations involved in 'gluScaleImage' are currently ported.
--  Other areas may be later ported at need.
--
--  Currently supports only GL datatypes allowed in the 'lean' profile.
is
   use GL, lean,
       Interfaces;

   use type GLint,
            GLenum,
            GLfloat;


   --  GLubyte
   --
   type GLubtye_array is array (C.size_t range <>) of aliased GLubyte;

   package GLubyte_Pointers is new C.Pointers (Index              => C.size_t,
                                               Element            => GLubyte,
                                               Element_Array      => GLubtye_array,
                                               Default_Terminator => 0);
   subtype GLubyte_view is GLubyte_Pointers.Pointer;

   function to_GLubyte_view is new ada.Unchecked_Conversion (system.Address, GLubyte_view);



   --  GLushort
   --
   package GLushort_Pointers is new C.Pointers (Index              => C.size_t,
                                                Element            => GLushort,
                                                Element_Array      => GLushort_array,
                                                Default_Terminator => 0);

   subtype GLushort_view is GLushort_Pointers.Pointer;

   function to_GLushort_view is new ada.Unchecked_Conversion (system.Address, GLushort_view);
   function to_GLushort_view is new ada.Unchecked_Conversion (GLubyte_view,   GLushort_view);

   type GLushort_array_view is access all GLushort_array;



   --  GLbyte
   --
   type GLbyte_view is access all GLbyte;
   function to_GLbyte_view is new ada.Unchecked_Conversion (GLubyte_view, GLbyte_view);



   --  Pixel storage modes
   --
   type PixelStorageModes is
      record
         pack_alignment,
         pack_row_length,
         pack_skip_rows,
         pack_skip_pixels,
         pack_lsb_first,
         pack_swap_bytes,
         pack_skip_images,
         pack_image_height,

         unpack_alignment,
         unpack_row_length,
         unpack_skip_rows,
         unpack_skip_pixels,
         unpack_lsb_first,
         unpack_swap_bytes,
         unpack_skip_images,
         unpack_image_height : aliased GLint;
      end record;


   -- Type_Widget
   --
   type widget_Kind is (ub, us, ui, b, s, i, f);

   type uchar_array is array (C.size_t range <>) of C.unsigned_char;
   type  char_array is array (C.size_t range <>) of C.char;
   type short_array is array (C.size_t range <>) of C.short;

   type Type_Widget (Kind : widget_Kind := widget_Kind'First) is
      record
         case Kind
         is
            when ub =>   ub : uchar_array    (0 .. 3);
            when us =>   us : GLushort_array (0 .. 1);
            when ui =>   ui : c.unsigned;
            when b  =>   b  : char_array  (0 .. 3);
            when s  =>   s  : short_array (0 .. 1);
            when i  =>   i  : C.int;
            when f  =>   f  : GLfloat;
         end case;
      end record;
   pragma Unchecked_Union (Type_Widget);



   function legalFormat (format : in GLenum) return Boolean
   is
   begin
      case format
      is
         when GL_ALPHA
            | GL_RGB
            | GL_RGBA
            | GL_LUMINANCE
            | GL_LUMINANCE_ALPHA =>
            return True;

         when others =>
            return False;
      end case;
   end legalFormat;



   function legalType (gl_type : in GLenum) return Boolean
   is
   begin
      case gl_type
      is
         when GL_BYTE
            | GL_UNSIGNED_BYTE
            | GL_SHORT
            | GL_UNSIGNED_SHORT
            | GL_INT
            | GL_UNSIGNED_INT
            | GL_FLOAT
            | GL_UNSIGNED_SHORT_5_6_5
            | GL_UNSIGNED_SHORT_4_4_4_4
            | GL_UNSIGNED_SHORT_5_5_5_1 =>
            return True;

         when others =>
            return False;
      end case;
   end legalType;



   function isTypePackedPixel (gl_type : in GLenum) return Boolean
   is
      pragma assert (legalType (gl_type));
   begin
      case gl_type
        is
         when GL_UNSIGNED_SHORT_5_6_5
            | GL_UNSIGNED_SHORT_4_4_4_4
            | GL_UNSIGNED_SHORT_5_5_5_1 =>
            return True;

         when others =>
            return False;
      end case;
   end isTypePackedPixel;





   -- Determines if the packed pixel type is compatible with the format.
   --
   function isLegalFormatForPackedPixelType (format,
                                             gl_type : in GLenum) return Boolean
   is
   begin
      -- if not a packed pixel type then return true
      --
      if not isTypePackedPixel (gl_type) then
         return True;
      end if;

      -- 3_3_2/2_3_3_REV & 5_6_5/5_6_5_REV are only compatible with RGB
      --
      if    gl_type = GL_UNSIGNED_SHORT_5_6_5
        and format /= GL_RGB
      then
         return False;
      end if;

      -- 4_4_4_4 & 5_5_5_1 are only compatible with RGBA.
      --
      if    (   gl_type = GL_UNSIGNED_SHORT_4_4_4_4
             or gl_type = GL_UNSIGNED_SHORT_5_5_5_1)
        and format /= GL_RGBA
      then
         return False;
      end if;

      return True;
   end isLegalFormatForPackedPixelType;



   -- Return the number of bytes per element, based on the element type.
   --
   function bytes_per_element (gl_type : in GLenum) return GLfloat
   is
   begin
      case gl_type
      is
         when GL_UNSIGNED_SHORT         =>   return GLfloat (GLushort'Size / 8);
         when GL_SHORT                  =>   return GLfloat (GLshort 'Size / 8);
         when GL_UNSIGNED_BYTE          =>   return GLfloat (GLubyte 'Size / 8);
         when GL_BYTE                   =>   return GLfloat (GLbyte  'Size / 8);
         when GL_INT                    =>   return GLfloat (GLint   'Size / 8);
         when GL_UNSIGNED_INT           =>   return GLfloat (GLuint  'Size / 8);
         when GL_FLOAT                  =>   return GLfloat (GLfloat 'Size / 8);
         when GL_UNSIGNED_SHORT_5_6_5
            | GL_UNSIGNED_SHORT_4_4_4_4
            | GL_UNSIGNED_SHORT_5_5_5_1 =>   return GLfloat (GLushort'Size / 8);
         when others                    =>   return 4.0;
      end case;
   end bytes_per_element;



   -- Return the number of elements per group of a specified format.
   --
   function elements_per_group (format, gl_type : in GLenum) return GLint
   is
   begin
      -- If the type is packedpixels then answer is 1 (ignore format).
      --
      if   gl_type = GL_UNSIGNED_SHORT_5_6_5
        or gl_type = GL_UNSIGNED_SHORT_4_4_4_4
        or gl_type = GL_UNSIGNED_SHORT_5_5_5_1
      then
         return 1;
      end if;

      -- Types are not packed pixels, so get elements per group.
      --
      case format
      is
         when GL_RGB             =>   return 3;
         when GL_LUMINANCE_ALPHA =>   return 2;
         when GL_RGBA            =>   return 4;
         when others             =>   return 1;
      end case;
   end elements_per_group;



   -- Compute memory required for internal packed array of data of given type and format.
   --
   function image_size (width,  height  : in GLint;
                        format, gl_type : in GLenum) return c.size_t
   is
      pragma assert (width  > 0);
      pragma assert (height > 0);

      bytes_per_row : GLint := GLint (bytes_per_element (gl_type)) * width;
      components    : GLint := elements_per_group (format, gl_type);
   begin
      return c.size_t (bytes_per_row * height * components);
   end image_size;



   procedure retrieveStoreModes (psm : in out PixelStorageModes)
   is
   begin
      glGetIntegerv (GL_UNPACK_ALIGNMENT, psm.unpack_alignment'Access);

      psm.unpack_row_length  := 0;
      psm.unpack_skip_rows   := 0;
      psm.unpack_skip_pixels := 0;
      psm.unpack_lsb_first   := 0;
      psm.unpack_swap_bytes  := 0;

      glGetIntegerv (GL_PACK_ALIGNMENT, psm.pack_alignment'Access);

      psm.pack_row_length  := 0;
      psm.pack_skip_rows   := 0;
      psm.pack_skip_pixels := 0;
      psm.pack_lsb_first   := 0;
      psm.pack_swap_bytes  := 0;
   end retrieveStoreModes;



   function GLU_SWAP_2_BYTES (s : in system.Address) return GLushort
   is
      use GLubyte_Pointers;
      s0 : GLubyte_view := to_GLubyte_view (s) + 0;
      s1 : GLubyte_view := to_GLubyte_view (s) + 1;
   begin
      return GLushort (   shift_Left (Unsigned_16 (s1.all), 8)
                       or             Unsigned_16 (s0.all));
   end GLU_SWAP_2_BYTES;

--  #define __GLU_SWAP_2_BYTES(s)\
--  (GLushort) (  ((GLushort)  ((const GLubyte*) (s)) [1]) << 8  |  ((const GLubyte*) (s)) [0]  )



   function GLU_SWAP_4_BYTES (s : in system.Address) return GLushort
   is
      use GLubyte_Pointers;
      s0 : GLubyte_view := to_GLubyte_view (s) + 0;
      s1 : GLubyte_view := to_GLubyte_view (s) + 1;
      s2 : GLubyte_view := to_GLubyte_view (s) + 2;
      s3 : GLubyte_view := to_GLubyte_view (s) + 3;
   begin
      return GLushort (   shift_Left (Unsigned_32 (s3.all), 24)
                       or shift_Left (Unsigned_32 (s2.all), 16)
                       or shift_Left (Unsigned_32 (s1.all), 8)
                       or             Unsigned_32 (s0.all));
   end GLU_SWAP_4_BYTES;

--  #define __GLU_SWAP_4_BYTES(s)\
--  (GLuint)(((GLuint)((const GLubyte*)(s))[3])<<24 | \
--          ((GLuint)((const GLubyte*)(s))[2])<<16 | \
--          ((GLuint)((const GLubyte*)(s))[1])<<8  |
--                   ((const GLubyte*)(s))[0])




   procedure extract565 (isSwap            : in     GLint;
                         packedPixel       : in     system.Address;
                         extractComponents :    out GLfloat_array)
   is
      use type GLushort;
      ushort : GLushort;
   begin
      if isSwap /= 0 then
         ushort := GLU_SWAP_2_BYTES (packedPixel);
      else
         ushort := to_GLushort_view (packedPixel).all;
      end if;

      -- 11111000,00000000 == 0xf800
      -- 00000111,11100000 == 0x07e0
      -- 00000000,00011111 == 0x001f
      --
      extractComponents (0) := GLfloat (shift_Right (Unsigned_16 (ushort and 16#f800#), 11)) / 31.0;   -- 31 = 2^5-1
      extractComponents (1) := GLfloat (shift_Right (Unsigned_16 (ushort and 16#07e0#),  5)) / 63.0;   -- 63 = 2^6-1
      extractComponents (2) := GLfloat (                          ushort and 16#001f#)       / 31.0;
   end extract565;



   procedure extract4444 (isSwap            : in     GLint;
                          packedPixel       : in     system.Address;
                          extractComponents :    out GLfloat_array)
   is
      use type GLushort;
      ushort : GLushort;
   begin
      if isSwap /= 0 then
         ushort := GLU_SWAP_2_BYTES (packedPixel);
      else
         ushort := to_GLushort_view (packedPixel).all;
      end if;

      -- 11110000,00000000 == 0xf000
      -- 00001111,00000000 == 0x0f00
      -- 00000000,11110000 == 0x00f0
      -- 00000000,00001111 == 0x000f
      --
      extractComponents (0) := GLfloat (shift_Right (Unsigned_16 (ushort and 16#f000#), 12)) / 15.0;   -- 15 = 2^4-1
      extractComponents (1) := GLfloat (shift_Right (Unsigned_16 (ushort and 16#0f00#),  8)) / 15.0;
      extractComponents (2) := GLfloat (shift_Right (Unsigned_16 (ushort and 16#00f0#),  4)) / 15.0;
      extractComponents (3) := GLfloat (                          ushort and 16#000f#)       / 15.0;
   end extract4444;



   procedure extract5551 (isSwap            : in     GLint;
                          packedPixel       : in     system.Address;
                          extractComponents :    out GLfloat_array)
   is
      use type GLushort;
      ushort : GLushort;
   begin
      if isSwap /= 0 then
         ushort := GLU_SWAP_2_BYTES (packedPixel);
      else
         ushort := to_GLushort_view (packedPixel).all;
      end if;

      -- 11111000,00000000 == 0xf800
      -- 00000111,11000000 == 0x07c0
      -- 00000000,00111110 == 0x003e
      -- 00000000,00000001 == 0x0001
      --
      extractComponents (0) := GLfloat (shift_Right (Unsigned_16 (ushort and 16#f800#), 11)) / 31.0;   -- 31 = 2^5-1
      extractComponents (1) := GLfloat (shift_Right (Unsigned_16 (ushort and 16#07c0#),  6)) / 31.0;
      extractComponents (2) := GLfloat (shift_Right (Unsigned_16 (ushort and 16#003e#),  1)) / 31.0;
      extractComponents (3) := GLfloat (                          ushort and 16#0001#);
   end extract5551;



   procedure shove565 (shoveComponents : in GLfloat_array;
                       index           : in GLint;
                       packedPixel     : in system.Address)
   is
      use GLushort_Pointers;
      use type GLushort;
      the_Pixel : GLushort_view := to_GLushort_view (packedPixel) + C.ptrdiff_t (index);
   begin
      -- 11111000,00000000 == 0xf800
      -- 00000111,11100000 == 0x07e0
      -- 00000000,00011111 == 0x001f

      pragma assert (0.0 <= shoveComponents(0) and shoveComponents(0) <= 1.0);
      pragma assert (0.0 <= shoveComponents(1) and shoveComponents(1) <= 1.0);
      pragma assert (0.0 <= shoveComponents(2) and shoveComponents(2) <= 1.0);

      -- due to limited precision, need to round before shoving
      --
      the_Pixel.all :=                  GLushort (shift_Left (Unsigned_16 (shoveComponents (0) * 31.0 + 0.5), 11) and 16#f800#);
      the_Pixel.all := the_Pixel.all or GLushort (shift_Left (Unsigned_16 (shoveComponents (1) * 63.0 + 0.5),  5) and 16#07e0#);
      the_Pixel.all := the_Pixel.all or GLushort (            Unsigned_16 (shoveComponents (2) * 31.0 + 0.5)      and 16#001f#);
   end shove565;



   procedure shove4444 (shoveComponents : in GLfloat_array;
                       index           : in GLint;
                       packedPixel     : in system.Address)
   is
      use GLushort_Pointers;
      use type GLushort;
      the_Pixel : GLushort_view := to_GLushort_view (packedPixel) + C.ptrdiff_t (index);
   begin
      pragma assert (0.0 <= shoveComponents (0) and shoveComponents (0) <= 1.0);
      pragma assert (0.0 <= shoveComponents (1) and shoveComponents (1) <= 1.0);
      pragma assert (0.0 <= shoveComponents (2) and shoveComponents (2) <= 1.0);
      pragma assert (0.0 <= shoveComponents (3) and shoveComponents (3) <= 1.0);

      -- due to limited precision, need to round before shoving
      --
      the_Pixel.all :=                  GLushort (shift_Left (Unsigned_16 (shoveComponents (0) * 15.0 + 0.5), 12) and 16#f000#);
      the_Pixel.all := the_Pixel.all or GLushort (shift_Left (Unsigned_16 (shoveComponents (1) * 15.0 + 0.5),  8) and 16#0f00#);
      the_Pixel.all := the_Pixel.all or GLushort (shift_Left (Unsigned_16 (shoveComponents (2) * 15.0 + 0.5),  4) and 16#00f0#);
      the_Pixel.all := the_Pixel.all or GLushort (            Unsigned_16 (shoveComponents (3) * 15.0 + 0.5)      and 16#000f#);
   end shove4444;



   procedure shove5551 (shoveComponents : in GLfloat_array;
                        index           : in GLint;
                        packedPixel     : in system.Address)
   is
      use GLushort_Pointers;
      use type GLushort;
      the_Pixel : GLushort_view := to_GLushort_view (packedPixel) + C.ptrdiff_t (index);
   begin
      -- 11111000,00000000 == 0xf800
      -- 00000111,11000000 == 0x07c0
      -- 00000000,00111110 == 0x003e
      -- 00000000,00000001 == 0x0001

      pragma assert (0.0 <= shoveComponents (0) and shoveComponents (0) <= 1.0);
      pragma assert (0.0 <= shoveComponents (1) and shoveComponents (1) <= 1.0);
      pragma assert (0.0 <= shoveComponents (2) and shoveComponents (2) <= 1.0);
      pragma assert (0.0 <= shoveComponents (3) and shoveComponents (3) <= 1.0);

      -- due to limited precision, need to round before shoving
      --
      the_Pixel.all :=                  GLushort (shift_Left (Unsigned_16 (shoveComponents (0) * 31.0 + 0.5), 11) and 16#f800#);
      the_Pixel.all := the_Pixel.all or GLushort (shift_Left (Unsigned_16 (shoveComponents (1) * 31.0 + 0.5),  6) and 16#07c0#);
      the_Pixel.all := the_Pixel.all or GLushort (shift_Left (Unsigned_16 (shoveComponents (2) * 31.0 + 0.5),  1) and 16#003e#);
      the_Pixel.all := the_Pixel.all or GLushort (            Unsigned_16 (shoveComponents (3)        + 0.5)      and 16#0001#);
   end shove5551;



   -- Extract array from user's data applying all pixel store modes.
   -- The internal format used is an array of unsigned shorts.
   --
   procedure fill_image (psm           : in PixelStorageModes;
                         width, height : in GLint;
                         format        : in GLenum;
                         gl_type       : in GLenum;
                         index_format  : in Boolean;
                         userdata      : in System.Address;
                         newimage      : in GLushort_array_view)
   is
      use GLubyte_Pointers,
          GLushort_Pointers;

      use type GLubyte,
               GLushort;

      components,
      element_size,
      rowsize,
      padding,
      groups_per_line,
      group_size,
      elements_per_line : GLint;

      start : GLubyte_view;
      iter  : GLubyte_view;
      iter2 : GLushort_view;

      myswap_bytes : GLint;

      function to_GLubyte_view is new ada.Unchecked_Conversion (System.Address, GLubyte_view);

   begin
      myswap_bytes := psm.unpack_swap_bytes;
      components   := elements_per_group (format, gl_type);

      if psm.unpack_row_length > 0 then
         groups_per_line := psm.unpack_row_length;
      else
         groups_per_line := width;
      end if;

      element_size := GLint (bytes_per_element (gl_type));
      group_size   := element_size * components;

      if element_size = 1 then
         myswap_bytes := 0;
      end if;

      rowsize := groups_per_line * group_size;
      padding := rowsize mod psm.unpack_alignment;

      if padding /= 0 then
         rowsize := rowsize + psm.unpack_alignment - padding;
      end if;

--        start             := (const GLubyte *) userdata + psm->unpack_skip_rows * rowsize
      start             :=   to_GLubyte_view (userdata)
                           + C.ptrdiff_t (  psm.unpack_skip_rows   * rowsize
                                          + psm.unpack_skip_pixels * group_size);
      elements_per_line := width * components;

      iter2 := newimage (newimage'First)'Access;

      for i in 0 .. height - 1
      loop
         iter := start;

         for j in 0 .. elements_per_line - 1
         loop
            declare
               use type GLfloat;
               widget            : Type_Widget;
               extractComponents : GLfloat_array (0 .. 3);
            begin
               case gl_type
               is
               when GL_UNSIGNED_BYTE =>
                  if index_format then
                     iter2.all := GLushort (iter.all);
                     iter2     := iter2 + 1;
                  else
                     iter2.all := GLushort (iter.all) * 257;
                     iter2     := iter2 + 1;
                  end if;

               when GL_BYTE =>
                  if index_format then
                     iter2.all := GLushort (to_GLbyte_view (iter).all);
                     iter2     := iter2 + 1;
                  else
                     -- rough approx
                     iter2.all := GLushort (to_GLbyte_view (iter).all) * 516;
                     iter2     := iter2 + 1;
                  end if;

               when GL_UNSIGNED_SHORT_5_6_5 =>
                  extract565 (myswap_bytes, iter.all'Address, extractComponents);
                  for k in C.size_t' (0) .. 2 loop
                     iter2.all := GLushort (extractComponents (k) * 65535.0);
                     iter2     := iter2 + 1;
                  end loop;

               when GL_UNSIGNED_SHORT_4_4_4_4 =>
                  extract4444 (myswap_bytes, iter.all'Address, extractComponents);
                  for k in C.size_t' (0) .. 3 loop
                     iter2.all := GLushort (extractComponents (k) * 65535.0);
                     iter2     := iter2 + 1;
                  end loop;

               when GL_UNSIGNED_SHORT_5_5_5_1 =>
                  extract5551 (myswap_bytes, iter.all'Address, extractComponents);
                  for k in C.size_t' (0) .. 3 loop
                     iter2.all := GLushort (extractComponents (k) * 65535.0);
                     iter2     := iter2 + 1;
                  end loop;

               when GL_UNSIGNED_SHORT
                  | GL_SHORT =>
                  if myswap_bytes /= 0 then
                     widget.ub (0) := GLubyte_view (iter + 1).all;
                     widget.ub (1) := GLubyte_view (iter + 0).all;
                  else
                     widget.ub (0) := GLubyte_view (iter + 0).all;
                     widget.ub (1) := GLubyte_view (iter + 1).all;
                  end if;

                  if gl_type = GL_SHORT then
                     if index_format then
                        iter2.all := GLushort (widget.s (0));
                        iter2     := iter2 + 1;
                     else
                        -- rough approx
                        iter2.all := GLushort (widget.s(0)) * 2;
                        iter2     := iter2 + 1;
                     end if;
                  else
                     iter2.all := widget.us (0);
                     iter2     := iter2 + 1;
                  end if;

               when GL_INT
                  | GL_UNSIGNED_INT
                  | GL_FLOAT =>
                  if myswap_bytes /= 0 then
                     widget.ub(0) := GLubyte_view (iter + 3).all;
                     widget.ub(1) := GLubyte_view (iter + 2).all;
                     widget.ub(2) := GLubyte_view (iter + 1).all;
                     widget.ub(3) := GLubyte_view (iter + 0).all;
                  else
                     widget.ub(0) := GLubyte_view (iter + 0).all;
                     widget.ub(1) := GLubyte_view (iter + 1).all;
                     widget.ub(2) := GLubyte_view (iter + 2).all;
                     widget.ub(3) := GLubyte_view (iter + 3).all;
                  end if;

                  if gl_type = GL_FLOAT then
                     if index_format then
                        iter2.all := GLushort (widget.f);
                        iter2     := iter2 + 1;
                     else
                        iter2.all := GLushort (65535.0 * widget.f);
                        iter2     := iter2 + 1;
                     end if;
                  elsif gl_type = GL_UNSIGNED_INT then
                     if index_format then
                        iter2.all := GLushort (widget.ui);
                        iter2     := iter2 + 1;
                     else
                        iter2.all := GLushort (shift_Right (Unsigned_32 (widget.ui), 16));
                        iter2     := iter2 + 1;
                     end if;
                  else
                     if index_format then
                        iter2.all := GLushort (widget.i);
                        iter2     := iter2 + 1;
                     else
                        iter2.all := GLushort (shift_Right (Unsigned_32 (widget.i), 15));
                        iter2     := iter2 + 1;
                     end if;
                  end if;

               when others =>
                  raise GLU_INVALID_TYPE;
               end case;

               iter := iter + C.ptrdiff_t (element_size);
            end;
         end loop; -- for j

         start := start + C.ptrdiff_t (rowsize);

         -- want 'iter' pointing at start, not within, row for assertion purposes
         iter := start;
      end loop; -- for i

      -- iterators should be one byte past end
      --
      if not isTypePackedPixel (gl_type) then
         pragma assert (iter2 = newimage (C.size_t (width * height * components))'Access);
      else
         pragma assert (iter2 = newimage (C.size_t (width * height * elements_per_group (format, 0)))'Access);
      end if;

      pragma assert (iter = to_GLubyte_view (userdata) + C.ptrdiff_t (  rowsize * height
                                                                      + psm.unpack_skip_rows   * rowsize
                                                                      + psm.unpack_skip_pixels * group_size));
   end fill_image;



   -- Insert array into user's data applying all pixel store modes.
   -- The internal format is an array of unsigned shorts.
   -- empty_image() because it is the opposite of fill_image().
   --
   procedure empty_image (psm           : in PixelStorageModes;
                          width, height : in GLint;
                          format        : in GLenum;
                          gl_type       : in GLenum;
                          index_format  : in Boolean;
                          oldimage      : in GLushort_array_view;
                          userdata      : in System.Address)

   is
      use GLubyte_Pointers,
          GLushort_Pointers;

      use type GLubyte,
               GLushort;
      components,
      element_size,
      rowsize,
      padding,
      groups_per_line,
      group_size,
      elements_per_line : GLint;

      start             : GLubyte_view;
      iter              : GLubyte_view;
      iter2             : GLushort_view;

      myswap_bytes      : GLint;
      shoveComponents   : GLfloat_array (0 .. 3);
   begin
      myswap_bytes := psm.pack_swap_bytes;
      components   := elements_per_group (format, gl_type);

      if psm.pack_row_length > 0 then
         groups_per_line := psm.pack_row_length;
      else
         groups_per_line := width;
      end if;

      element_size := GLint (bytes_per_element (gl_type));
      group_size   := element_size * components;

      if element_size = 1 then
         myswap_bytes := 0;
      end if;

      rowsize := groups_per_line * group_size;
      padding := (rowsize mod psm.pack_alignment);

      if padding /= 0 then
         rowsize := rowsize + psm.pack_alignment - padding;
      end if;

      start             :=   to_GLubyte_view (userdata)
                           + C.ptrdiff_t (  psm.pack_skip_rows   * rowsize
                                          + psm.pack_skip_pixels * group_size);
      elements_per_line := width * components;

      iter2 := oldimage (oldimage'First)'Access;

      for i in 0 .. height - 1
      loop
         iter := start;

         for j in 0 .. elements_per_line - 1
         loop
            declare
               widget : Type_Widget;
            begin
               case gl_type
               is
               when GL_UNSIGNED_BYTE =>
                  if index_format then
                     iter.all := GLubyte (iter2.all);
                     iter2     := iter2 + 1;
                  else
                     iter.all := GLubyte (shift_Right (Unsigned_16 (iter2.all), 8));
                     iter2     := iter2 + 1;
                  end if;

               when GL_BYTE =>
                  if index_format then
                     to_GLbyte_view (iter).all := GLbyte (iter2.all);
                     iter2     := iter2 + 1;
                  else
                     to_GLbyte_view (iter).all := GLbyte (shift_Right (Unsigned_16 (iter2.all), 9));
                     iter2     := iter2 + 1;
                  end if;

               when GL_UNSIGNED_SHORT_5_6_5 =>
                  for k in C.size_t' (0) .. 2 loop
                     shoveComponents (k) := GLfloat (iter2.all) / 65535.0;
                     iter2     := iter2 + 1;
                  end loop;

                  shove565 (shoveComponents, 0, widget.us (0)'Address);

                  if myswap_bytes /= 0 then
                     GLubyte_view (iter + 0).all := widget.ub (1);
                     GLubyte_view (iter + 1).all := widget.ub (0);
                  else
                     to_GLushort_view (iter).all := widget.us (0);
                  end if;

               when GL_UNSIGNED_SHORT_4_4_4_4 =>
                  for k in C.size_t' (0) .. 3 loop
                     shoveComponents (k) := GLfloat (iter2.all) / 65535.0;
                     iter2               := iter2 + 1;
                  end loop;

                  shove4444 (shoveComponents, 0, widget.us (0)'Address);

                  if myswap_bytes /= 0 then
                     GLubyte_view (iter + 0).all := widget.ub (1);
                     GLubyte_view (iter + 1).all := widget.ub (0);
                  else
                     to_GLushort_view (iter).all := widget.us (0);
                  end if;

               when GL_UNSIGNED_SHORT_5_5_5_1 =>
                  for k in C.size_t' (0) .. 3 loop
                     shoveComponents (k) := GLfloat (iter2.all) / 65535.0;
                     iter2               := iter2 + 1;
                  end loop;

                  shove5551 (shoveComponents, 0, widget.us (0)'Address);

                  if myswap_bytes /= 0 then
                     GLubyte_view (iter + 0).all := widget.ub (1);
                     GLubyte_view (iter + 1).all := widget.ub (0);
                  else
                     to_GLushort_view (iter).all := widget.us (0);
                  end if;

               when GL_UNSIGNED_SHORT
                  | GL_SHORT =>
                  if gl_type = GL_SHORT then
                     if index_format then
                        widget.s (0) := GLshort (iter2.all);
                        iter2        := iter2 + 1;
                     else
                        widget.s (0) := GLshort (shift_Right (Unsigned_16 (iter2.all), 1));
                        iter2        := iter2 + 1;
                     end if;
                  else
                     widget.us (0) := iter2.all;
                     iter2         := iter2 + 1;
                  end if;

                  if myswap_bytes /= 0 then
                     GLubyte_view (iter + 0).all := widget.ub (1);
                     GLubyte_view (iter + 1).all := widget.ub (0);
                  else
                     GLubyte_view (iter + 0).all := widget.ub (0);
                     GLubyte_view (iter + 1).all := widget.ub (1);
                  end if;

               when GL_INT
		  | GL_UNSIGNED_INT
		  | GL_FLOAT =>
                  if gl_type = GL_FLOAT then
                     if index_format then
                        widget.f := GLfloat (iter2.all);
                        iter2    := iter2 + 1;
                     else
                        widget.f := GLfloat (iter2.all) / 65535.0;
                        iter2    := iter2 + 1;
                     end if;
                  elsif gl_type = GL_UNSIGNED_INT then
                     if index_format then
                        widget.ui := GLuint (iter2.all);
                        iter2     := iter2 + 1;
                     else
                        widget.ui := GLuint (iter2.all) * 65537;
                        iter2     := iter2 + 1;
                     end if;
                  else
                     if index_format then
                        widget.i := GLint (iter2.all);
                        iter2    := iter2 + 1;
                     else
                        widget.i := GLint ((GLuint (iter2.all) * 65537) / 2);
                        iter2    := iter2 + 1;
                     end if;
                  end if;

                  if myswap_bytes /= 0 then
                     GLubyte_view (iter + 3).all := widget.ub (0);
                     GLubyte_view (iter + 2).all := widget.ub (1);
                     GLubyte_view (iter + 1).all := widget.ub (2);
                     GLubyte_view (iter + 0).all := widget.ub (3);
                  else
                     GLubyte_view (iter + 0).all := widget.ub (0);
                     GLubyte_view (iter + 1).all := widget.ub (1);
                     GLubyte_view (iter + 2).all := widget.ub (2);
                     GLubyte_view (iter + 3).all := widget.ub (3);
                  end if;

               when others =>
                  raise GLU_INVALID_TYPE;
               end case;

               iter := iter + C.ptrdiff_t (element_size);
            end;
         end loop;  --  for j

         start := start + C.ptrdiff_t (rowsize);

         -- want 'iter' pointing at start, not within, row for assertion purposes
         iter := start;
      end loop;  -- for i


      -- iterators should be one byte past end
      --
      if not isTypePackedPixel (gl_type) then
         pragma assert (iter2 = oldimage (C.size_t (width * height * components))'Access);
      else
         pragma assert (iter2 = oldimage (C.size_t (width * height * elements_per_group (format, 0)))'Access);
      end if;

      pragma assert ( iter = to_GLubyte_view (userdata) + C.ptrdiff_t (  rowsize * height
                                                                       + psm.pack_skip_rows   * rowsize
                                                                       + psm.pack_skip_pixels * group_size) );
   end empty_image;



   procedure halveImage (components : in GLint;
                         width      : in GLuint;
                         height     : in GLuint;
                         datain     : in GLushort_view;
                         dataout    : in GLushort_view)
   is
      use GLushort_Pointers;
      use type GLushort;

      newwidth,
      newheight : GLint;
      the_delta : GLint;
      s, t      : GLushort_view;
   begin
      newwidth  := GLint (width)  / 2;
      newheight := GLint (height) / 2;
      the_delta := GLint (width)  * components;

      s := dataout;
      t := datain;

      -- Piece o' cake !
      --
      for i in 0 .. newheight - 1
      loop
         for j in 0 .. newwidth - 1
         loop
            for k in 0 .. components - 1
            loop
               s.all :=   (  GLushort_view (t + 0                                   ).all
                           + GLushort_view (t + C.ptrdiff_t (components)            ).all
                           + GLushort_view (t + C.ptrdiff_t (the_delta)             ).all
                           + GLushort_view (t + C.ptrdiff_t (the_delta + components)).all
                           + 2)
                        / 4;
               s     := s + 1;
               t     := t + 1;
            end loop;

            t := t + C.ptrdiff_t (components);
         end loop;

         t := t + C.ptrdiff_t (the_delta);
      end loop;
   end halveImage;



   procedure scale_internal (components : in GLint;
                             widthin    : in GLint;
                             heightin   : in GLint;
                             datain     : in GLushort_view;
                             widthout   : in GLint;
                             heightout  : in GLint;
                             dataout    : in GLushort_view)
   is
      use GLushort_Pointers;

      x, lowx, highx, convx, halfconvx : GLfloat;
      y, lowy, highy, convy, halfconvy : GLfloat;
      xpercent, ypercent               : GLfloat;
      percent                          : GLfloat;

      -- Max components in a format is 4, so...
      totals                     : GLfloat_array (0 .. 3);
      area                       : GLfloat;
      yint, xint, xindex, yindex : GLint;
      temp                       : GLint;
   begin
      if    widthin  = widthout  * 2
        and heightin = heightout * 2
      then
         halveImage (components,
                     GLuint (widthin),
                     GLuint (heightin),
                     datain,
                     dataout);
         return;
      end if;

      convy     := GLfloat (heightin) / GLfloat (heightout);
      convx     := GLfloat (widthin)  / GLfloat (widthout);
      halfconvx := convx / 2.0;
      halfconvy := convy / 2.0;

      for i in 0 .. heightout - 1
      loop
         y := convy * (GLfloat (i) + 0.5);
         if heightin > heightout then
            highy := y + halfconvy;
            lowy  := y - halfconvy;
         else
            highy := y + 0.5;
            lowy  := y - 0.5;
         end if;

         for j in 0 .. widthout - 1
         loop
            x := convx * (GLfloat (j) + 0.5);

            if widthin > widthout then
               highx := x + halfconvx;
               lowx  := x - halfconvx;
            else
               highx := x + 0.5;
               lowx  := x - 0.5;
            end if;

	    -- Ok, now apply box filter to box that goes from (lowx, lowy)
	    -- to (highx, highy) on input data into this pixel on output data.
	    --
	    totals := (others => 0.0);
	    area   := 0.0;

	    y    := lowy;
	    yint := GLint (GLfloat'Floor (y));

            while y < highy
            loop
               yindex := (yint + heightin) mod heightin;

               if highy < GLfloat (yint + 1) then
                  ypercent := highy - y;
               else
                  ypercent := GLfloat (yint + 1) - y;
               end if;

               x    := lowx;
               xint := GLint (GLfloat'Floor (x));

               while x < highx
               loop
                  xindex := (xint + widthin) mod widthin;

                  if highx < GLfloat (xint + 1) then
                     xpercent := highx - x;
                  else
                     xpercent := GLfloat (xint + 1) - x;
                  end if;

                  percent := xpercent * ypercent;
                  area    := area + percent;
                  temp    := (xindex + (yindex * widthin)) * components;

                  for k in 0 .. C.size_t (components - 1) loop
                     totals (k) := totals (k) +   GLfloat (GLushort_view (datain + C.ptrdiff_t (temp) + C.ptrdiff_t (k)).all)
                                                * percent;
                  end loop;

                  xint := xint + 1;
                  x    := GLfloat (xint);
               end loop;
               yint := yint + 1;
               y    := GLfloat (yint);
            end loop;

            temp := (j + (i * widthout)) * components;

            for k in 0 .. C.size_t (components - 1) loop
               -- totals[] should be rounded in the case of enlarging an RGB
               -- ramp when the type is 332 or 4444
               --
               GLushort_view (dataout
                              + C.ptrdiff_t (temp)
                              + C.ptrdiff_t (k   )).all := GLushort ((totals (k) + 0.5) / area);
            end loop;

         end loop;  -- for j
      end loop;  -- for i
   end scale_internal;



   function is_index (format : in GLenum) return Boolean   -- todo: Remove this, it doesn'y apply to 'lean' GL types.
   is
   begin
      return False; -- format == GL_COLOR_INDEX || format = GL_STENCIL_INDEX;
   end;



   procedure gluScaleImage (format    : in GLenum;
                            widthin   : in GLsizei;
                            heightin  : in GLsizei;
                            typein    : in GLenum;
                            datain    : in System.Address;
                            widthout  : in GLsizei;
                            heightout : in GLsizei;
                            typeout   : in GLenum;
                            dataout   : in System.Address)
   is
      use type GLint;

      procedure free is new ada.Unchecked_Deallocation (GLushort_array, GLushort_array_view);

      components  : GLint;
      beforeImage : GLushort_array_view;
      afterImage  : GLushort_array_view;
      psm         : PixelStorageModes;
   begin
      if   widthin   = 0
        or heightin  = 0
        or widthout  = 0
        or heightout = 0
      then
         return;
      end if;

      if   widthin   < 0
        or heightin  < 0
        or widthout  < 0
        or heightout < 0
      then
         raise GLU_INVALID_VALUE;
      end if;

      if   not legalFormat (format)
        or not legalType   (typein)
        or not legalType   (typeout)
      then
         raise GLU_INVALID_ENUM;
      end if;

      if not isLegalFormatForPackedPixelType (format, typein)
      then
         raise GLU_INVALID_OPERATION;
      end if;

      if not isLegalFormatForPackedPixelType (format, typeout)
      then
         raise GLU_INVALID_OPERATION;
      end if;


      beforeImage := new GLushort_array' (1 .. image_size (widthin,  heightin,  format, GL_UNSIGNED_SHORT) => <>);
      afterImage  := new GLushort_array' (1 .. image_size (widthout, heightout, format, GL_UNSIGNED_SHORT) => <>);

      if   beforeImage = null
        or afterImage  = null
      then
         free (beforeImage);
         free (afterImage);
         raise GLU_OUT_OF_MEMORY;
      end if;


      retrieveStoreModes (psm);

      fill_image (psm,
                  widthin, heightin,
                  format,
                  typein,
                  is_index (format),
                  datain,
                  beforeImage);

      components := elements_per_group (format, 0);

      scale_internal (components,
                      widthin, heightin,
                      beforeImage (beforeImage'First)'Access,
                      widthout, heightout,
                      afterImage (afterImage'First)'Access);

      empty_image (psm,
                   widthout, heightout,
                   format,
                   typeout,
                   is_index (format),
                   afterImage,
                   dataout);

      free (beforeImage);
      free (afterImage);
   end gluScaleImage;


end GLU;
