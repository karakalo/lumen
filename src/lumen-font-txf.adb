
-- Lumen.Font.Txf -- Display textual information using texture-mapped fonts
--
-- Chip Richards, NiEstu, Phoenix AZ, Summer 2010

-- This code is covered by the ISC License:
--
-- Copyright © 2010, NiEstu
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- The software is provided "as is" and the author disclaims all warranties
-- with regard to this software including all implied warranties of
-- merchantability and fitness. In no event shall the author be liable for any
-- special, direct, indirect, or consequential damages or any damages
-- whatsoever resulting from loss of use, data or profits, whether in an
-- action of contract, negligence or other tortious action, arising out of or
-- in connection with the use or performance of this software.


-- Environment
with Ada.Containers.Generic_Array_Sort;
with Ada.Streams;
with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

with Lumen.Binary.IO;
with Lumen.Binary.Endian.Shorts;
with Lumen.Binary.Endian.Words;
with Lumen.GLU;


package body Lumen.Font.Txf is

   ---------------------------------------------------------------------------

   -- Texture-mapped glyph vertex info
   type Texture_Coords is array (1 .. 4) of GL.Floats_2;
   type Vertices is array (1 .. 4) of GL.Shorts_2;
   type Txf_Vertex_Info is record
      Glyph   : Natural;
      T       : Texture_Coords;
      V       : Vertices;
      Advance : Float;
   end record;
   type Txf_Vertex_Table is array (Positive range <>) of Txf_Vertex_Info;

   -- Internal view of a font
   type Lookup_Table is array (Positive range <>) of Positive;
   type Font_Info (Glyphs : Natural;   Size : Natural) is record
      Object     : GL.UInt := 0;
      Width      : Natural;
      Height     : Natural;
      Ascent     : Integer;
      Descent    : Integer;
      Num_Glyphs : Natural;
      Base       : Natural;
      Verts      : Txf_Vertex_Table (1 .. Glyphs);
      Image      : Binary.Byte_String (1 .. Size);
   end record;

   ---------------------------------------------------------------------------

   -- Generic procedure to read an instance of a given type, confining all the
   -- ugliness of address thwacking to one place
   generic
      type Item_Type is private;
   procedure Read (File : in     Lumen.Binary.IO.File_Type;
                   Item :    out Item_Type);

   procedure Read (File : in     Lumen.Binary.IO.File_Type;
                   Item :    out Item_Type) is

      use Lumen.Binary;

      Buffer_Len : Natural := Item'Size / Byte_Bits;

      subtype Buffer_Type is Byte_String (1 .. Buffer_Len);
      package Address_Convert is new System.Address_To_Access_Conversions (Buffer_Type);

      Buffer : Address_Convert.Object_Pointer := Address_Convert.To_Pointer (Item'Address);

   begin  -- Read
      Buffer.all := IO.Read (File, Buffer_Len);
   end Read;

   ---------------------------------------------------------------------------
   --
   -- Package subroutines
   --
   ---------------------------------------------------------------------------

   -- Fetch vertex info record for given glyph
   function Get_Vertex_Info (Font  : in Handle;
                             Glyph : in Natural) return Txf_Vertex_Info is

      Index : Positive := Glyph - Font.Info.Base;

   begin  -- Get_Vertex_Info

      -- Kilgard's code did automatic ASCII case folding here.  Should we?
      if Index in Font.Info.Verts'Range then
         return Font.Info.Verts (Index);
      else
         raise No_Glyph with "glyph" & Natural'Image (Glyph) & " is not in this font";
      end if;
   end Get_Vertex_Info;

   -- Glyph vertex info record comparison function
   function "<" (Left, Right : Txf_Vertex_Info) return Boolean is
   begin  -- "<"
      return Left.Glyph < Right.Glyph;
   end "<";

   procedure Sort is new Ada.Containers.Generic_Array_Sort (Index_Type   => Positive,
                                                            Element_Type => Txf_Vertex_Info,
                                                            Array_Type   => Txf_Vertex_Table,
                                                            "<"          => "<");

   ---------------------------------------------------------------------------
   --
   -- Public subroutines
   --
   ---------------------------------------------------------------------------

   procedure Finalize (Font : in out Handle) is
   begin  -- Finalize
      if Font.Info /= null then
         Unload (Font);
      end if;
   end Finalize;

   ---------------------------------------------------------------------------

   -- Load a texture font
   procedure Load (Font     : in out Handle;
                   Pathname : in     String) is

      use Lumen.Binary;

      -- Byte swapping for word-length values
      package Wrds is new Lumen.Binary.Endian.Words (Word);
      package Ints is new Lumen.Binary.Endian.Words (Integer);

      -- Traditional txf file header
      subtype Txf_Signature is String (1 .. 4);
      Txf_Signature_Mark : Txf_Signature := Character'Val (16#FF#) & "txf";
      Txf_Endian_Mark    : Word := 16#12345678#;
      Txf_Byte_Format    : Word := 0;  -- should be an enum, just way easier like this
      Txf_Bitmap_Format  : Word := 1;

      type Txf_Header is record
         Signature  : Txf_Signature;
         Direction  : Word;
         Format     : Word;
         Width      : Natural;
         Height     : Natural;
         Ascent     : Integer;
         Descent    : Integer;
         Num_Glyphs : Natural;
      end record;
      for Txf_Header use record
         Signature  at 0 * Word_Bytes range 0 .. Word_LB;
         Direction  at 1 * Word_Bytes range 0 .. Word_LB;
         Format     at 2 * Word_Bytes range 0 .. Word_LB;
         Width      at 3 * Word_Bytes range 0 .. Word_LB;
         Height     at 4 * Word_Bytes range 0 .. Word_LB;
         Ascent     at 5 * Word_Bytes range 0 .. Word_LB;
         Descent    at 6 * Word_Bytes range 0 .. Word_LB;
         Num_Glyphs at 7 * Word_Bytes range 0 .. Word_LB;
      end record;

      procedure Read_Header is new Read (Txf_Header);

      -- Texture glyph info, used to build the vertex table
      type Txf_Glyph_Info is record
         Glyph    : Short;
         Width    : Byte;
         Height   : Byte;
         X_Offset : S_Byte;
         Y_Offset : S_Byte;
         Advance  : S_Byte;
         X        : S_Short;
         Y        : S_Short;
      end record;
      for Txf_Glyph_Info use record
         Glyph    at  0 range 0 .. Short_LB;
         Width    at  2 range 0 .. Byte_LB;
         Height   at  3 range 0 .. Byte_LB;
         X_Offset at  4 range 0 .. Byte_LB;
         Y_Offset at  5 range 0 .. Byte_LB;
         Advance  at  6 range 0 .. Byte_LB;
         X        at  8 range 0 .. Short_LB;
         Y        at 10 range 0 .. Short_LB;
      end record;
      type Txf_Glyph_Info_Array is array (Positive range <>) of Txf_Glyph_Info;

      -- Operating variables
      File   : IO.File_Type;
      Header : Txf_Header;
      Swap   : Boolean := False;  -- assume the best

   begin  -- Load

      -- Slurp in the file's header
      IO.Open (File, Pathname);
      Read_Header (File, Header);

      -- Verify that it's got the txf signature
      if Header.Signature /= Txf_Signature_Mark then
         IO.Close (File);
         raise Unknown_Format with "missing txf signature";
      end if;

      -- Check the byte ordering
      if Header.Direction /= Txf_Endian_Mark then
         -- It must be one or the other
         if Wrds.Swap_Bytes (Header.Direction) = Txf_Endian_Mark then
            Swap := True;

            -- Go ahead and swap the header while we're here
            Wrds.Swap_Bytes (Header.Format);
            Ints.Swap_Bytes (Header.Width);
            Ints.Swap_Bytes (Header.Height);
            Ints.Swap_Bytes (Header.Ascent);
            Ints.Swap_Bytes (Header.Descent);
            Ints.Swap_Bytes (Header.Num_Glyphs);
         else
            -- Nothing we can recognize
            IO.Close (File);
            raise Unknown_Format with "unrecognized byte-order marker";
         end if;
      end if;

      -- Reject bitmapped texture-font files for now
      if Header.Format /= Txf_Byte_Format then
         IO.Close (File);
         raise Invalid_Format with "bitmap format not yet supported";
      end if;

      -- Now we know the size of the texture glyph info table, so read it in
      -- and use it to create the vertex table, which is what we actually use
      -- to draw the glyphs
      declare
         subtype Txf_Info_Table is Txf_Glyph_Info_Array (1 .. Header.Num_Glyphs);
         procedure Read_Txf_Info is new Read (Txf_Info_Table);
         package USht is new Lumen.Binary.Endian.Shorts (Short);
         package SSht is new Lumen.Binary.Endian.Shorts (S_Short);

         Info   : Txf_Info_Table;
         W      : Float := Float (Header.Width);
         H      : Float := Float (Header.Height);
         X_Step : Float := 0.5 / W;
         Y_Step : Float := 0.5 / H;
         T      : Txf_Glyph_Info;
         Verts  : Txf_Vertex_Info;
      begin

         -- Read the data from the file
         Read_Txf_Info (File, Info);

         -- Ensure correct byte ordering of the multi-byte fields
         if Swap then
            for I in Info'Range loop
               USht.Swap_Bytes (Info (I).Glyph);
               SSht.Swap_Bytes (Info (I).X);
               SSht.Swap_Bytes (Info (I).Y);
            end loop;
         end if;

         -- Create our return value object
         Font.Info := new Font_Info (Glyphs => Header.Num_Glyphs,
                                     Size   => Header.Width * Header.Height);
         Font.Info.Width      := Header.Width;
         Font.Info.Height     := Header.Height;
         Font.Info.Ascent     := Header.Ascent;
         Font.Info.Descent    := Header.Descent;
         Font.Info.Num_Glyphs := Header.Num_Glyphs;

         -- Finish reading the file so we can close it
         Font.Info.Image := IO.Read (File, Font.Info.Size);
         IO.Close (File);

         -- Build the texture vertex table
         for I in Info'Range loop
            T := Info (I);
            Verts.Glyph := Natural (T.Glyph);
            Verts.T (1) (1) := Float (T.X) / W + X_Step;
            Verts.T (1) (2) := Float (T.Y) / H + Y_Step;
            Verts.V (1) (1) := GL.Short (T.X_Offset);
            Verts.V (1) (2) := GL.Short (T.Y_Offset);
            Verts.T (2) (1) := Float (T.X + S_Short (T.Width)) / W + X_Step;
            Verts.T (2) (2) := Float (T.Y) / H + Y_Step;
            Verts.V (2) (1) := GL.Short (T.X_Offset + S_Byte (T.Width));
            Verts.V (2) (2) := GL.Short (T.Y_Offset);
            Verts.T (3) (1) := Float (T.X + S_Short (T.Width)) / W + X_Step;
            Verts.T (3) (2) := Float (T.Y + S_Short (T.Height)) / H + Y_Step;
            Verts.V (3) (1) := GL.Short (T.X_Offset + S_Byte (T.Width));
            Verts.V (3) (2) := GL.Short (T.Y_Offset + S_Byte (T.Height));
            Verts.T (4) (1) := Float (T.X) / W + X_Step;
            Verts.T (4) (2) := Float (T.Y + S_Short (T.Height)) / H + Y_Step;
            Verts.V (4) (1) := GL.Short (T.X_Offset);
            Verts.V (4) (2) := GL.Short (T.Y_Offset + S_Byte (T.Height));
            Verts.Advance := Float (T.Advance);
            Font.Info.Verts (I) := Verts;
        end loop;

        -- Sort the vertex table and fetch (and adjust) the base-glyph value.
        -- NOTE: Doesn't allow for a glpyh value of zero, but that's poorly
        -- supported elsewhere too.
        Sort (Font.Info.Verts);
        Font.Info.Base := Font.Info.Verts (Font.Info.Verts'First).Glyph - 1;
      end;
   end Load;

   ---------------------------------------------------------------------------

   -- Unload a texture font
   procedure Unload (Font : in out Handle) is

      procedure Free is new Ada.Unchecked_Deallocation (Font_Info, Font_Info_Pointer);

   begin  -- Unload
      if Font.Info /= null then
         Free (Font.Info);
         Font.Info := null;  -- shouldn't be necessary, but oh well
      end if;
   end Unload;

   ---------------------------------------------------------------------------

   -- Set up an OpenGL texture object.  If Object is zero, create a new object
   -- to use for the font's texture.
   function Establish_Texture (Font          : in Handle;
                               Object        : in GL.UInt;
                               Setup_Mipmaps : in Boolean) return GL.UInt is

      use type GL.UInt;

      Status : GL.Int;

   begin  -- Establish_Texture

      -- Either use given texture object, or ask GL to generate a new one
      if Font.Info.Object = 0 then
         if Object = 0 then
            GL.GenTextures (1, Font.Info.Object'Address);
         else
            Font.Info.Object := Object;
         end if;
      end if;

      -- Bind the texture
      GL.BindTexture (GL.GL_TEXTURE_2D, Font.Info.Object);

      -- Now send the texture image to GL, either as a series of mipmaps, or just directly
      if Setup_Mipmaps then
         Status := GLU.Build2DMipmaps (GL.GL_TEXTURE_2D, GL.Int (GL.GL_INTENSITY4), Font.Info.Width, Font.Info.Height,
                                       GL.GL_LUMINANCE, GL.GL_UNSIGNED_BYTE, Font.Info.Image'Address);
      else
         GL.TexImage (GL.GL_TEXTURE_2D, 0, GL.GL_INTENSITY4, Font.Info.Width, Font.Info.Height, 0,
                      GL.GL_LUMINANCE, GL.GL_UNSIGNED_BYTE, Font.Info.Image'Address);
      end if;

      -- Return the texture object's name
      return Font.Info.Object;
   end Establish_Texture;

   ---------------------------------------------------------------------------

   -- Bind the font's texture to an OpenGL texture object
   procedure Bind_Font_Texture (Font : in Handle) is
   begin  -- Bind_Font_Texture
      GL.BindTexture (GL.GL_TEXTURE_2D, Font.Info.Object);
   end Bind_Font_Texture;

   ---------------------------------------------------------------------------

   -- Return dimensions of given string, to permit calculations for placing it
   -- in a scene
   procedure Get_String_Metrics (Font        : in     Handle;
                                 Str         : in     String;
                                 Width       :    out Natural;
                                 Max_Ascent  :    out Integer;
                                 Max_Descent :    out Integer) is

      W : Natural := 0;
      V : Txf_Vertex_Info;

   begin  -- Get_String_Metrics

      -- Scan the string a char at a time, accumulating glyph widths (well
      -- really, "advances")
      for C in Str'Range loop
         V := Get_Vertex_Info (Font, Character'Pos (Str (C)));
         W := W + Natural (V.Advance);
      end loop;

      -- Return results
      Width       := W;
      Max_Ascent  := Font.Info.Ascent;
      Max_Descent := Font.Info.Descent;
   end Get_String_Metrics;

   ---------------------------------------------------------------------------

   -- Render a single character
   procedure Render (Font : in Handle;
                     Char : in Character) is

      V : Txf_Vertex_Info := Get_Vertex_Info (Font, Character'Pos (Char));

   begin  -- Render
      GL.glBegin (GL.GL_QUADS);
      GL.TexCoord (V.T (1));
      GL.Vertex (V.V (1));
      GL.TexCoord (V.T (2));
      GL.Vertex (V.V (2));
      GL.TexCoord (V.T (3));
      GL.Vertex (V.V (3));
      GL.TexCoord (V.T (4));
      GL.Vertex (V.V (4));
      GL.glEnd;

      GL.Translate (V.Advance, 0.0, 0.0);
   end Render;

   ---------------------------------------------------------------------------

   -- Render a string of characters
   procedure Render (Font : in Handle;
                     Str  : in String) is
   begin  -- Render
      for C in Str'Range loop
         Render (Font, Str (C));
      end loop;
   end Render;

   ---------------------------------------------------------------------------

end Lumen.Font.Txf;
