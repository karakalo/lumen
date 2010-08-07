
-- Lumen.Font.Txf -- Display textual information using texture-mapped fonts
--
-- Chip Richards, NiEstu, Phoenix AZ, Summer 2010

-- Credit for the design of the texfont mechanism belongs to Mark J. Kilgard,
-- who invented it while he was at SGI.  His paper on the topic, and his
-- original source code, may be found here:
--
--    http://www.opengl.org/resources/code/samples/mjktips/TexFont/TexFont.html
--
-- Mark's contributions to the field of computer graphics, and to the
-- advancement of OpenGL in particular, cannot be overstated.

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
with Ada.Finalization;

with Lumen.GL;


package Lumen.Font.Txf is

   ---------------------------------------------------------------------------

   -- Exceptions added by this package
   Unknown_Format : exception;  -- can't recognize the file's data
   Invalid_Format : exception;  -- thought we did, but encountered bad values
   No_Glyph       : exception;  -- character has no glyph in the font

   ---------------------------------------------------------------------------

   -- Our font handle, used to refer to fonts within the library and app
   type Font_Info_Pointer is private;
   type Handle is new Ada.Finalization.Limited_Controlled with record
      Info : Font_Info_Pointer;
   end record;
   procedure Finalize (Font : in out Handle);

   ---------------------------------------------------------------------------

   -- Load a texture font
   procedure Load (Font     : in out Handle;
                   Pathname : in     String);

   -- Unload a texture font
   procedure Unload (Font : in out Handle);

   -- Set up an OpenGL texture object.  If Object is zero, create a new object
   -- to use for the font's texture.
   function Establish_Texture (Font          : in Handle;
                               Object        : in GL.UInt;
                               Setup_Mipmaps : in Boolean) return GL.UInt;

   -- Bind the font's texture to an OpenGL texture object
   procedure Bind_Font_Texture (Font : in Handle);

   -- Return dimensions of given string, to permit calculations for placing it
   -- in a scene
   procedure Get_String_Metrics (Font        : in     Handle;
                                 Str         : in     String;
                                 Width       :    out Natural;
                                 Max_Ascent  :    out Natural;
                                 Max_Descent :    out Natural);

   -- Render a single character
   procedure Render (Font : in Handle;
                     Char : in Character);

   -- Render a string of characters
   procedure Render (Font : in Handle;
                     Str  : in String);

   -- Will eventually have (Wide_)Wide_String and UTF-8 too

   ---------------------------------------------------------------------------

private

   type Font_Info;
   type Font_Info_Pointer is access Font_Info;

   ---------------------------------------------------------------------------

end Lumen.Font.Txf;
