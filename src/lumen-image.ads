
-- Lumen.Image -- Load and save image data
--
-- Chip Richards, NiEstu, Phoenix AZ, Spring 2010

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
with Lumen.Binary;


package Lumen.Image is

   ---------------------------------------------------------------------------

   -- Exceptions added by this package
   Unknown_Format : exception;  -- can't recognize the file's data
   Invalid_Format : exception;  -- thought we did, but encountered bad values

   ---------------------------------------------------------------------------

   -- Lumen.Image uses 8-bit RGBA, thank you.  (Red, green, blue, alpha (transparency)).

   -- One RGBA pixel, an array of them, and some pointers to pixels
   type Pixel is record
      R : Binary.Byte;
      G : Binary.Byte;
      B : Binary.Byte;
      A : Binary.Byte;
   end record;

   type Pixels is array (Natural range <>) of Pixel;

   type Pixel_Ptr is access Pixel;

   type Pixels_Ptr is access Pixels;

   -- A matrix of RGBA pixels, and a pointer to it
   type Pixel_Matrix is array (Natural range <>, Natural range <>) of Pixel;

   type Pixel_Matrix_Ptr is access Pixel_Matrix;

   -- Two types of image data info records
   pragma Warnings (Off);  -- yes, we know it might be big if declared wrong
   type Data (Width, Height : Natural := 0) is record
      Complete : Boolean := False;
      Values   : Pixel_Matrix (1 .. Width, 1 .. Height);
   end record;
   pragma Warnings (On);

   type Descriptor is record
      Complete : Boolean := False;
      Width    : Natural := 0;
      Height   : Natural := 0;
      Values   : Pixel_Matrix_Ptr := null;
   end record;

   ---------------------------------------------------------------------------

   -- Three fairly useful pixel values
   Black_Pixel       : constant Pixel := (R => Binary.Byte'First, G => Binary.Byte'First, B => Binary.Byte'First,
                                          A => Binary.Byte'Last);
   White_Pixel       : constant Pixel := (R => Binary.Byte'Last, G => Binary.Byte'Last, B => Binary.Byte'Last,
                                          A => Binary.Byte'Last);
   Transparent_Pixel : constant Pixel := (R => Binary.Byte'First, G => Binary.Byte'First, B => Binary.Byte'First,
                                          A => Binary.Byte'First);

   ---------------------------------------------------------------------------

   -- Read image data from a file
   function From_File (Pathname : String) return Descriptor;

   ---------------------------------------------------------------------------

end Lumen.Image;

