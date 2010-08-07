
-- Lumen.GLU -- Lumen's own thin OpenGL utilities bindings
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
with System;

with Lumen.GL;
use  Lumen.GL;
with Lumen.Image;


package Lumen.GLU is

   -- Build mipmaps
   function Build1DMipmaps (Target     : in Enum;
                            Components : in Int;
                            Width      : in Int;
                            Format     : in Enum;
                            Data_Type  : in Enum;
                            Data       : in System.Address)
   return Int;

   function Build2DMipmaps (Target     : in Enum;
                            Components : in Int;
                            Width      : in Int;
                            Height     : in Int;
                            Format     : in Enum;
                            Data_Type  : in Enum;
                            Data       : in System.Address)
   return Int;

   -- Projections
   procedure Ortho2D (Left   : in Double;
                      Right  : in Double;
                      Bottom : in Double;
                      Top    : in Double);

private
   -- These can be bound directly
   pragma Import (C, Build1DMipmaps, "gluBuild1DMipmaps");
   pragma Import (C, Build2DMipmaps, "gluBuild2DMipmaps");
   pragma Import (C, Ortho2D, "gluOrtho2D");

end Lumen.GLU;
