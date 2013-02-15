
-- Teapot -- Produces a Newell teapot, also known as a Utah teapot, an iconic
--           object in computer graphics history
--
-- Chip Richards, NiEstu, Phoenix AZ, Spring 2012

-- This code is covered by the ISC License:
--
-- Copyright © 2012, NiEstu
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

package Teapot is

   -- Type of teapot to draw: wireframe or solid
   type Drawing_Type is ( Wire, Solid );

   -- The procedure that draws a teapot
   procedure Draw (Size : in Float;
                   Form : in Drawing_Type := Solid);

end Teapot;

