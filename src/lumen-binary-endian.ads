
-- Lumen.Binary.Endian -- Parent package for big- vs. little-endian
--                        byte-ordering services
--
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


package Lumen.Binary.Endian is
   pragma Elaborate_Body;

   ---------------------------------------------------------------------------

   type Byte_Order is (High_Order_First, Low_Order_First);

   ---------------------------------------------------------------------------

   -- This is the order defined as "network byte order" by the widely-used BSD
   -- networking routines, and by common practice on the Internet.
   Network_Byte_Order : constant Byte_Order := High_Order_First;

   ---------------------------------------------------------------------------

   -- Returns the current system's byte ordering configuration.
   function System_Byte_Order return Byte_Order;
   pragma Inline (System_Byte_Order);

   ---------------------------------------------------------------------------

private

   -- This type is needed by the byte-order test in the Endian package body
   -- init code; otherwise, it could just go in the package body for
   -- Endian.Two_Byte
   type Two_Bytes is record
      B0 : Byte;
      B1 : Byte;
   end record;
   for Two_Bytes'Size use Short_Bits;

   for Two_Bytes use record
      B0 at 0 range 0 .. 7;
      B1 at 1 range 0 .. 7;
   end record;

end Lumen.Binary.Endian;
