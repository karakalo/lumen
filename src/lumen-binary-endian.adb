
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


-- Environment
with Ada.Unchecked_Conversion;


package body Lumen.Binary.Endian is

   ---------------------------------------------------------------------------

   -- Set to the host system's byte order during package initialization, in
   -- case an app ever needs to know.  Normally, the child package functions
   -- will be used instead of querying this directly.
   Host_Byte_Order : Byte_Order;

   ---------------------------------------------------------------------------
   --
   -- Exported subroutines
   --
   ---------------------------------------------------------------------------

   function System_Byte_Order return Byte_Order is
   begin  -- System_Byte_Order
      return Host_Byte_Order;
   end System_Byte_Order;

   ---------------------------------------------------------------------------
   --
   -- Package subroutines
   --
   ---------------------------------------------------------------------------

   -- Discover the system's byte ordering
   procedure Set_System_Byte_Order (Order : out Byte_Order) is

      S : Short;
      T : Two_Bytes;

      function STT is new Ada.Unchecked_Conversion (Short, Two_Bytes);

   begin  -- Set_System_Byte_Order

      -- Set up a bit pattern that we can recognize later
      S := 16#0102#;

      -- Now stick it into a record of two consecutive bytes, with no swapping
      T := STT (S);

      -- If the first byte is the one that was originally the leftmost byte in
      -- the word, then this is a big-endian platform, otherwise little-endian
      if T.B0 = 16#01# then
         Order := High_Order_First;
      else
         Order := Low_Order_First;
      end if;

   end Set_System_Byte_Order;

   ---------------------------------------------------------------------------

begin  -- package Lumen.Binary.Endian initialization code
   Set_System_Byte_Order (Host_Byte_Order);
end Lumen.Binary.Endian;
