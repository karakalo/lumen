
-- Lumen.Binary.Endian.Shorts -- Byte re-ordering routines for "short"
--                               (16-bit) values
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


package body Lumen.Binary.Endian.Shorts is

   ---------------------------------------------------------------------------

   -- Swap the bytes, no matter the host ordering
   function Swap_Bytes (Value : Short_Type) return Short_Type is

      S : Two_Bytes;
      T : Two_Bytes;

      function VTT is new Ada.Unchecked_Conversion (Short_Type, Two_Bytes);
      function TTV is new Ada.Unchecked_Conversion (Two_Bytes, Short_Type);

   begin  -- Swap_Bytes
      T := VTT (Value);
      S.B0 := T.B1;
      S.B1 := T.B0;
      return TTV (S);
   end Swap_Bytes;

   ---------------------------------------------------------------------------

   -- Swap bytes if host is little-endian, or no-op if it's big-endian
   function To_Big (Value : Short_Type) return Short_Type is
   begin  -- To_Big
      if System_Byte_Order /= High_Order_First then
         return Swap_Bytes (Value);
      else
         return Value;
      end if;
   end To_Big;

   ---------------------------------------------------------------------------

   -- Swap bytes if host is big-endian, or no-op if it's little-endian
   function To_Little (Value : Short_Type) return Short_Type is
   begin  -- To_Little
      if System_Byte_Order /= Low_Order_First then
         return Swap_Bytes (Value);
      else
         return Value;
      end if;
   end To_Little;

   ---------------------------------------------------------------------------

   -- Swap the bytes, no matter the host ordering
   procedure Swap_Bytes (Value : in out Short_Type) is

      S : Two_Bytes;
      T : Two_Bytes;

      function VTT is new Ada.Unchecked_Conversion (Short_Type, Two_Bytes);
      function TTV is new Ada.Unchecked_Conversion (Two_Bytes, Short_Type);

   begin  -- Swap_Bytes
      T := VTT (Value);
      S.B0 := T.B1;
      S.B1 := T.B0;
      Value := TTV (S);
   end Swap_Bytes;

   ---------------------------------------------------------------------------

   -- Swap bytes if host is little-endian, or no-op if it's big-endian
   procedure To_Big (Value : in out Short_Type) is
   begin  -- To_Big
      if System_Byte_Order /= High_Order_First then
         Swap_Bytes (Value);
      end if;
   end To_Big;

   ---------------------------------------------------------------------------

   -- Swap bytes if host is big-endian, or no-op if it's little-endian
   procedure To_Little (Value : in out Short_Type) is
   begin  -- To_Little
      if System_Byte_Order /= Low_Order_First then
         Swap_Bytes (Value);
      end if;
   end To_Little;

   ---------------------------------------------------------------------------

end Lumen.Binary.Endian.Shorts;
