
-- Lumen.Binary.Endian.Words -- Byte re-ordering routines for "word"
--                              (32-bit) values
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


package body Lumen.Binary.Endian.Words is

   ---------------------------------------------------------------------------

   type Four_Bytes is record
      B0 : Byte;
      B1 : Byte;
      B2 : Byte;
      B3 : Byte;
   end record;
   for Four_Bytes'Size use Word_Bits;

   for Four_Bytes use record
      B0 at 0 range 0 .. 7;
      B1 at 1 range 0 .. 7;
      B2 at 2 range 0 .. 7;
      B3 at 3 range 0 .. 7;
   end record;

   ---------------------------------------------------------------------------

   -- Swap the bytes, no matter the host ordering
   function Swap_Bytes (Value : Word_Type) return Word_Type is

      W : Four_Bytes;
      T : Four_Bytes;

      function VTT is new Ada.Unchecked_Conversion (Word_Type, Four_Bytes);
      function TTV is new Ada.Unchecked_Conversion (Four_Bytes, Word_Type);

   begin  -- Swap_Bytes
      T := VTT (Value);
      W.B0 := T.B3;
      W.B1 := T.B2;
      W.B2 := T.B1;
      W.B3 := T.B0;
      return TTV (W);
   end Swap_Bytes;

   ---------------------------------------------------------------------------

   -- Swap bytes if host is little-endian, or no-op if it's big-endian
   function To_Big (Value : Word_Type) return Word_Type is
   begin  -- To_Big
      if System_Byte_Order /= High_Order_First then
         return Swap_Bytes (Value);
      else
         return Value;
      end if;
   end To_Big;

   ---------------------------------------------------------------------------

   -- Swap bytes if host is big-endian, or no-op if it's little-endian
   function To_Little (Value : Word_Type) return Word_Type is
   begin  -- To_Little
      if System_Byte_Order /= Low_Order_First then
         return Swap_Bytes (Value);
      else
         return Value;
      end if;
   end To_Little;

   ---------------------------------------------------------------------------

   -- Swap the bytes, no matter the host ordering
   procedure Swap_Bytes (Value : in out Word_Type) is

      W : Four_Bytes;
      T : Four_Bytes;

      function VTT is new Ada.Unchecked_Conversion (Word_Type, Four_Bytes);
      function TTV is new Ada.Unchecked_Conversion (Four_Bytes, Word_Type);

   begin  -- Swap_Bytes
      T := VTT (Value);
      W.B0 := T.B3;
      W.B1 := T.B2;
      W.B2 := T.B1;
      W.B3 := T.B0;
      Value := TTV (W);
   end Swap_Bytes;

   ---------------------------------------------------------------------------

   -- Swap bytes if host is little-endian, or no-op if it's big-endian
   procedure To_Big (Value : in out Word_Type) is
   begin  -- To_Big
      if System_Byte_Order /= High_Order_First then
         Swap_Bytes (Value);
      end if;
   end To_Big;

   ---------------------------------------------------------------------------

   -- Swap bytes if host is big-endian, or no-op if it's little-endian
   procedure To_Little (Value : in out Word_Type) is
   begin  -- To_Little
      if System_Byte_Order /= Low_Order_First then
         Swap_Bytes (Value);
      end if;
   end To_Little;

   ---------------------------------------------------------------------------

end Lumen.Binary.Endian.Words;
