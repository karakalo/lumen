
-- Lumen.Byte_IO -- Read and write streams of bytes from external files.
--
-- Chip Richards, NiEstu, Phoenix AZ, Summer 2010

-- Lumen would not be possible without the support and contributions of a cast
-- of thousands, including and primarily Rod Kay.

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
with Ada.Streams.Stream_IO;
with Lumen.Binary;
with System.Address_To_Access_Conversions;


package body Lumen.Byte_IO is

   ---------------------------------------------------------------------------

   -- Open a file for reading
   procedure Open (File     : in out File_Type;
                   Pathname : in     String) is
   begin  -- Open

      -- Just regular stream open, for reading
      Ada.Streams.Stream_IO.Open (File => File,
                                  Mode => Ada.Streams.Stream_IO.In_File,
                                  Name => Pathname);

   exception
      when Ada.Streams.Stream_IO.Name_Error =>
         raise Nonexistent_File;
      when Ada.Streams.Stream_IO.Use_Error =>
         raise Unreadable_File;
      when Ada.Streams.Stream_IO.Device_Error =>
         raise Access_Failed;
      when others =>
         raise Unknown_Error;
   end Open;

   ---------------------------------------------------------------------------

   -- Close open file
   procedure Close  (File : in out File_Type) is
   begin  -- Close
      Ada.Streams.Stream_IO.Close (File);
   end Close;

   ---------------------------------------------------------------------------

   -- Read and return a stream of bytes up to the given length
   function Read (File   : File_Type;
                  Length : Positive)
   return Binary.Byte_String is

      use Ada.Streams;

      -- Pointer-fumbling routines used to read data without copying it
      subtype SEA is Stream_Element_Array (1 .. Stream_Element_Offset (Length));
      package SEA_Addr is new System.Address_To_Access_Conversions (SEA);

      Into : Binary.Byte_String (1 .. Length);
      Item : SEA_Addr.Object_Pointer := SEA_Addr.To_Pointer (Into'Address);
      Got  : Stream_Element_Offset;

   begin  -- Read

      Stream_IO.Read (File, Item.all, Got);
      return Into (Into'First .. Natural (Got));

   exception
      when others =>
         raise Read_Error;
   end Read;

   ---------------------------------------------------------------------------

   -- Read and return a stream of bytes up to the given length
   procedure Read (File : in     File_Type;
                   Item :    out Binary.Byte_String;
                   Last :    out Natural) is

      use Ada.Streams;

      -- Pointer-fumbling routines used to read data without copying it
      subtype SEA is Stream_Element_Array (Stream_Element_Offset (Item'First) .. Stream_Element_Offset (Item'Last));
      package SEA_Addr is new System.Address_To_Access_Conversions (SEA);

      Into : SEA_Addr.Object_Pointer := SEA_Addr.To_Pointer (Item'Address);
      Got  : Stream_Element_Offset;

   begin  -- Read

      Stream_IO.Read (File, Into.all, Got);
      Last := Natural (Got);

   exception
     when others =>
        raise Read_Error;
   end Read;

   ---------------------------------------------------------------------------

   -- Writing will go here

   ---------------------------------------------------------------------------

end Lumen.Byte_IO;
