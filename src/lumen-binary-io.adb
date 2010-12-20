
-- Lumen.Binary.IO -- Read and write streams of binary data from external
--                    files.
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
with Ada.Streams.Stream_IO;
with System.Address_To_Access_Conversions;


package body Lumen.Binary.IO is

   ---------------------------------------------------------------------------

   -- Useful constant for our short I/O routines
   Bytes_Per_Short : constant := Short'Size / Ada.Streams.Stream_Element'Size;
   Bytes_Per_Word  : constant := Word'Size  / Ada.Streams.Stream_Element'Size;

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

   -- Create a file for writing
   procedure Create (File     : in out File_Type;
                     Pathname : in     String) is
   begin  -- Create

      -- Just regular stream create, for writing
      Ada.Streams.Stream_IO.Create (File => File,
                                    Mode => Ada.Streams.Stream_IO.Out_File,
                                    Name => Pathname);

   exception
      when Ada.Streams.Stream_IO.Name_Error =>
         raise Malformed_Name;
      when Ada.Streams.Stream_IO.Use_Error =>
         raise Unwriteable_File;
      when Ada.Streams.Stream_IO.Device_Error =>
         raise Access_Failed;
      when others =>
         raise Unknown_Error;
   end Create;

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
   return Byte_String is

      use Ada.Streams;

      -- Pointer-fumbling routines used to read data without copying it
      subtype SEA is Stream_Element_Array (1 .. Stream_Element_Offset (Length));
      package SEA_Addr is new System.Address_To_Access_Conversions (SEA);

      Into : Byte_String (1 .. Length);
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

   -- Read and return a stream of bytes up to the length of the given buffer
   procedure Read (File : in     File_Type;
                   Item :    out Byte_String;
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

   -- Read and return a stream of shorts up to the given length
   function Read (File   : File_Type;
                  Length : Positive)
   return Short_String is

      use Ada.Streams;

      -- Pointer-fumbling routines used to read data without copying it
      subtype SEA is Stream_Element_Array (1 .. Stream_Element_Offset (Length * Bytes_Per_Short));
      package SEA_Addr is new System.Address_To_Access_Conversions (SEA);

      Into : Short_String (1 .. Length);
      Item : SEA_Addr.Object_Pointer := SEA_Addr.To_Pointer (Into'Address);
      Got  : Stream_Element_Offset;

   begin  -- Read

      Stream_IO.Read (File, Item.all, Got);
      return Into (Into'First .. Natural (Got / Bytes_Per_Short));

   exception
      when others =>
         raise Read_Error;
   end Read;

   ---------------------------------------------------------------------------

   -- Read and return a stream of shorts up to the length of the given buffer
   procedure Read (File : in     File_Type;
                   Item :    out Short_String;
                   Last :    out Natural) is

      use Ada.Streams;

      -- Pointer-fumbling routines used to read data without copying it
      subtype SEA is Stream_Element_Array (1 .. Stream_Element_Offset (Item'Length * Bytes_Per_Short));
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

   -- Read and return a stream of words up to the given length
   function Read (File   : File_Type;
                  Length : Positive)
   return Word_String is

      use Ada.Streams;

      -- Pointer-fumbling routines used to read data without copying it
      subtype SEA is Stream_Element_Array (1 .. Stream_Element_Offset (Length * Bytes_Per_Word));
      package SEA_Addr is new System.Address_To_Access_Conversions (SEA);

      Into : Word_String (1 .. Length);
      Item : SEA_Addr.Object_Pointer := SEA_Addr.To_Pointer (Into'Address);
      Got  : Stream_Element_Offset;

   begin  -- Read

      Stream_IO.Read (File, Item.all, Got);
      return Into (Into'First .. Natural (Got / Bytes_Per_Word));

   exception
      when others =>
         raise Read_Error;
   end Read;

   ---------------------------------------------------------------------------

   -- Read and return a stream of words up to the length of the given buffer
   procedure Read (File : in     File_Type;
                   Item :    out Word_String;
                   Last :    out Natural) is

      use Ada.Streams;

      -- Pointer-fumbling routines used to read data without copying it
      subtype SEA is Stream_Element_Array (1 .. Stream_Element_Offset (Item'Length * Bytes_Per_Word));
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

   -- Write a stream of bytes
   procedure Write (File : in File_Type;
                    Item : in Byte_String) is

      use Ada.Streams;

      -- Pointer-fumbling routines used to write data without copying it
      subtype SEA is Stream_Element_Array (Stream_Element_Offset (Item'First) .. Stream_Element_Offset (Item'Last));
      package SEA_Addr is new System.Address_To_Access_Conversions (SEA);

      From : SEA_Addr.Object_Pointer := SEA_Addr.To_Pointer (Item'Address);

   begin  -- Write

      Stream_IO.Write (File, From.all);

   exception
     when others =>
        raise Write_Error;
   end Write;

   ---------------------------------------------------------------------------

end Lumen.Binary.IO;
