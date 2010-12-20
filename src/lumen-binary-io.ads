
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


package Lumen.Binary.IO is

   -- Our interpretation of the various file exceptions
   Access_Failed    : exception;
   Malformed_Name   : exception;
   Nonexistent_File : exception;
   Read_Error       : exception;
   Unknown_Error    : exception;
   Unreadable_File  : exception;
   Unwriteable_File : exception;
   Write_Error      : exception;

   -- Our version of the Stream_IO file type
   subtype File_Type is Ada.Streams.Stream_IO.File_Type;

   -- Open a file for reading
   procedure Open (File     : in out File_Type;
                   Pathname : in     String);

   -- Create a file for writing
   procedure Create (File     : in out File_Type;
                     Pathname : in     String);

   -- Close open file
   procedure Close  (File : in out File_Type);

   -- Read and return a stream of bytes up to the given length
   function Read (File     : File_Type;
                  Length   : Positive)
   return Byte_String;

   -- Read and return a stream of bytes up to the length of the given buffer
   procedure Read (File : in     File_Type;
                   Item :    out Byte_String;
                   Last :    out Natural);

   -- Read and return a stream of shorts up to the given length
   function Read (File     : File_Type;
                  Length   : Positive)
   return Short_String;

   -- Read and return a stream of shorts up to the length of the given buffer
   procedure Read (File : in     File_Type;
                   Item :    out Short_String;
                   Last :    out Natural);

   -- Read and return a stream of words up to the given length
   function Read (File     : File_Type;
                  Length   : Positive)
   return Word_String;

   -- Read and return a stream of words up to the length of the given buffer
   procedure Read (File : in     File_Type;
                   Item :    out Word_String;
                   Last :    out Natural);

   -- Write a stream of bytes
   procedure Write (File : in File_Type;
                    Item : in Byte_String);

end Lumen.Binary.IO;
