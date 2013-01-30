
-- Lumen.Shader -- Helper routines to fetch shader source, load it, and compile it
--
-- Chip Richards, NiEstu, Phoenix AZ, Winter 2013

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

with Ada.Directories;
with Ada.Streams.Stream_IO;
with Interfaces.C;
with System;

with Lumen.Binary;

use type Lumen.Binary.Byte;

package body Lumen.Shader is

   ---------------------------------------------------------------------------

   -- Read shader source from a disk file
   procedure From_File (Shader_Type : in GL.Enum;
                        Name        : in String;
                        ID          : out GL.UInt;
                        Success     : out Boolean) is

      Result  : GL.UInt;
      Size    : Ada.Directories.File_Size := Ada.Directories.Size (Name);
      Status  : GL.Int;

   begin  -- From_File

      -- Tell the GPU to create a new shader
      Result := GL.Create_Shader (Shader_Type);
      ID := Result;

      -- Read shader source from file
      declare
         use Ada.Streams;
         use type Ada.Directories.File_Size;

         File       : Stream_IO.File_Type;
         Source     : Stream_Element_Array (1 .. Stream_Element_Offset  (Size + 1));  -- +1 = room for terminating NUL
         Last       : Stream_Element_Offset;
         Source_Ptr : GL.Pointer := Source'Address;
      begin

         -- Open the file and try to read it all in one gulp, then close it
         Stream_IO.Open (File, Stream_IO.In_File, Name);
         Stream_IO.Read (File, Source, Last);
         Stream_IO.Close (File);

         -- Double-check that we got it all
         if Last /= Stream_Element_Offset (Size) then
            raise Read_Error with "Got only" & Stream_Element_Offset'Image (Last) & " bytes out of" & Ada.Directories.File_Size'Image (Size);
         end if;

         -- Add a NUL byte to the end of the source string
         Source (Source'Last) := 0;

         -- Pump the shader source down to the GPU
         GL.Shader_Source (Result, 1, Source_Ptr'Address, System.Null_Address);
      end;

      -- Tell it to compile the source
      GL.Compile_Shader (Result);

      -- Check that compile worked, return status to caller
      GL.Get_Shader (Result, GL.GL_COMPILE_STATUS, Status'Address);
      Success := GL.Bool (Status) = GL.GL_TRUE;

      return;

   end From_File;

   ---------------------------------------------------------------------------

   -- Use shader source provided in a string
   procedure From_String (Shader_Type : in GL.Enum;
                          Source      : in String;
                          ID          : out GL.UInt;
                          Success     : out Boolean) is

      Result     : GL.UInt;
      Text       : String := Source & ASCII.NUL;  -- doesn't hurt if caller already did it
      Source_Ptr : GL.Pointer := Text'Address;
      Status     : GL.Int;

   begin  -- From_String

      -- Tell the GPU to create a new shader
      Result := GL.Create_Shader (Shader_Type);
      ID := Result;

      -- Pump the shader source down to the GPU
      GL.Shader_Source (Result, 1, Source_Ptr'Address, System.Null_Address);

      -- Tell it to compile the source
      GL.Compile_Shader (Result);

      -- Check that compile worked
      GL.Get_Shader (Result, GL.GL_COMPILE_STATUS, Status'Address);
      Success := GL.Bool (Status) = GL.GL_TRUE;

      return;

   end From_String;

   ---------------------------------------------------------------------------

   function Get_Info_Log (Shader : GL.UInt) return String is

      Log_Len : GL.Int;

   begin  -- Get_Info_Log

      GL.Get_Shader (Shader, GL.GL_INFO_LOG_LENGTH, Log_Len'Address);

      declare
         Log : Interfaces.C.char_array (1 .. Interfaces.C.size_t (Log_Len));
         Got : GL.SizeI;
      begin
         GL.Get_Shader_Info_Log (Shader, Log'Length, Got'Address, Log'Address);
         return Interfaces.C.To_Ada (Log);
      end;

   end Get_Info_Log;

   ---------------------------------------------------------------------------

end Lumen.Shader;
