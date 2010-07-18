
-- Lumen.Image.BMP -- Load and save netpbm's PPM image data

-- This code is covered by the ISC License:
--
-- Copyright © 2010, Julian Leyh <jleyh@auroraux.org>
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

package body Lumen.Image.BMP is

   function From_File (File : in Binary.IO.File_Type) return Descriptor is
      Result : Descriptor;

      use type Binary.Byte;
      use type Binary.Short;
      use type Binary.Word;
      use type Binary.S_Word;

      -- File Header
      type Bitmap_File_Header is record
            Magic       : Binary.Byte_String (1..2); -- Magic Number
            File_Size   : Binary.Word;  -- File Size in Bytes
            Reserved1   : Binary.Short; -- Reserved
            Reserved2   : Binary.Short; -- Reserved
            Offset_Bits : Binary.Word;  -- Offset to Bitmap Data in Bits
         end record;
      pragma Pack(Bitmap_File_Header);
      for Bitmap_File_Header'Size use 14 * Binary.Byte_Bits; -- 14 Bytes long

      -- Bitmap Version
      type Bitmap_Version is range 1 .. 5;

      -- Compression Method
      type Compression_Method is (BI_RGB, BI_RLE8, BI_RLE4, BI_BITFIELDS, BI_JPEG, BI_PNG);
      pragma Convention(C, Compression_Method);
      for Compression_Method use (BI_RGB => 0, BI_RLE8 => 1, BI_RLE4 => 2, BI_BITFIELDS =>3,
                                  BI_JPEG => 4, BI_PNG => 5);
      for Compression_Method'Size use Binary.Word_Bits;

      -- Version 1
      type Bitmap_Core_Header is record
            Header_Size : Binary.Word; -- Size of this Structure
            Width : Binary.Short;  -- width of image in pixels
            Height : Binary.S_Short; -- height of image in pixels
            Planes : Binary.Short; -- number of planes for target device, must be 1
            Bit_Count : Binary.Short; -- number of bits per pixel / max number of colors
         end record;
      pragma Pack (Bitmap_Core_Header);
      for Bitmap_Core_Header'Size use 12 * Binary.Byte_Bits; -- 12 Bytes long

      -- Version 2
      type Bitmap_Core_Header_2 is record
            Header_Size : Binary.Word; -- Size of this Structure
            Width : Binary.S_Word;  -- width of image in pixels
            Height : Binary.S_Word; -- height of image in pixels
            Planes : Binary.Short; -- number of planes for target device, must be 1
            Bit_Count : Binary.Short; -- number of bits per pixel / max number of colors
            Compression : Compression_Method; -- used compression method
            Image_Size : Binary.Word; -- size of image in bytes, may be 0
            XRes : Binary.Word; -- horizontal resolution in pixels-per-meter
            YRes : Binary.Word; -- vertical resolution in pixels-per-meter
            Colors_Used : Binary.Word; -- number of color indexes in color table, may be 0
            Important_Colors : Binary.Word; -- number of color indexes that are required, may be 0
            Resolution_Unit : Binary.Short;
            Reserved : Binary.Short;
            Orientation : Binary.Short;
            Halftoning : Binary.Short;
            Halftone_Size_1 : Binary.Word;
            Halftone_Size_2 : Binary.Word;
            Color_Space : Binary.Word;
            App_Data : Binary.Word;
         end record;
      pragma Pack (Bitmap_Core_Header_2);
      for Bitmap_Core_Header_2'Size use 64 * Binary.Byte_Bits; -- 64 Bytes long

      -- Version 3
      type Bitmap_Info_Header is record
            Header_Size : Binary.Word; -- Size of this Structure
            Width : Binary.S_Word;  -- width of image in pixels
            Height : Binary.S_Word; -- height of image in pixels, may be negative
            Planes : Binary.Short; -- number of planes for target device, must be 1
            Bit_Count : Binary.Short; -- number of bits per pixel / max number of colors
            Compression : Compression_Method; -- used compression method
            Image_Size : Binary.Word; -- size of image in bytes, may be 0
            XRes : Binary.Word; -- horizontal resolution in pixels-per-meter
            YRes : Binary.Word; -- vertical resolution in pixels-per-meter
            Colors_Used : Binary.Word; -- number of color indexes in color table, may be 0
            Important_Colors : Binary.Word; -- number of color indexes that are required, may be 0
         end record;
      pragma Pack (Bitmap_Info_Header);
      for Bitmap_Info_Header'Size use 40 * Binary.Byte_Bits; -- 40 Bytes long

      -- Version 4
      type Bitmap_V4_Header is record
            Header_Size : Binary.Word; -- Size of this Structure
            Width : Binary.S_Word;  -- width of image in pixels
            Height : Binary.S_Word; -- height of image in pixels
            Planes : Binary.Short; -- number of planes for target device, must be 1
            Bit_Count : Binary.Short; -- number of bits per pixel / max number of colors
            Compression : Compression_Method; -- used compression method
            Image_Size : Binary.Word; -- size of image in bytes, may be 0
            XRes : Binary.Word; -- horizontal resolution in pixels-per-meter
            YRes : Binary.Word; -- vertical resolution in pixels-per-meter
            Colors_Used : Binary.Word; -- number of color indexes in color table, may be 0
            Important_Colors : Binary.Word; -- number of color indexes that are required, may be 0
            Red_Mask : Binary.Word; -- color mask for red component (BI_BITFIELDS compression)
            Green_Mask : Binary.Word; -- color mask for green component (BI_BITFIELDS compression)
            Blue_Mask : Binary.Word; -- color mask for blue component (BI_BITFIELDS compression)
            Alpha_Mask : Binary.Word; -- color mask for alpha component
            Color_Space_Type : Binary.Word; -- color space of the DIB
            Endpoints : Binary.Word_String(1..9); -- endpoints for the three colors for LCS_CALIBRATED_RGB
            Gamma_Red : Binary.Word; -- response curve for red (LCS_CALIBRATED_RGB)
            Gamma_Green : Binary.Word; -- response curve for green (LCS_CALIBRATED_RGB)
            Gamma_Blue : Binary.Word; -- response curve for blue (LCS_CALIBRATED_RGB)
         end record; -- Bitmap_V4_Header
      pragma Pack (Bitmap_V4_Header);
      for Bitmap_V4_Header'Size use 108 * Binary.Byte_Bits; -- 108 Bytes long
   
      -- Version 5
      type Bitmap_V5_Header is record
            Header_Size : Binary.Word; -- Size of this Structure
            Width : Binary.S_Word;  -- width of image in pixels
            Height : Binary.S_Word; -- height of image in pixels
            Planes : Binary.Short; -- number of planes for target device, must be 1
            Bit_Count : Binary.Short; -- number of bits per pixel / max number of colors
            Compression : Compression_Method; -- used compression method
            Image_Size : Binary.Word; -- size of image in bytes, may be 0
            XRes : Binary.Word; -- horizontal resolution in pixels-per-meter
            YRes : Binary.Word; -- vertical resolution in pixels-per-meter
            Colors_Used : Binary.Word; -- number of color indexes in color table, may be 0
            Important_Colors : Binary.Word; -- number of color indexes that are required, may be 0
            Red_Mask : Binary.Word; -- color mask for red component (BI_BITFIELDS compression)
            Green_Mask : Binary.Word; -- color mask for green component (BI_BITFIELDS compression)
            Blue_Mask : Binary.Word; -- color mask for blue component (BI_BITFIELDS compression)
            Alpha_Mask : Binary.Word; -- color mask for alpha component
            Color_Space_Type : Binary.Word; -- color space of the DIB
            Endpoints : Binary.Word_String(1..9); -- endpoints for the three colors for LCS_CALIBRATED_RGB
            Gamma_Red : Binary.Word; -- response curve for red (LCS_CALIBRATED_RGB)
            Gamma_Green : Binary.Word; -- response curve for green (LCS_CALIBRATED_RGB)
            Gamma_Blue : Binary.Word; -- response curve for blue (LCS_CALIBRATED_RGB)
            Intent : Binary.Word; -- rendering intent for bitmap
            Profile_Data : Binary.Word; -- offset in bytes from beginning of Bitmap_V5_Header to start of profile data, or the data itself
            Profile_Size : Binary.Word; -- size in bytes of embedded profile data
            Reserved : Binary.Word; -- reserved, should be 0
         end record;
      pragma Pack (Bitmap_V5_Header);
      for Bitmap_V5_Header'Size use 124 * Binary.Byte_Bits; -- 124 Bytes long

      type DIB is record
            Header_Size : Binary.Word; -- Size of this Structure
            Width : Binary.S_Word;  -- width of image in pixels
            Height : Binary.S_Word; -- height of image in pixels
--            Planes : Binary.Short; -- number of planes for target device, must be 1
            Bit_Count : Binary.Short; -- number of bits per pixel / max number of colors
            Compression : Compression_Method := BI_RGB; -- used compression method
            Image_Size : Binary.Word := 0; -- size of image in bytes, may be 0
--            XRes : Binary.Word; -- horizontal resolution in pixels-per-meter
--            YRes : Binary.Word; -- vertical resolution in pixels-per-meter
            Colors_Used : Binary.Word := 0; -- number of color indexes in color table, may be 0
--            Important_Colors : Binary.Word; -- number of color indexes that are required, may be 0

            Reversed : Boolean := False;
         end record;

      procedure Read_DIB (To : out DIB; Version : Bitmap_Version) is
         use type Ada.Streams.Stream_IO.Count;
      begin
         Ada.Streams.Stream_IO.Set_Index (File, Bitmap_File_Header'Size/8 + 1);
         case Version is
            when 1 =>
               declare
                  The_Header : Bitmap_Core_Header;
               begin
                  Bitmap_Core_Header'Read (Ada.Streams.Stream_IO.Stream(File), The_Header);
                  To.Header_Size := The_Header.Header_Size;
                  To.Width := Binary.S_Word(The_Header.Width);
                  To.Height := Binary.S_Word(The_Header.Height);
                  To.Bit_Count := The_Header.Bit_Count;
--                  To.Image_Size := The_Header.Image_Size;
--                  To.Colors_Used := The_Header.Colors_Used;
               end;
            when 2 =>
               declare
                  The_Header : Bitmap_Core_Header_2;
               begin
                  Bitmap_Core_Header_2'Read (Ada.Streams.Stream_IO.Stream(File), The_Header);
                  To.Header_Size := The_Header.Header_Size;
                  To.Width := The_Header.Width;
                  To.Height := The_Header.Height;
                  To.Compression := The_Header.Compression;
                  To.Bit_Count := The_Header.Bit_Count;
                  To.Colors_Used := The_Header.Colors_Used;
                  To.Image_Size := The_Header.Image_Size;
                  To.Colors_Used := The_Header.Colors_Used;
               end;
            when 3 =>
               declare
                  The_Header : Bitmap_Info_Header;
               begin
                  Bitmap_Info_Header'Read (Ada.Streams.Stream_IO.Stream(File), The_Header);
                  To.Header_Size := The_Header.Header_Size;
                  To.Width := The_Header.Width;
                  To.Height := The_Header.Height;
                  To.Compression := The_Header.Compression;
                  To.Bit_Count := The_Header.Bit_Count;
                  To.Colors_Used := The_Header.Colors_Used;
                  To.Image_Size := The_Header.Image_Size;
                  To.Colors_Used := The_Header.Colors_Used;
               end;
            when 4 =>
               declare
                  The_Header : Bitmap_V4_Header;
               begin
                  Bitmap_V4_Header'Read (Ada.Streams.Stream_IO.Stream(File), The_Header);
                  To.Header_Size := The_Header.Header_Size;
                  To.Width := The_Header.Width;
                  To.Height := The_Header.Height;
                  To.Compression := The_Header.Compression;
                  To.Bit_Count := The_Header.Bit_Count;
                  To.Colors_Used := The_Header.Colors_Used;
                  To.Image_Size := The_Header.Image_Size;
                  To.Colors_Used := The_Header.Colors_Used;
               end;
            when 5 =>
               declare
                  The_Header : Bitmap_V5_Header;
               begin
                  Bitmap_V5_Header'Read (Ada.Streams.Stream_IO.Stream(File), The_Header);
                  To.Header_Size := The_Header.Header_Size;
                  To.Width := The_Header.Width;
                  To.Height := The_Header.Height;
                  To.Compression := The_Header.Compression;
                  To.Bit_Count := The_Header.Bit_Count;
                  To.Colors_Used := The_Header.Colors_Used;
                  To.Image_Size := The_Header.Image_Size;
                  To.Colors_Used := The_Header.Colors_Used;
               end;
            when others =>
               raise Invalid_Format;
         end case;
         if To.Height < 0 then
            To.Height := -To.Height;
            To.Reversed := True;
         end if;
         if To.Colors_Used = 0 then
            To.Colors_Used := 2**Integer(To.Bit_Count);
         end if;
      end Read_DIB;

      File_Header : Bitmap_File_Header;
      Info_Header_Size : Binary.Word;
      The_Version : Bitmap_Version;
      The_DIB : DIB;

      procedure Read_One_Bit_Format is
      begin
         raise Invalid_Format;
      end;

      procedure Read_Four_Bit_Format is
      begin
         raise Invalid_Format;
      end;

      procedure Read_Eight_Bit_Format is
      begin
         raise Invalid_Format;
      end;

      procedure Read_RGB_Format is
         Row_Size : Natural := Natural(The_DIB.Width) * 3; -- 3 Bytes for color information
         Padding  : Natural := Row_Size mod 4; -- padding
         Row_Buf  : Binary.Byte_String (1 .. Row_Size + Padding);
         Last     : Natural;
         Col      : Natural;
      begin
         Ada.Streams.Stream_IO.Set_Index (File, Ada.Streams.Stream_IO.Count(File_Header.Offset_Bits / 8));

         for Row in Result.Values'Range (1) loop
            Binary.IO.Read (File, Row_Buf, Last);
            if Last /= Row_Size + Padding then
               raise Invalid_Format;
            end if;
            Col := 1;
            while Col < Natural(The_DIB.Height) loop
               Result.Values (Row, Col).R := Row_Buf (Col * 3 - 0);
               Result.Values (Row, Col).G := Row_Buf (Col * 3 - 1);
               Result.Values (Row, Col).B := Row_Buf (Col * 3 - 2);
               Col := Col + 1;
            end loop;
         end loop;
      end;

   begin

      Ada.Streams.Stream_IO.Set_Index (File, 1);

      Bitmap_File_Header'Read (Ada.Streams.Stream_IO.Stream(File), File_Header);

      if (File_Header.Magic(1) /= 16#42# or File_Header.Magic(2) /= 16#4D#) or else
         Binary.Word(Ada.Streams.Stream_IO.Size(File)) /= File_Header.File_Size then
         raise Invalid_Format;
      end if;

      Binary.Word'Read (Ada.Streams.Stream_IO.Stream(File), Info_Header_Size);

      case Info_Header_Size is
         when 12  => The_Version := 1;
         when 64  => The_Version := 2;
         when 40  => The_Version := 3;
         when 108 => The_Version := 4;
         when 124 => The_Version := 5;
         when others => raise Invalid_Format;
      end case;

      Read_DIB (The_DIB, The_Version);
      Result.Width    := Integer (The_DIB.Width);
      Result.Height   := Integer (The_DIB.Height);
      Result.Values   := new Pixel_Matrix (1..Result.Height, 1..Result.Width);
      Result.Complete := True;

      case The_DIB.Bit_Count is
         when 1 =>
            Read_One_Bit_Format;
         when 4 =>
            Read_Four_Bit_Format;
         when 8 =>
            Read_Eight_Bit_Format;
         when 24 =>
            Read_RGB_Format;
         when others =>
            raise Invalid_Format;
      end case;

      -- read color palette (if necessary)
      -- read bitmap data
      -- uncompress data (if necessary)

      return Result;
   end From_File;

end Lumen.Image.BMP;

