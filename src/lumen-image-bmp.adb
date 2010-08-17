
-- Lumen.Image.BMP -- Load and save Microsoft BMP image data

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
with Ada.Text_IO;

package body Lumen.Image.BMP is

   function From_File (File : in Binary.IO.File_Type) return Descriptor is
      Result : Descriptor;

      use type Binary.Byte;
      use type Binary.Short;
      use type Binary.Word;
      use type Binary.S_Word;
      package Byte_IO is new Ada.Text_IO.Modular_IO (Binary.Byte);
      package Short_IO is new Ada.Text_IO.Modular_IO (Binary.Short);
      package S_Short_IO is new Ada.Text_IO.Integer_IO (Binary.S_Short);
      package Word_IO is new Ada.Text_IO.Modular_IO (Binary.Word);
      package S_Word_IO is new Ada.Text_IO.Integer_IO (Binary.S_Word);

      -- File Header
      type Bitmap_File_Header is record
            Magic      : Binary.Byte_String (1..2); -- Magic Number
            File_Size  : Binary.Word;  -- File Size in Bytes
            Reserved   : Binary.Short_String (1..2); -- Reserved
            Offset     : Binary.Word;  -- Offset to Bitmap Data
         end record;
      pragma Pack(Bitmap_File_Header);
      for Bitmap_File_Header'Size use 14 * Binary.Byte_Bits; -- 14 Bytes long

      -- Bitmap Version
      type Bitmap_Version is (V1, V3, V2, V4, V5);
      pragma Convention(C, Bitmap_Version);
      for Bitmap_Version use (V1 => 12, V3 => 40, V2 => 64, V4 => 108, V5 => 128);
      for Bitmap_Version'Size use Binary.Word_Bits;

      package Version_IO is new Ada.Text_IO.Enumeration_IO(Bitmap_Version);

      -- Compression Method
      type Compression_Method is (BI_RGB, BI_RLE8, BI_RLE4, BI_BITFIELDS, BI_JPEG, BI_PNG);
      pragma Convention(C, Compression_Method);
      for Compression_Method use (BI_RGB => 0, BI_RLE8 => 1, BI_RLE4 => 2, BI_BITFIELDS =>3,
                                  BI_JPEG => 4, BI_PNG => 5);
      for Compression_Method'Size use Binary.Word_Bits;

      package Compression_IO is new Ada.Text_IO.Enumeration_IO(Compression_Method);

      -- Version 1
      type V1_Info_Header is record
            Width : Binary.Short;  -- width of image in pixels
            Height : Binary.S_Short; -- height of image in pixels
            Planes : Binary.Short; -- number of planes for target device, must be 1
            Bit_Count : Binary.Short; -- number of bits per pixel / max number of colors
         end record;
      pragma Pack (V1_Info_Header);
      for V1_Info_Header'Size use 8 * Binary.Byte_Bits; -- 8 Bytes + Version => 12 Bytes long

      -- Version 3
      type V3_Info_Header is record
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
      pragma Pack (V3_Info_Header);
      for V3_Info_Header'Size use 36 * Binary.Byte_Bits; -- 36 Bytes + Version => 40 Bytes long

      -- Version 2 = Version 3 + Extra
      type V2_Info_Header_Extra is record
            Resolution_Unit : Binary.Short;
            Reserved : Binary.Short;
            Orientation : Binary.Short;
            Halftoning : Binary.Short;
            Halftone_Size_1 : Binary.Word;
            Halftone_Size_2 : Binary.Word;
            Color_Space : Binary.Word;
            App_Data : Binary.Word;
         end record;
      pragma Pack (V2_Info_Header_Extra);
      for V2_Info_Header_Extra'Size use 24 * Binary.Byte_Bits; -- 24 Bytes + Version + V3 => 64 Bytes long

      -- Version 4 = Version 3 + Extra
      type V4_Info_Header_Extra is record
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
      pragma Pack (V4_Info_Header_Extra);
      for V4_Info_Header_Extra'Size use 68 * Binary.Byte_Bits; -- 68 Bytes + Version + V3 => 108 Bytes long

      -- Version 5 = Version 3 + Version 4 + Extra
      type V5_Info_Header_Extra is record
            Intent : Binary.Word; -- rendering intent for bitmap
            Profile_Data : Binary.Word; -- offset in bytes from beginning of Bitmap_V5_Header to start of profile data, or the data itself
            Profile_Size : Binary.Word; -- size in bytes of embedded profile data
            Reserved : Binary.Word; -- reserved, should be 0
         end record;
      pragma Pack (V5_Info_Header_Extra);
      for V5_Info_Header_Extra'Size use 16 * Binary.Byte_Bits; -- 16 Bytes + Version + V3 + V4 => 124 Bytes long

      File_Header : Bitmap_File_Header; -- File Header
      The_Version : Bitmap_Version; -- Bitmap Version
      BPP         : Binary.Short; -- Bits Per Pixel (1, 4, 8, 16, 24, 32)
      Reversed    : Boolean := False; -- Top/Bottom reversed
      Compression : Compression_Method := BI_RGB; -- used compression method

      procedure Read_One_Bit_Format is
      begin
         -- read palette
         raise Invalid_Format;
      end Read_One_Bit_Format;

      procedure Read_Four_Bit_Format is
      begin
         -- read palette
         raise Invalid_Format;
      end Read_Four_Bit_Format;

      procedure Read_Eight_Bit_Format is
      begin
         -- read palette
         raise Invalid_Format;
      end Read_Eight_Bit_Format;

      procedure Read_RGB_Format is
         Row_Size : Natural := Natural(Result.Width) * 3; -- 3 Bytes for color information
         Padding  : Natural := (-Row_Size) mod 4; -- padding
         Row_Buf  : Binary.Byte_String (1 .. Row_Size + Padding);
         Last     : Natural;
         Row,The_Row : Integer;
      begin
         Ada.Streams.Stream_IO.Set_Index (File, Ada.Streams.Stream_IO.Count((File_Header.Offset) + 1));

         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line("Reading RGB Format:");
         Ada.Text_IO.Put_Line("Row Size: " & Natural'Image(Row_Size));
         Ada.Text_IO.Put_Line("Padding:  " & Natural'Image(Padding));

         Row := Result.Values'Last(1);
         while Row >= Result.Values'First(1) loop
            Binary.IO.Read (File, Row_Buf, Last);
            if Last /= Row_Size + Padding then
               raise Invalid_Format;
            end if;
            if Reversed then -- invert the order
               The_Row := Result.Values'Last(1) - Row + Result.Values'First(1);
            else
               The_Row := Row;
            end if;
            for Col in Result.Values'Range(2) loop
               Result.Values (The_Row, Col).B := Row_Buf (Col * 3 - 2);
               Result.Values (The_Row, Col).G := Row_Buf (Col * 3 - 1);
               Result.Values (The_Row, Col).R := Row_Buf (Col * 3 - 0);
            end loop;
            Row := Row - 1;
         end loop;
      end Read_RGB_Format;

      procedure Read_Old_Header is
         The_Header : V1_Info_Header;
      begin
         V1_Info_Header'Read (Ada.Streams.Stream_IO.Stream(File), The_Header);
         Result.Width := Integer(The_Header.Width);
         Result.Height := Integer(The_Header.Height);
         BPP := The_Header.Bit_Count;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("V1 Info Header:");
      Ada.Text_IO.Put_Line("===============");
      Ada.Text_IO.Put("Width:     "); Short_IO.Put(The_Header.Width,0); Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Height:    "); S_Short_IO.Put(The_Header.Height,0); Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Bit_Count: "); Short_IO.Put(The_Header.Bit_Count,0); Ada.Text_IO.New_Line;

      end Read_Old_Header;

      procedure Read_Header is
         The_Header : V3_Info_Header;
      begin
         V3_Info_Header'Read (Ada.Streams.Stream_IO.Stream(File), The_Header);
         Result.Width := Integer(The_Header.Width);
         Result.Height := Integer(The_Header.Height);
         BPP := The_Header.Bit_Count;
         Compression := The_Header.Compression;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("V3 Info Header:");
      Ada.Text_IO.Put_Line("===============");
      Ada.Text_IO.Put("Width:       "); S_Word_IO.Put(The_Header.Width,0); Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Height:      "); S_Word_IO.Put(The_Header.Height,0); Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Bit_Count:   "); Short_IO.Put(The_Header.Bit_Count,0); Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Compression: "); Compression_IO.Put(The_Header.Compression,0); Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Colors_Used: "); Word_IO.Put(The_Header.Colors_Used,0); Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Imp_Colors:  "); Word_IO.Put(The_Header.Important_Colors,0); Ada.Text_IO.New_Line;

         if The_Version = V2 then
            declare
               V2_Header : V2_Info_Header_Extra;
            begin
               V2_Info_Header_Extra'Read (Ada.Streams.Stream_IO.Stream(File), V2_Header);
               -- TODO: process extra information provided by V2-Header
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("V2 Info Header:");
      Ada.Text_IO.Put_Line("===============");
            end;
         elsif The_Version >= V4 then
            declare
               V4_Header : V4_Info_Header_Extra;
            begin
               V4_Info_Header_Extra'Read (Ada.Streams.Stream_IO.Stream(File), V4_Header);
               -- TODO: process extra information provided by V4-Header
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("V4 Info Header:");
      Ada.Text_IO.Put_Line("===============");
               if The_Version = V5 then
                  declare
                     V5_Header : V5_Info_Header_Extra;
                  begin
                     V5_Info_Header_Extra'Read (Ada.Streams.Stream_IO.Stream(File), V5_Header);
                     -- TODO: process extra information provided by V5-Header
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("V5 Info Header:");
      Ada.Text_IO.Put_Line("===============");
                  end;
               end if;
            end;
         end if;
      end Read_Header;

   begin

      Ada.Streams.Stream_IO.Set_Index (File, 1);

      -- Read the Bitmap File Header
      Bitmap_File_Header'Read (Ada.Streams.Stream_IO.Stream(File), File_Header);

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("BMP File Header:");
      Ada.Text_IO.Put_Line("================");
      Ada.Text_IO.Put("Magic:     "); Byte_IO.Put(File_Header.Magic(1),0,16); Ada.Text_IO.Put(" "); Byte_IO.Put(File_Header.Magic(2),0,16); Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("File_Size: "); Word_IO.Put(File_Header.File_Size,0); Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Reserved:  "); Short_IO.Put(File_Header.Reserved(1),0); Ada.Text_IO.Put(" "); Short_IO.Put(File_Header.Reserved(2),0); Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Offset:    "); Word_IO.Put(File_Header.Offset,0,16); Ada.Text_IO.New_Line;

      if File_Header.Magic(1) /= 16#42# or File_Header.Magic(2) /= 16#4D# then
         raise Invalid_Format;
      end if;

      if Binary.Word(Ada.Streams.Stream_IO.Size(File)) /= File_Header.File_Size then
         Ada.Text_IO.Put_Line("WARNING: File Size mismatch");
      end if;

      Bitmap_Version'Read (Ada.Streams.Stream_IO.Stream(File), The_Version);

      case The_Version is
         when V1 => -- Version 1 has different datatypes for width/height
            Read_Old_Header;
         when V2|V3|V4|V5 =>
            Read_Header;
      end case;

      -- Are the lines of the image in reversed order?
      if Result.Height < 0 then
         Reversed := True;
         Result.Height := -Result.Height;
      end if;

      -- initialize pixel matrix
      Result.Values   := new Pixel_Matrix (1..Result.Height, 1..Result.Width);
      Result.Complete := True;

      -- read color palette (if necessary)
      case BPP is
         when 1  => Read_One_Bit_Format;
         when 4  => Read_Four_Bit_Format;
         when 8  => Read_Eight_Bit_Format;
--         when 16 => Read_Sixteen_Bit_Format;
         when 24 => Read_RGB_Format;
--         when 32 => Read_Thirtytwo_Bit_Format;
         when others => raise Invalid_Format;
      end case;
      -- read bitmap data
      -- uncompress data (if necessary)

      return Result;
   end From_File;

end Lumen.Image.BMP;
