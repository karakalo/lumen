
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

package body Lumen.Image.BMP is

   function From_File (File : in Binary.IO.File_Type) return Descriptor is
      Result : Descriptor;

      use type Binary.Byte;
      use type Binary.Short;
      use type Binary.Word;
      use type Binary.S_Word;

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
      for Bitmap_Version use (V1 => 12, V3 => 40, V2 => 64, V4 => 108, V5 => 124);
      for Bitmap_Version'Size use Binary.Word_Bits;

      -- Compression Method
      type Compression_Method is (BI_RGB, BI_RLE8, BI_RLE4, BI_BITFIELDS, BI_JPEG, BI_PNG);
      pragma Convention(C, Compression_Method);
      for Compression_Method use (BI_RGB => 0, BI_RLE8 => 1, BI_RLE4 => 2, BI_BITFIELDS =>3,
                                  BI_JPEG => 4, BI_PNG => 5);
      for Compression_Method'Size use Binary.Word_Bits;

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

      subtype RGBQuad is Binary.Byte_String(1..4);
      type Palette is array (Natural range <>) of RGBQuad;

      type Color_Masks is record
            Red, Green, Blue, Alpha : Binary.Word;
            Red_Bits, Green_Bits, Blue_Bits, Alpha_Bits : Natural;
         end record;

      function Mask_Value (Value: Binary.Word; Mask: Binary.Word; Bits: Natural) return Binary.Byte is
         Bit_Count    : Natural := 0;
         Return_Value : Binary.Byte := 0;
      begin
         if Bits > 8 or Bits < 1 then
            return Return_Value;
         end if;
         for Bit in 0 .. Binary.Word_Bits-1 loop
            if (Mask and 2 ** Bit) /= 0 then
               if (Value and 2 ** Bit) /= 0 then
                  Return_Value := Return_Value or 2**Bit_Count;
               end if;
               Bit_Count := Bit_Count + 1;
            end if;
         end loop;
         Return_Value := Return_Value * 2**(8-Bits);
         return Return_Value;
      end Mask_Value;

      File_Header : Bitmap_File_Header; -- File Header
      The_Version : Bitmap_Version; -- Bitmap Version
      BPP         : Binary.Short; -- Bits Per Pixel (1, 4, 8, 16, 24, 32)
      Color_Count : Natural := 0;
      Reversed    : Boolean := False; -- Top/Bottom reversed
      Compression : Compression_Method := BI_RGB; -- used compression method
      Masks       : Color_Masks;

      procedure Read_One_Bit_Format is
         Colors : Palette (0..1);
         Components : Natural := 4;

         Row_Size : Natural := Natural(Result.Width) / Binary.Byte_Bits + 1; -- Row size in Bytes
         Padding  : Natural := (-Row_Size) mod 4; -- padding
         Row_Buf  : Binary.Byte_String (1 .. Row_Size + Padding);
         Last     : Natural;
         Row,The_Row,The_Col : Natural;

         type Bit is mod 2;
         type BitBytes is array (1..Binary.Byte_Bits) of Bit;
         pragma Pack(BitBytes);
         for BitBytes'Size use Binary.Byte_Bits;

         Byte_Of_Row  : Natural;
         Current_Byte : Binary.Byte;
         Current_Bits : BitBytes;
         for Current_Bits'Address use Current_Byte'Address;
      begin
         if The_Version = V1 then
            Components := 3;
         end if;
         -- read palette (two entries)
         for Color in Colors'Range loop
            Colors(Color)(1..Components) := Binary.IO.Read(File, Components);
         end loop;

         Ada.Streams.Stream_IO.Set_Index (File, Ada.Streams.Stream_IO.Count((File_Header.Offset) + 1));

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
            Byte_Of_Row := 0;
            for Col in Result.Values'Range(2) loop
               The_Col := Current_Bits'Last - (Col - 1) mod Binary.Byte_Bits;
               if The_Col = Current_Bits'Last then
                  Byte_Of_Row := Byte_Of_Row + 1;
                  Current_Byte := Row_Buf (Byte_Of_Row);
               end if;
               Result.Values (The_Row, Col).B := Colors(Natural(Current_Bits(The_Col)))(1);
               Result.Values (The_Row, Col).G := Colors(Natural(Current_Bits(The_Col)))(2);
               Result.Values (The_Row, Col).R := Colors(Natural(Current_Bits(The_Col)))(3);
            end loop;
            Row := Row - 1;
         end loop;
      end Read_One_Bit_Format;

      procedure Read_Four_Bit_Format is
         Colors : Palette (0..Color_Count-1);
         Components : Natural := 4;

         type HalfByte is mod 2**4;
         type Convert_Bytes is array (1..2) of HalfByte;
         pragma Pack(Convert_Bytes);
         for Convert_Bytes'Size use Binary.Byte_Bits;

         Current_Byte   : Binary.Byte;
         Current_Values : Convert_Bytes;
         for Current_Values'Address use Current_Byte'Address;
      begin
         if The_Version = V1 then
            Components := 3;
         end if;
         -- read palette
         for Color in Colors'Range loop
            Colors(Color)(1..Components) := Binary.IO.Read(File, Components);
         end loop;

         Ada.Streams.Stream_IO.Set_Index (File, Ada.Streams.Stream_IO.Count((File_Header.Offset) + 1));

         if Compression = BI_RGB then
            declare
               Row_Size : Natural := Natural(Result.Width) / 2 + 1; -- Row size in Bytes
               Padding  : Natural := (-Row_Size) mod 4; -- padding
               Row_Buf  : Binary.Byte_String (1 .. Row_Size + Padding);
               Last     : Natural;
               Row,The_Row,The_Col : Natural;
               Byte_Of_Row    : Natural;
            begin
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
                  Byte_Of_Row := 0;
                  for Col in Result.Values'Range(2) loop
                     The_Col := Current_Values'Last - (Col - 1) mod 2;
                     if The_Col = Current_Values'Last then
                        Byte_Of_Row := Byte_Of_Row + 1;
                        Current_Byte := Row_Buf (Byte_Of_Row);
                     end if;
                     Result.Values (The_Row, Col).B := Colors(Natural(Current_Values(The_Col)))(1);
                     Result.Values (The_Row, Col).G := Colors(Natural(Current_Values(The_Col)))(2);
                     Result.Values (The_Row, Col).R := Colors(Natural(Current_Values(The_Col)))(3);
                  end loop;
                  Row := Row - 1;
               end loop;
            end;
         elsif Compression = BI_RLE4 then
            declare
               Read_Buf : Binary.Byte_String(1..2);
               Last     : Natural;
               Done     : Boolean := False;
               Row, Col : Natural;
               function The_Row return Natural is
               begin
                  if Reversed then
                     return Result.Values'Last(1) - Row + Result.Values'First(1);
                  else
                     return Row;
                  end if;
               end The_Row;
            begin
               Row := Result.Values'Last(1);
               Col := Result.Values'First(2);
               while not done loop
                  Binary.IO.Read (File, Read_Buf, Last);
                  if Last /= Read_Buf'Length then
                     raise Invalid_Format;
                  end if;
                  if Read_Buf(1) = 0 then
                     if Read_Buf(2) >= 3 then -- Absolute Mode
                        declare
                           Bytes  : Natural := Natural(Read_Buf(2)) / 2;
                           Padding : Natural := (-Bytes) mod 2;
                           Values  : Binary.Byte_String(1 .. Bytes + Padding);
                        begin
                           Binary.IO.Read (File, Values, Last);
                           if Last /= Values'Length then
                              raise Invalid_Format;
                           end if;
                           for Byte in 1 .. Bytes loop
                              Current_Byte := Values(Byte);
                              for Value in Current_Values'Range loop
                                 Result.Values (The_Row, Col).B := Colors(Natural(Current_Values(3-Value)))(1);
                                 Result.Values (The_Row, Col).G := Colors(Natural(Current_Values(3-Value)))(2);
                                 Result.Values (The_Row, Col).R := Colors(Natural(Current_Values(3-Value)))(3);
                                 Col := Col + 1;
                              end loop;
                           end loop;
                        end;
                     elsif Read_Buf(2) = 0 then -- End of Line
                        Row := Row - 1;
                        Col := Result.Values'First (2);
                     elsif Read_Buf(2) = 1 then -- End of File
                        Done := True;
                     elsif Read_Buf(2) = 2 then -- Delta
                        declare
                           Offset : Binary.Byte_String(1 .. 2);
                        begin
                           Binary.IO.Read (File, Offset, Last);
                           if Last /= Offset'Length then
                              raise Invalid_Format;
                           end if;
                           Col := Col + Natural(Offset(1));
                           Row := Row - Natural(Offset(2));
                        end;
                     end if;
                  else -- Encoded Mode
                     declare
                        Times : Natural := Natural (Read_Buf (1));
                     begin
                        Current_Byte := Read_Buf (2);
                        for Time in 1 .. Times loop
                           if The_Row in Result.Values'Range (1) and Col in Result.Values'Range (2) then -- hack for BMP image test suite
                              Result.Values (The_Row, Col).B := Colors(Natural(Current_Values(Time mod 2 + 1)))(1);
                              Result.Values (The_Row, Col).G := Colors(Natural(Current_Values(Time mod 2 + 1)))(2);
                              Result.Values (The_Row, Col).R := Colors(Natural(Current_Values(Time mod 2 + 1)))(3);
                           end if;
                           Col := Col + 1;
                        end loop;
                     end;
                  end if;
               end loop;
            end;
         else
            raise Invalid_Format;
         end if;
      end Read_Four_Bit_Format;

      procedure Read_Eight_Bit_Format is
         Colors : Palette (0 .. Color_Count - 1);
         Components : Natural := 4;
      begin
         if The_Version = V1 then
            Components := 3;
         end if;
         -- read palette (max 2^8 entries)
         for Color in Colors'Range loop
            Colors(Color)(1..Components) := Binary.IO.Read(File, Components);
         end loop;

         Ada.Streams.Stream_IO.Set_Index (File, Ada.Streams.Stream_IO.Count((File_Header.Offset) + 1));

         if Compression = BI_RGB then
            declare
               Row_Size : Natural := Natural(Result.Width); -- Row size in Bytes
               Padding  : Natural := (-Row_Size) mod 4; -- padding
               Row_Buf  : Binary.Byte_String (1 .. Row_Size + Padding);
               Last     : Natural;
               Row,The_Row : Natural;
            begin
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
                     Result.Values (The_Row, Col).B := Colors(Integer(Row_Buf(Col)))(1);
                     Result.Values (The_Row, Col).G := Colors(Integer(Row_Buf(Col)))(2);
                     Result.Values (The_Row, Col).R := Colors(Integer(Row_Buf(Col)))(3);
                  end loop;
                  Row := Row - 1;
               end loop;
            end;
         elsif Compression = BI_RLE8 then
            declare
               Read_Buf : Binary.Byte_String(1..2);
               Last     : Natural;
               Done     : Boolean := False;
               Row, Col : Natural;
               function The_Row return Natural is
               begin
                  if Reversed then
                     return Result.Values'Last(1) - Row + Result.Values'First(1);
                  else
                     return Row;
                  end if;
               end The_Row;
            begin
               Row := Result.Values'Last(1);
               Col := Result.Values'First(2);
               while not done loop
                  Binary.IO.Read (File, Read_Buf, Last);
                  if Last /= Read_Buf'Length then
                     raise Invalid_Format;
                  end if;
                  if Read_Buf(1) = 0 then
                     if Read_Buf(2) >= 3 then -- Absolute Mode
                        declare
                           Bytes  : Natural := Natural(Read_Buf(2));
                           Padding : Natural := (-Bytes) mod 2;
                           Values  : Binary.Byte_String(1 .. Bytes + Padding);
                        begin
                           Binary.IO.Read (File, Values, Last);
                           if Last /= Values'Length then
                              raise Invalid_Format;
                           end if;
                           for Byte in 1 .. Bytes loop
                              Result.Values (The_Row, Col).B := Colors(Natural(Values(Byte)))(1);
                              Result.Values (The_Row, Col).G := Colors(Natural(Values(Byte)))(2);
                              Result.Values (The_Row, Col).R := Colors(Natural(Values(Byte)))(3);
                              Col := Col + 1;
                           end loop;
                        end;
                     elsif Read_Buf(2) = 0 then -- End of Line
                        Row := Row - 1;
                        Col := Result.Values'First (2);
                     elsif Read_Buf(2) = 1 then -- End of File
                        Done := True;
                     elsif Read_Buf(2) = 2 then -- Delta
                        declare
                           Offset : Binary.Byte_String(1 .. 2);
                        begin
                           Binary.IO.Read (File, Offset, Last);
                           if Last /= Offset'Length then
                              raise Invalid_Format;
                           end if;
                           Col := Col + Natural(Offset(1));
                           Row := Row - Natural(Offset(2));
                        end;
                     end if;
                  else -- Encoded Mode
                     declare
                        Times : Natural := Natural (Read_Buf (1));
                        Value : Natural := Natural (Read_Buf (2));
                     begin
                        for I in 1 .. Times loop
                           Result.Values (The_Row, Col).B := Colors(Value)(1);
                           Result.Values (The_Row, Col).G := Colors(Value)(2);
                           Result.Values (The_Row, Col).R := Colors(Value)(3);
                           Col := Col + 1;
                        end loop;
                     end;
                  end if;
               end loop;
            end;
         else
            raise Invalid_Format;
         end if;
      end Read_Eight_Bit_Format;

      procedure Read_RGB_Format is
         Row_Size : Natural := Natural(Result.Width) * 3; -- 3 Bytes for color information
         Padding  : Natural := (-Row_Size) mod 4; -- padding
         Row_Buf  : Binary.Byte_String (1 .. Row_Size + Padding);
         Last     : Natural;
         Row,The_Row : Natural;
      begin
         Ada.Streams.Stream_IO.Set_Index (File, Ada.Streams.Stream_IO.Count((File_Header.Offset) + 1));

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

      procedure Read_Sixteen_Bit_Format is
         Row_Size : Natural := Natural(Result.Width); -- Size (in Shorts)
         Padding  : Natural := (-Row_Size) mod (4 / Binary.Short_Bytes); -- padding
         Row_Buf  : Binary.Short_String (1 .. Row_Size + Padding);
         Last     : Natural;
         Row,The_Row : Natural;

         type Color_Part is mod 2**5; -- 5 bit for each R/G/B part
         type Padding_Bit is mod 2;
         type RGB is record
               B, G, R : Color_Part;
               Padding : Padding_Bit;
            end record;
         pragma Pack(RGB);
         for RGB'Size use Binary.Short_Bits;

         Current_Short : Binary.Short;
         Current_Value : RGB;
         for Current_Value'Address use Current_Short'Address;
      begin
         Ada.Streams.Stream_IO.Set_Index (File, Ada.Streams.Stream_IO.Count((File_Header.Offset) + 1));

         Row := Result.Values'Last(1);
         while Row >= Result.Values'First(1) loop
            Binary.IO.Read (File, Row_Buf, Last);
            if Last /= (Row_Size + Padding) * Binary.Short_Bytes then
               raise Invalid_Format;
            end if;
            if Reversed then -- invert the order
               The_Row := Result.Values'Last(1) - Row + Result.Values'First(1);
            else
               The_Row := Row;
            end if;
            for Col in Result.Values'Range(2) loop
               if Compression = BI_RGB then
                  Current_Short := Row_Buf (Col);
                  Result.Values (The_Row, Col).B := Binary.Byte(Current_Value.B) * 2**(8-5); -- convert 5 bit to 8 bit values
                  Result.Values (The_Row, Col).G := Binary.Byte(Current_Value.G) * 2**(8-5);
                  Result.Values (The_Row, Col).R := Binary.Byte(Current_Value.R) * 2**(8-5);
               elsif Compression = BI_BITFIELDS then
                  Result.Values (The_Row, Col).B := Mask_Value (Binary.Word(Row_Buf(Col)), Masks.Blue, Masks.Blue_Bits);
                  Result.Values (The_Row, Col).G := Mask_Value (Binary.Word(Row_Buf(Col)), Masks.Green, Masks.Green_Bits);
                  Result.Values (The_Row, Col).R := Mask_Value (Binary.Word(Row_Buf(Col)), Masks.Red, Masks.Red_Bits);
                  Result.Values (The_Row, Col).A := Mask_Value (Binary.Word(Row_Buf(Col)), Masks.Alpha, Masks.Alpha_Bits);
               else
                  raise Invalid_Format;
               end if;
            end loop;
            Row := Row - 1;
         end loop;
      end Read_Sixteen_Bit_Format;

      procedure Read_Thirtytwo_Bit_Format is
         Row_Size : Natural := Natural(Result.Width); -- Size in Words
         Row_Buf  : Binary.Word_String (1 .. Row_Size);
         Last     : Natural;
         Row,The_Row : Natural;

         Current_Word   : Binary.Word;
         Current_Values : Binary.Byte_String(1..4);
         for Current_Values'Address use Current_Word'Address;

      begin
         Ada.Streams.Stream_IO.Set_Index (File, Ada.Streams.Stream_IO.Count((File_Header.Offset) + 1));

         Row := Result.Values'Last(1);
         while Row >= Result.Values'First(1) loop
            Binary.IO.Read (File, Row_Buf, Last);
            if Last /= Row_Size * Binary.Word_Bytes then
               raise Invalid_Format;
            end if;
            if Reversed then -- invert the order
               The_Row := Result.Values'Last(1) - Row + Result.Values'First(1);
            else
               The_Row := Row;
            end if;
            for Col in Result.Values'Range(2) loop
               if Compression = BI_RGB then
                  Current_Word := Row_Buf (Col);
                  Result.Values (The_Row, Col).B := Current_Values (1);
                  Result.Values (The_Row, Col).G := Current_Values (2);
                  Result.Values (The_Row, Col).R := Current_Values (3);
               elsif Compression = BI_BITFIELDS then
                  Result.Values (The_Row, Col).B := Mask_Value (Row_Buf(Col), Masks.Blue, Masks.Blue_Bits);
                  Result.Values (The_Row, Col).G := Mask_Value (Row_Buf(Col), Masks.Green, Masks.Green_Bits);
                  Result.Values (The_Row, Col).R := Mask_Value (Row_Buf(Col), Masks.Red, Masks.Red_Bits);
                  Result.Values (The_Row, Col).A := Mask_Value (Row_Buf(Col), Masks.Alpha, Masks.Alpha_Bits);
               else
                  raise Invalid_Format;
               end if;
            end loop;
            Row := Row - 1;
         end loop;
      end Read_Thirtytwo_Bit_Format;

      procedure Read_Old_Header is
         The_Header : V1_Info_Header;
      begin
         V1_Info_Header'Read (Ada.Streams.Stream_IO.Stream(File), The_Header);
         Result.Width := Integer(The_Header.Width);
         Result.Height := Integer(The_Header.Height);
         BPP := The_Header.Bit_Count;
         if BPP = 16 or BPP = 32 then
            raise Invalid_Format;
         end if;
         Color_Count := 2**Integer(BPP); -- Always maximum Colors
      end Read_Old_Header;

      procedure Read_Header is
         The_Header : V3_Info_Header;
      begin
         V3_Info_Header'Read (Ada.Streams.Stream_IO.Stream(File), The_Header);
         Result.Width := Integer(The_Header.Width);
         Result.Height := Integer(The_Header.Height);
         BPP := The_Header.Bit_Count;
         Color_Count := Integer(The_Header.Colors_Used);
         if Color_Count = 0 then
            Color_Count := 2**Integer(BPP); -- 0 = maximum Colors
         end if;
         Compression := The_Header.Compression;

         if The_Version = V2 then
            declare
               V2_Header : V2_Info_Header_Extra;
            begin
               V2_Info_Header_Extra'Read (Ada.Streams.Stream_IO.Stream(File), V2_Header);
               -- TODO: process extra information provided by V2-Header
            end;
         elsif The_Version >= V4 then
            declare
               V4_Header : V4_Info_Header_Extra;
            begin
               V4_Info_Header_Extra'Read (Ada.Streams.Stream_IO.Stream(File), V4_Header);
               Masks.Red   := V4_Header.Red_Mask;
               Masks.Green := V4_Header.Green_Mask;
               Masks.Blue  := V4_Header.Blue_Mask;
               Masks.Alpha := V4_Header.Alpha_Mask;
               if The_Version = V5 then
                  declare
                     V5_Header : V5_Info_Header_Extra;
                  begin
                     V5_Info_Header_Extra'Read (Ada.Streams.Stream_IO.Stream(File), V5_Header);
                     -- TODO: process extra information provided by V5-Header
                  end;
               end if;
            end;
         end if;
      end Read_Header;

   begin

      Ada.Streams.Stream_IO.Set_Index (File, 1);

      -- Read the Bitmap File Header
      Bitmap_File_Header'Read (Ada.Streams.Stream_IO.Stream(File), File_Header);

      if File_Header.Magic(1) /= 16#42# or File_Header.Magic(2) /= 16#4D# then
         raise Invalid_Format;
      end if;

--      if Binary.Word(Ada.Streams.Stream_IO.Size(File)) /= File_Header.File_Size then
--         Ada.Text_IO.Put_Line("WARNING: File Size mismatch");
--      end if;

      Bitmap_Version'Read (Ada.Streams.Stream_IO.Stream(File), The_Version);

      if not The_Version'Valid then
         raise Invalid_Format;
      end if;

      case The_Version is
         when V1 => -- Version 1 has different datatypes for width/height
            Read_Old_Header;
         when V2|V3|V4|V5 =>
            Read_Header;
      end case;

      if Compression = BI_BITFIELDS then
         if The_Version = V3 then -- V3 has Bitmasks directly after header
            declare
               Mask_Values : Binary.Word_String(1..3);
               Last        : Natural;
            begin
               Binary.IO.Read(File, Mask_Values, Last);
               Masks.Red   := Mask_Values (1);
               Masks.Green := Mask_Values (2);
               Masks.Blue  := Mask_Values (3);
               Masks.Alpha := 0;
            end;
         end if;
         Masks.Red_Bits := 0;
         Masks.Green_Bits := 0;
         Masks.Blue_Bits := 0;
         Masks.Alpha_Bits := 0;
         for Bit in 0 .. Binary.Word_Bits-1 loop
            if (Masks.Red and 2 ** Bit) /= 0 then
               Masks.Red_Bits := Masks.Red_Bits + 1;
            end if;
            if (Masks.Green and 2 ** Bit) /= 0 then
               Masks.Green_Bits := Masks.Green_Bits + 1;
            end if;
            if (Masks.Blue and 2 ** Bit) /= 0 then
               Masks.Blue_Bits := Masks.Blue_Bits + 1;
            end if;
            if (Masks.Alpha and 2 ** Bit) /= 0 then
               Masks.Alpha_Bits := Masks.Alpha_Bits + 1;
            end if;
         end loop;
      end if;

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
         when 24 => Read_RGB_Format;
         when 16 => Read_Sixteen_Bit_Format;
         when 32 => Read_Thirtytwo_Bit_Format;
         when others => raise Invalid_Format;
      end case;
      -- read bitmap data
      -- uncompress data (if necessary)

      return Result;
   end From_File;

end Lumen.Image.BMP;

