
-- Lumen.Image.PPM -- Load and save netpbm's PPM image data
--
-- Chip Richards, NiEstu, Phoenix AZ, Spring 2010

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
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Streams.Stream_IO;

with Lumen.Binary.Endian.Shorts;

package body Lumen.Image.PPM is

   ---------------------------------------------------------------------------

   function From_File (File : in Binary.IO.File_Type;   PPM_Format : in Character) return Descriptor is

      ------------------------------------------------------------------------

      subtype Bytes_Per_Pixel_Range is Integer range 1 .. 6;  -- actually 1/8th .. 6

      ------------------------------------------------------------------------

      Next   : Character;
      Result : Descriptor;

      ------------------------------------------------------------------------

      -- Return the next character from the input stream; used only when
      -- reading the header/metadata
      function Next_Char return Character is

         BS : Binary.Byte_String := Binary.IO.Read (File, 1);

      begin  -- Next_Char
         return Character'Val (BS (1));
      end Next_Char;

      ------------------------------------------------------------------------

      -- Skip intervening whitespace and comments, and return the next
      -- character; used only when reading the header/metadata
      function Skip return Character is

         C : Character;

      begin  -- Skip

         loop
            C := Next_Char;

            if C = '#' then
               while C /= Ada.Characters.Latin_1.LF loop  -- works for DOS-style lines, too
                  C := Next_Char;
               end loop ;
            elsif Ada.Characters.Handling.Is_Decimal_Digit (C) then  -- only thing we're looking for
               return C ;
            end if ;
         end loop ;
      end Skip ;

      ------------------------------------------------------------------------

      -- Read a nonnegative decimal integer from the stream, represented as
      -- Latin_1 digits; used when reading the header/metadata in all formats
      -- and the RGB values in the ASCII version of the format.
      function Read_Num (First : in Character) return Natural is

         Number : Natural := 0;
         C      : Character;

      begin  -- Read_Num
         C := First;

         loop
            Number := Number * 10 + (Character'Pos (C) - Character'Pos ('0'));
            C := Next_Char;
            if not Ada.Characters.Handling.Is_Decimal_Digit (C) then
               return Number;  -- always discards trailing non-digit
            end if ;
         end loop;

      end Read_Num;

      ------------------------------------------------------------------------
      -- Read a PBM ASCII (portable bitmap) file
      procedure Read_APBM is
         Col      : Natural;
         Pix      : Pixel;
         Value    : Natural;
      begin -- Read_APBM
         -- Read the data a row at a time and unpack it into our internal format
         for Row in Result.Values'Range (1) loop
            Col := Result.Values'First (2);
            for Pixel in 1 .. Result.Width loop
               Next := Skip;
               Value := Read_Num (Next);

               if Value = 1 then
                  Pix := Black_Pixel;
               else
                  Pix := White_Pixel;
               end if;
               Result.Values (Row, Col) := Pix;
               Col := Col + 1;
            end loop;
         end loop;
      end Read_APBM;
      ------------------------------------------------------------------------
      -- Read a PGM ASCII (portable greymap) file
      procedure Read_APGM is
         Maxval : Natural;
         Col    : Natural;
         Value  : Natural;
      begin -- Read_APGM
         -- Read the maxval
         Maxval := Read_Num (Skip);
         -- Read the data a row at a time and unpack it into our internal format
         for Row in Result.Values'Range (1) loop
            Col := Result.Values'First (2);
            for Pixel in 1 .. Result.Width loop
               Next := Skip;
               Value := Read_Num (Next);

               Result.Values (Row, Col).R := Binary.Byte (Value);
               Result.Values (Row, Col).G := Binary.Byte (Value);
               Result.Values (Row, Col).B := Binary.Byte (Value);
               Result.Values (Row, Col).A := Binary.Byte'Last;  -- PGMs don't have alpha, so use max
               Col := Col + 1;
            end loop;
         end loop;
      end Read_APGM;
      ------------------------------------------------------------------------
      -- Read a PPM ASCII (portable pixmap) file
      procedure Read_APPM is
         Maxval : Natural;
         Col    : Natural;
         function Read_Color return Binary.Byte
         is
         begin
            return Binary.Byte (Read_Num (Skip));
         end Read_Color;
      begin -- Read_APPM
         -- Read the maxval
         Maxval := Read_Num (Skip);
         -- Read the data a row at a time and unpack it into our internal format
         for Row in Result.Values'Range (1) loop
            Col := Result.Values'First (2);
            for Pixel in 1 .. Result.Width loop
               Result.Values (Row, Col).R := Read_Color;
               Result.Values (Row, Col).G := Read_Color;
               Result.Values (Row, Col).B := Read_Color;
               Result.Values (Row, Col).A := Binary.Byte'Last;  -- PPMs don't have alpha, so use max
               Col := Col + 1;
            end loop;
         end loop;
      end Read_APPM;
      ------------------------------------------------------------------------
      -- Read a PBM (portable bitmap) file
      procedure Read_PBM is

         use Binary;

         Row_Size : Natural := (Result.Width + Byte_LB) / Byte_Bits;
         Row_Buf  : Byte_String (0 .. Row_Size - 1);
         Last     : Natural;
         Col      : Natural;
         Pix      : Pixel;

      begin  -- Read_PBM

         -- Read the data a row at a time and unpack it into our internal format
         for Row in Result.Values'Range (1) loop
            Binary.IO.Read (File, Row_Buf, Last);

            -- Move the image data into our output buffer a pixel at a time,
            -- expanding bitmap data to RGB and adding an alpha value to each
            Col := Result.Values'First (2);
            Buffer : for Row_Byte in Row_Buf'Range loop
               for Bits in 1 .. Byte_Bits loop

                  -- Test top bit; 1 = black, 0 = white (yeah, that's right)
                  if Row_Buf (Row_Byte) > Byte'Last / 2 then
                     Pix := Black_Pixel;
                  else
                     Pix := White_Pixel;
                  end if;

                  Row_Buf (Row_Byte) := Row_Buf (Row_Byte) * 2;
                  Result.Values (Row, Col) := Pix;
                  Col := Col + 1;

                  exit Buffer when Col > Result.Values'Last (2);  -- rest of buffer is don't-care bits

               end loop;
            end loop Buffer;

            -- Check for early EOF
            if Last < Row_Size - 1 then
               Col := (Last + 1) * Byte_Bits;
               for Fill in Col .. Result.Values'Last (2) loop
                  Result.Values (Row, Col) := Transparent_Pixel;  -- disappear missing columns in current row
               end loop;

               -- Disappear missing rows, if any
               if Row < Result.Height - 1 then
                  for Fill in Row + 1 .. Result.Values'Last (1) loop
                     for Col in Result.Values'Range (2) loop
                        Result.Values (Fill, Col) := Transparent_Pixel;
                     end loop;
                  end loop;
               end if;

               -- Signal truncated image, if the caller cares
               Result.Complete := False;

               exit;  -- EOF ends the input process
            end if;

         end loop ;

      end Read_PBM;

      ------------------------------------------------------------------------

      -- Read a PGM (portable greymap) file
      procedure Read_PGM is

         Maxval : Natural;

      begin  -- Read_PGM

         -- Read the maxval
         Maxval := Read_Num (Skip);
         if Maxval > 65535 then
            raise Invalid_Format;
         elsif Maxval > 255 then

            -- Wide greymap pixels, 2 bytes each.  Calculate the row size and
            -- create an environment in which that type exists.
            declare

               use Binary;

               package BS is new Endian.Shorts (Short);

               Mult : constant Short := Short'Last / Short (Maxval);  -- multiplier to maximize pixel value retention
               Div  : constant := Short (Byte'Last) + 1;              -- divisor to convert shorts to bytes

               Row_Buf : Short_String (0 .. Result.Width - 1);
               Last    : Natural;
               Grey    : Byte;

            begin  -- block to read 16-bit PGM data

               -- Read the data a row at a time and unpack it into our internal format
               for Row in Result.Values'Range (1) loop
                  IO.Read (File, Row_Buf, Last);

                  -- Move the image data into our output buffer a pixel at a
                  -- time, trying to retain as much grey data as possible,
                  -- expanding greymap data to RGB and adding an alpha value
                  -- to each
                  for Col in Result.Values'Range (2) loop
                     Grey := Byte ((BS.From_Big (Row_Buf (Col)) * Mult) / Div);
                     Result.Values (Row, Col).R := Grey;
                     Result.Values (Row, Col).G := Grey;
                     Result.Values (Row, Col).B := Grey;
                     Result.Values (Row, Col).A := Byte'Last;  -- PGMs don't have alpha, so use max
                  end loop ;

                  -- Check for early EOF
                  if Last < Result.Width - 1 then
                     for Col in Last + 1 .. Result.Values'Last (2) loop
                        Result.Values (Row, Col) := Transparent_Pixel;  -- disappear missing columns in current row
                     end loop;

                     -- Disappear missing rows, if any
                     if Row < Result.Height - 1 then
                        for Fill in Row + 1 .. Result.Values'Last (1) loop
                           for Col in Result.Values'Range (2) loop
                              Result.Values (Fill, Col) := Transparent_Pixel;
                           end loop;
                        end loop;
                     end if;

                     -- Signal truncated image, if the caller cares
                     Result.Complete := False;

                     exit;  -- EOF ends the input process
                  end if;

               end loop;

            end;

         else

            -- Regular greymap pixels, 1 byte each.  Create the byte string to
            -- use as an input buffer.
            declare

               use type Binary.Byte;

               Row_Buf : Binary.Byte_String (0 .. Result.Width - 1);
               Last    : Natural;
               Grey    : Binary.Byte;

            begin  -- block to read 8-bit PGM data

               -- Read the data a row at a time and unpack it into our internal format
               for Row in Result.Values'Range (1) loop
                  Binary.IO.Read (File, Row_Buf, Last);

                  -- Move the image data into our output buffer a pixel at a
                  -- time, expanding greymap data to RGB and adding an alpha
                  -- value to each
                  for Col in Result.Values'Range (2) loop
                     Grey := Row_Buf (Col);
                     Result.Values (Row, Col).R := Grey;
                     Result.Values (Row, Col).G := Grey;
                     Result.Values (Row, Col).B := Grey;
                     Result.Values (Row, Col).A := Binary.Byte'Last;  -- PGMs don't have alpha, so use max
                  end loop ;

                  -- Check for early EOF
                  if Last < Result.Width - 1 then
                     for Col in Last + 1 .. Result.Values'Last (2) loop
                        Result.Values (Row, Col) := Transparent_Pixel;  -- disappear missing columns in current row
                     end loop;

                     -- Disappear missing rows, if any
                     if Row < Result.Height - 1 then
                        for Fill in Row + 1 .. Result.Values'Last (1) loop
                           for Col in Result.Values'Range (2) loop
                              Result.Values (Fill, Col) := Transparent_Pixel;
                           end loop;
                        end loop;
                     end if;

                     -- Signal truncated image, if the caller cares
                     Result.Complete := False;

                     exit;  -- EOF ends the input process
                  end if;

               end loop;

            end;

         end if;

      end Read_PGM;

      ------------------------------------------------------------------------

      -- Read a PPM (portable pixmap) file
      procedure Read_PPM is

         Maxval : Natural;

      begin  -- Read_PPM

         -- Read the maxval
         Maxval := Read_Num (Skip);
         if Maxval > 65535 then
            raise Invalid_Format;
         elsif Maxval > 255 then

            -- Wide pixels, 2 bytes per color value.  Calculate the row size
            -- and create an environment in which that type exists.
            declare

               use Binary;

               package BS is new Endian.Shorts (Short);

               Mult : constant Short := Short'Last / Short (Maxval);  -- multiplier to maximize pixel value retention
               Div  : constant := Short (Byte'Last) + 1;              -- divisor to convert shorts to bytes

               Row_Size : Natural := Result.Width * 3;  -- 3 color values per pixel
               Row_Buf  : Short_String (0 .. Row_Size - 1);
               Last     : Natural;

            begin  -- block to read 16-bit PPM data

               -- Read the data a row at a time and unpack it into our internal format
               for Row in Result.Values'Range (1) loop
                  IO.Read (File, Row_Buf, Last);

                  -- Move the image data into our output buffer a pixel at a
                  -- time, trying to retain as much color data as possible,
                  -- and adding an alpha value to each
                  for Col in Result.Values'Range (2) loop
                     Result.Values (Row, Col).R := Byte ((BS.From_Big (Row_Buf ((Col * 3) + 0)) * Mult) / Div);
                     Result.Values (Row, Col).G := Byte ((BS.From_Big (Row_Buf ((Col * 3) + 1)) * Mult) / Div);
                     Result.Values (Row, Col).B := Byte ((BS.From_Big (Row_Buf ((Col * 3) + 2)) * Mult) / Div);
                     Result.Values (Row, Col).A := Byte'Last;  -- PPMs don't have alpha, so use max
                  end loop ;

                  -- Check for early EOF
                  if Last < Row_Size - 1 then
                     for Col in (Last / 3) + 1 .. Result.Values'Last (2) loop
                        Result.Values (Row, Col) := Transparent_Pixel;  -- disappear missing columns in current row
                     end loop;

                     -- Disappear missing rows, if any
                     if Row < Result.Height - 1 then
                        for Fill in Row + 1 .. Result.Values'Last (1) loop
                           for Col in Result.Values'Range (2) loop
                              Result.Values (Fill, Col) := Transparent_Pixel;
                           end loop;
                        end loop;
                     end if;

                     -- Signal truncated image, if the caller cares
                     Result.Complete := False;

                     exit;  -- EOF ends the input process
                  end if;

               end loop;
            end;

         else

            -- Regular pixels, 1 byte per color value.  Calculate the row size
            -- and create an environment in which that type exists.
            declare

               use type Binary.Byte;

               Row_Size : Natural := Result.Width * 3;  -- 3 color values per pixel
               Row_Buf  : Binary.Byte_String (0 .. Row_Size - 1);
               Last     : Natural;

            begin  -- block to read 8-bit PPM data

               -- Read the data a row at a time and unpack it into our internal format
               for Row in Result.Values'Range (1) loop
                  Binary.IO.Read (File, Row_Buf, Last);

                  -- Move the image data into our output buffer a pixel at a
                  -- time, adding an alpha value to each
                  for Col in Result.Values'Range (2) loop
                     Result.Values (Row, Col).R := Row_Buf ((Col * 3) + 0);
                     Result.Values (Row, Col).G := Row_Buf ((Col * 3) + 1);
                     Result.Values (Row, Col).B := Row_Buf ((Col * 3) + 2);
                     Result.Values (Row, Col).A := Binary.Byte'Last;  -- PPMs don't have alpha, so use max
                  end loop ;

                  -- Check for early EOF
                  if Last < Row_Size - 1 then
                     for Col in (Last / 3) + 1 .. Result.Values'Last (2) loop
                        Result.Values (Row, Col) := Transparent_Pixel;  -- disappear missing columns in current row
                     end loop;

                     -- Disappear missing rows, if any
                     if Row < Result.Height - 1 then
                        for Fill in Row + 1 .. Result.Values'Last (1) loop
                           for Col in Result.Values'Range (2) loop
                              Result.Values (Fill, Col) := Transparent_Pixel;
                           end loop;
                        end loop;
                     end if;

                     -- Signal truncated image, if the caller cares
                     Result.Complete := False;

                     exit;  -- EOF ends the input process
                  end if;

               end loop;
            end;

         end if;

      end Read_PPM;

      ------------------------------------------------------------------------

   begin  -- From_File

      -- Reposition to actual start of metadata, after signature check
      Ada.Streams.Stream_IO.Set_Index (File, 3);

      -- Get image dimensions and allocate the image buffer
      Next := Skip;
      Result.Width  := Read_Num (Next);
      Next := Skip;
      Result.Height := Read_Num (Next);
      Result.Values := new Pixel_Matrix (0 .. Result.Height - 1, 0 .. Result.Width - 1);
      Result.Complete := True;  -- assume complete image unless we hit early EOF

      -- Based on format, read the rest of the data
      case PPM_Format is
         when '1' =>
            Read_APBM;
            return Result;

         when '2' =>
            Read_APGM;
            return Result;

         when '3' =>
            Read_APPM;
            return Result;

         when '4' =>
            Read_PBM;
            return Result;

         when '5' =>
            Read_PGM;
            return Result;

         when '6' =>
            Read_PPM;
            return Result;

         when others =>
            raise Program_Error;  -- shouldn't happen

      end case;
   end From_File;

   ---------------------------------------------------------------------------

end Lumen.Image.PPM;
