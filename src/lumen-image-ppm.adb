
-- Lumen.Image.PPM -- Load and save netpbm's PPM image data
--
-- Chip Richards, NiEstu, Phoenix AZ, Spring 2010

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
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Streams.Stream_IO;

with Lumen.Binary;


package body Lumen.Image.PPM is

   ---------------------------------------------------------------------------

   function From_File (File : in Byte_IO.File_Type;   PPM_Format : in Character) return Descriptor is

      ------------------------------------------------------------------------

      subtype Bytes_Per_Pixel_Range is Integer range 1 .. 6;  -- actually 1/8th .. 6

      ------------------------------------------------------------------------

      Next   : Character;
      Result : Descriptor;

      ------------------------------------------------------------------------

      -- Return the next character from the input stream
      function Next_Char return Character is
      begin  -- Next_Char
         return Character'Val (Byte_IO.Read (File, 1) (1));
      end Next_Char;

      ------------------------------------------------------------------------

      -- Skip intervening whitespace and comments, and return the next character
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
      -- Latin_1 digits
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

      -- Read a PBM (portable bitmap) file
      procedure Read_PBM is
      begin  -- Read_PBM

         -- Read the data and unpack it into our internal format
         null;
      end Read_PBM;

      ------------------------------------------------------------------------

      -- Read a PGM (portable greymap) file
      procedure Read_PGM is

         Maxval   : Natural;
         BPP      : Bytes_Per_Pixel_Range;
         Resample : Boolean := False;

      begin  -- Read_PGM

         -- Read the maxval
         Maxval := Read_Num (Skip);
         if Maxval > 65535 then
            raise Invalid_Format;
         elsif Maxval > 255 then
            BPP := 2;
         else
            BPP := 1;
         end if;

         -- Read the data and unpack it into our internal format

      end Read_PGM;

      ------------------------------------------------------------------------

      -- Read a PPM (portable pixmap) file
      procedure Read_PPM is

         Maxval   : Natural;
         BPP      : Bytes_Per_Pixel_Range;
         Resample : Boolean := False;

      begin  -- Read_PPM

         -- Read the maxval
         Maxval := Read_Num (Skip);
         if Maxval > 65535 then
            raise Invalid_Format;
         elsif Maxval > 255 then
            BPP := 6;
            Resample := True;
         else
            BPP := 3;
         end if;

         -- Calculate the row size and create an environment in which that type exists
         declare

            use type Binary.Byte;

            Row_Size : Natural := Result.Width * BPP;
            Row_Buf  : Binary.Byte_String (0 .. Row_Size - 1);
            Last     : Natural;

         begin

            -- Read the data a row at a time and unpack it into our internal format
            for Row in Result.Values'Range (1) loop
               Byte_IO.Read (File, Row_Buf, Last);

               -- Do we need to resample wide values to our internal form?
               -- And should this really be a more complex calculation, taking
               -- gamma into account?
               if Resample then
                  for Col in Result.Values'Range (2) loop
                     -- FIXME: this is all wrong, need to fetch words, not
                     -- bytes!  Luckily, I've never once seen a PPM file with
                     -- maxval > 255.
                     Result.Values (Row, Col).R := Row_Buf ((Col * 3) + 0) / Binary.Byte'Last;
                     Result.Values (Row, Col).G := Row_Buf ((Col * 3) + 1) / Binary.Byte'Last;
                     Result.Values (Row, Col).B := Row_Buf ((Col * 3) + 2) / Binary.Byte'Last;
                     Result.Values (Row, Col).A := Binary.Byte'Last;  -- PPMs don't have alpha, so use max
                  end loop ;
               else
                  for Col in Result.Values'Range (2) loop
                     Result.Values (Row, Col).R := Row_Buf ((Col * BPP) + 0);
                     Result.Values (Row, Col).G := Row_Buf ((Col * BPP) + 1);
                     Result.Values (Row, Col).B := Row_Buf ((Col * BPP) + 2);
                     Result.Values (Row, Col).A := Binary.Byte'Last;  -- PPMs don't have alpha, so use max
                  end loop ;
               end if;

               -- Check for early EOF
               if Last < Row_Size - 1 then
                  for Col in (Last / BPP) + 1 .. Result.Values'Last (2) loop
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
         end;

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

