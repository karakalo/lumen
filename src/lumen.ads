
-- Lumen -- A simple graphical user interface library based on OpenGL
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

private with Ada.Calendar;
with System;

package Lumen is

   -- Values used to compute record rep clause values that are portable
   -- between 32- and 64-bit systems
   Is_32      : constant := Boolean'Pos (System.Word_Size = 32);
   Is_64      : constant := 1 - Is_32;
   Word_Bytes : constant := Integer'Size / System.Storage_Unit;
   Word_Bits  : constant := Integer'Size - 1;
   Long_Bytes : constant := Long_Integer'Size / System.Storage_Unit;
   Long_Bits  : constant := Long_Integer'Size - 1;

   ---------------------------------------------------------------------------

   type Window_Type is tagged private;
   type Window_Handle is access all Window_Type'Class;

   type Context_Type is tagged private;
   type Context_Handle is access all Context_Type'Class;

private
   -- A time that won't ever happen during the execution of a Lumen app
   Never : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (Year  => Ada.Calendar.Year_Number'First,
                                                               Month => Ada.Calendar.Month_Number'First,
                                                               Day   => Ada.Calendar.Day_Number'First);


   type Context_Type is tagged
      record
         null;
      end record;

   type Window_Type is tagged
      record
         Prior_Frame : Ada.Calendar.Time     := Never;
         SPF         : Duration              := 0.0;
         App_Frames  : Long_Integer          := 0;
         Last_Frames : Long_Integer          := 0;
         Looping     : Boolean               := True;
         App_Start   : Ada.Calendar.Time     := Never;
         Last_Start  : Ada.Calendar.Time     := Never;
         Height      : Natural;
         Width       : Natural;
      end record;

end Lumen;
