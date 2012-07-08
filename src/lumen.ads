
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

   type Button_Enum is (Button_1, Button_2, Button_3, Button_4, Button_5);

   -- OpenGL context ("visual") attribute specifiers
   type Context_Attribute_Name is
      (
       Attr_None,
       Attr_Use_GL,             -- unused
       Attr_Buffer_Size,        -- color index buffer size, ignored if TrueColor
       Attr_Level,              -- buffer level for over/underlays
       Attr_RGBA,               -- set by Depth => TrueColor
       Attr_Doublebuffer,       -- set by Animate => True
       Attr_Stereo,             -- wow, you have stereo visuals?
       Attr_Aux_Buffers,        -- number of auxiliary buffers
       Attr_Red_Size,           -- bit depth, red
       Attr_Green_Size,         -- bit depth, green
       Attr_Blue_Size,          -- bit depth, blue
       Attr_Alpha_Size,         -- bit depth, alpha
       Attr_Depth_Size,         -- depth buffer size
       Attr_Stencil_Size,       -- stencil buffer size
       Attr_Accum_Red_Size,     -- accumulation buffer bit depth, red
       Attr_Accum_Green_Size,   -- accumulation buffer bit depth, green
       Attr_Accum_Blue_Size,    -- accumulation buffer bit depth, blue
       Attr_Accum_Alpha_Size    -- accumulation buffer bit depth, alpha
      );

   type Context_Attribute (Name  : Context_Attribute_Name := Attr_None) is record
      case Name is
         when Attr_None | Attr_Use_GL | Attr_RGBA | Attr_Doublebuffer | Attr_Stereo =>
            null;  -- present or not, no value
         when Attr_Level =>
            Level : Integer := 0;
         when Attr_Buffer_Size | Attr_Aux_Buffers | Attr_Depth_Size | Attr_Stencil_Size |
              Attr_Red_Size | Attr_Green_Size | Attr_Blue_Size | Attr_Alpha_Size |
              Attr_Accum_Red_Size | Attr_Accum_Green_Size | Attr_Accum_Blue_Size | Attr_Accum_Alpha_Size =>
            Size : Natural := 0;
      end case;
   end record;

   type Context_Attributes is array (Positive range <>) of Context_Attribute;

   type Event_MouseDown is
     access procedure
       (X      : Integer;
        Y      : Integer;
        Button : Button_Enum);

   type Event_MouseUp is
     access procedure
       (X      : Integer;
        Y      : Integer;
        Button : Button_Enum);

   type Event_MouseMove is
     access procedure
       (X      : Integer;
        Y      : Integer);

   type Event_KeyPress is
     access procedure
       (Key : Integer);

   type Event_KeyRelease is
     access procedure
       (Key : Integer);

   type Event_Character is
     access procedure
       (Char : String);

   type Window_Public is tagged
      record
         OnMouseDown  : Event_MouseDown  := null;
         OnMouseUp    : Event_MouseUp    := null;
         OnMouseMove  : Event_MouseMove  := null;
         OnKeyPress   : Event_KeyPress   := null;
         OnKeyRelease : Event_KeyRelease := null;
         OnCharacter  : Event_Character  := null;
      end record;

   type Window_Type is new Window_Public with private;
   type Window_Handle is access all Window_Type'Class;

private
   -- A time that won't ever happen during the execution of a Lumen app
   Never : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (Year  => Ada.Calendar.Year_Number'First,
                                                               Month => Ada.Calendar.Month_Number'First,
                                                               Day   => Ada.Calendar.Day_Number'First);

   type Window_Type is new Window_Public with
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
