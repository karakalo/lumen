
-- Lumen.Internal -- Internal declarations not intended for user applications
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
with System;
with Ada.Calendar;

with PQueue;

package Lumen.Internal is

   ---------------------------------------------------------------------------

   -- Xlib stuff needed for our window info record
   type Atom            is new Long_Integer;
   type Display_Pointer is new System.Address;
   Null_Display_Pointer : constant Display_Pointer := Display_Pointer (System.Null_Address);
   type Screen_Depth    is new Natural;
   type Screen_Number   is new Natural;
   type Visual_ID       is new Long_Integer;
   type Window_ID       is new Long_Integer;
   type X_Event_Mask    is mod 2 ** Long_Integer'Size;

   type X_Visual_Info is record
      Visual        : System.Address;
      Visual_Ident  : Visual_ID;
      Screen        : Screen_Number;
      Depth         : Screen_Depth;
      Class         : Integer;
      Red_Mask      : Long_Integer;
      Green_Mask    : Long_Integer;
      Blue_Mask     : Long_Integer;
      Colormap_Size : Natural;
      Bits_Per_RGB  : Natural;
   end record;
   type X_Visual_Info_Pointer is access all X_Visual_Info;

   ---------------------------------------------------------------------------

   -- The GL rendering context type
   type GLX_Context is new System.Address;
   Null_Context : constant GLX_Context := GLX_Context (System.Null_Address);

   ---------------------------------------------------------------------------

   -- A time that won't ever happen during the execution of a Lumen app
   Never : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (Year  => Ada.Calendar.Year_Number'First,
                                                               Month => Ada.Calendar.Month_Number'First,
                                                               Day   => Ada.Calendar.Day_Number'First);

   ---------------------------------------------------------------------------

   -- Our "delete window" atom value
   Delete_Window_Atom : Atom;

   ---------------------------------------------------------------------------

   -- Binding to XNextEvent, used by Window for mapping notify events, and by
   -- Events for everything else
   procedure X_Next_Event (Display : in Display_Pointer;
                           Event   : in System.Address);
   pragma Import (C, X_Next_Event, "XNextEvent");

   ---------------------------------------------------------------------------

   -- Values used to compute record rep clause values that are portable
   -- between 32- and 64-bit systems
   Is_32      : constant := Boolean'Pos (System.Word_Size = 32);
   Is_64      : constant := 1 - Is_32;
   Word_Bytes : constant := Integer'Size / System.Storage_Unit;
   Word_Bits  : constant := Integer'Size - 1;
   Long_Bytes : constant := Long_Integer'Size / System.Storage_Unit;
   Long_Bits  : constant := Long_Integer'Size - 1;

   ---------------------------------------------------------------------------

   -- The maximum length of an event data record
   type Padding is array (1 .. 23) of Long_Integer;

   ---------------------------------------------------------------------------

   -- The X input event record

   -- X event type codes
   X_Error            : constant :=  0;  -- we don't actually use this, just there to define bounds
   X_Key_Press        : constant :=  2;
   X_Key_Release      : constant :=  3;
   X_Button_Press     : constant :=  4;
   X_Button_Release   : constant :=  5;
   X_Motion_Notify    : constant :=  6;
   X_Enter_Notify     : constant :=  7;
   X_Leave_Notify     : constant :=  8;
   X_Focus_In         : constant :=  9;
   X_Focus_Out        : constant := 10;
   X_Expose           : constant := 12;
   X_Unmap_Notify     : constant := 18;
   X_Map_Notify       : constant := 19;
   X_Configure_Notify : constant := 22;
   X_Client_Message   : constant := 33;
   X_Generic_Event    : constant := 35;  -- we don't actually use this, just there to define bounds

   X_First_Event    : constant := X_Error;
   X_Last_Event     : constant := X_Generic_Event + 1;

   -- X modifier mask and its values
   type Modifier_Mask is mod 2 ** Integer'Size;

   Shift_Mask    : constant Modifier_Mask := 2#0000_0000_0000_0001#;
   Lock_Mask     : constant Modifier_Mask := 2#0000_0000_0000_0010#;
   Control_Mask  : constant Modifier_Mask := 2#0000_0000_0000_0100#;
   Mod_1_Mask    : constant Modifier_Mask := 2#0000_0000_0000_1000#;
   Mod_2_Mask    : constant Modifier_Mask := 2#0000_0000_0001_0000#;
   Mod_3_Mask    : constant Modifier_Mask := 2#0000_0000_0010_0000#;
   Mod_4_Mask    : constant Modifier_Mask := 2#0000_0000_0100_0000#;
   Mod_5_Mask    : constant Modifier_Mask := 2#0000_0000_1000_0000#;
   Button_1_Mask : constant Modifier_Mask := 2#0000_0000_0000_0000#;
   Button_2_Mask : constant Modifier_Mask := 2#0000_0001_0000_0000#;
   Button_3_Mask : constant Modifier_Mask := 2#0000_0010_0000_0000#;
   Button_4_Mask : constant Modifier_Mask := 2#0000_0100_0000_0000#;
   Button_5_Mask : constant Modifier_Mask := 2#0001_1000_0000_0000#;

   -- Actual type of input event
   type X_Event_Code is new Integer range X_First_Event .. X_Last_Event;

   -- The event record and its layout
   Bytes     : constant := Word_Bytes;
   Bits      : constant := Word_Bits;
   Atom_Bits : constant := Atom'Size - 1;
   Base_1_32 : constant :=  8;
   Base_2_32 : constant :=  5;
   Base_3_32 : constant :=  6;
   Base_4_32 : constant :=  7;
   Base_5_32 : constant :=  4;
   Base_1_64 : constant := 16;
   Base_2_64 : constant := 10;
   Base_3_64 : constant := 12;
   Base_4_64 : constant := 14;
   Base_5_64 : constant :=  8;
   Base_1    : constant := (Base_1_32 * Is_32) + (Base_1_64 * Is_64);
   Base_2    : constant := (Base_2_32 * Is_32) + (Base_2_64 * Is_64);
   Base_3    : constant := (Base_3_32 * Is_32) + (Base_3_64 * Is_64);
   Base_4    : constant := (Base_4_32 * Is_32) + (Base_4_64 * Is_64);
   Base_5    : constant := (Base_5_32 * Is_32) + (Base_5_64 * Is_64);
   type X_Event_Data (X_Event_Type : X_Event_Code := X_Error) is record
      Window : Window_ID;
      case X_Event_Type is
         when X_Key_Press | X_Key_Release =>
            Key_X      : Natural;
            Key_Y      : Natural;
            Key_Root_X : Natural;
            Key_Root_Y : Natural;
            Key_State  : Modifier_Mask;
            Key_Code   : Natural;
         when X_Button_Press | X_Button_Release =>
            Btn_X      : Natural;
            Btn_Y      : Natural;
            Btn_Root_X : Natural;
            Btn_Root_Y : Natural;
            Btn_State  : Modifier_Mask;
            Btn_Code   : Natural;
         when X_Motion_Notify =>
            Mov_X      : Natural;
            Mov_Y      : Natural;
            Mov_Root_X : Natural;
            Mov_Root_Y : Natural;
            Mov_State  : Modifier_Mask;
         when X_Enter_Notify | X_Leave_Notify =>
            Xng_X      : Natural;
            Xng_Y      : Natural;
            Xng_Root_X : Natural;
            Xng_Root_Y : Natural;
         when X_Expose =>
            Xps_X      : Natural;
            Xps_Y      : Natural;
            Xps_Width  : Natural;
            Xps_Height : Natural;
            Xps_Count  : Natural;
         when X_Configure_Notify =>
            Cfg_X      : Natural;
            Cfg_Y      : Natural;
            Cfg_Width  : Natural;
            Cfg_Height : Natural;
         when X_Client_Message =>
            Msg_Value  : Atom;
         when others =>
            Pad        : Padding;
      end case;
   end record;
   for X_Event_Data use record
      X_Event_Type at  0 * Bytes range 0 .. Bits;

      Window       at (Base_5 + 0) * Bytes range 0 .. Long_Bits;

      Key_X        at (Base_1 + 0) * Bytes range 0 .. Bits;
      Key_Y        at (Base_1 + 1) * Bytes range 0 .. Bits;
      Key_Root_X   at (Base_1 + 2) * Bytes range 0 .. Bits;
      Key_Root_Y   at (Base_1 + 3) * Bytes range 0 .. Bits;
      Key_State    at (Base_1 + 4) * Bytes range 0 .. Bits;
      Key_Code     at (Base_1 + 5) * Bytes range 0 .. Bits;

      Btn_X        at (Base_1 + 0) * Bytes range 0 .. Bits;
      Btn_Y        at (Base_1 + 1) * Bytes range 0 .. Bits;
      Btn_Root_X   at (Base_1 + 2) * Bytes range 0 .. Bits;
      Btn_Root_Y   at (Base_1 + 3) * Bytes range 0 .. Bits;
      Btn_State    at (Base_1 + 4) * Bytes range 0 .. Bits;
      Btn_Code     at (Base_1 + 5) * Bytes range 0 .. Bits;

      Mov_X        at (Base_1 + 0) * Bytes range 0 .. Bits;
      Mov_Y        at (Base_1 + 1) * Bytes range 0 .. Bits;
      Mov_Root_X   at (Base_1 + 2) * Bytes range 0 .. Bits;
      Mov_Root_Y   at (Base_1 + 3) * Bytes range 0 .. Bits;
      Mov_State    at (Base_1 + 4) * Bytes range 0 .. Bits;

      Xng_X        at (Base_1 + 0) * Bytes range 0 .. Bits;
      Xng_Y        at (Base_1 + 1) * Bytes range 0 .. Bits;
      Xng_Root_X   at (Base_1 + 2) * Bytes range 0 .. Bits;
      Xng_Root_Y   at (Base_1 + 3) * Bytes range 0 .. Bits;

      Xps_X        at (Base_2 + 0) * Bytes range 0 .. Bits;
      Xps_Y        at (Base_2 + 1) * Bytes range 0 .. Bits;
      Xps_Width    at (Base_2 + 2) * Bytes range 0 .. Bits;
      Xps_Height   at (Base_2 + 3) * Bytes range 0 .. Bits;

      Cfg_X        at (Base_3 + 0) * Bytes range 0 .. Bits;
      Cfg_Y        at (Base_3 + 1) * Bytes range 0 .. Bits;
      Cfg_Width    at (Base_3 + 2) * Bytes range 0 .. Bits;
      Cfg_Height   at (Base_3 + 3) * Bytes range 0 .. Bits;

      Msg_Value    at (Base_4 + 0) * Bytes range 0 .. Atom_Bits;
   end record;

   ---------------------------------------------------------------------------

   -- Temporary joystick queue event type
   type Joystick_Event_Data is null record;

   ---------------------------------------------------------------------------

   -- Combined event type, instances of which are placed in the window's event
   -- queue
   type Queue_Event_Type is (X_Input_Event, Joystick_Event);
   type Queue_Event_Data (Which : Queue_Event_Type := X_Input_Event) is record
      case Which is
         when X_Input_Event =>
            X : X_Event_Data;
         when Joystick_Event =>
            Joystick : Joystick_Event_Data;
      end case;
   end record;

   ---------------------------------------------------------------------------

   -- Instantiate the protected-queue package with the queue-event type
   package Event_Queue_Pkg is new PQueue (Queue_Event_Data);
   type Event_Queue_Pointer is access Event_Queue_Pkg.Protected_Queue_Type;

   ---------------------------------------------------------------------------

   -- The native window type
   type Window_Info is record
      Display     : Display_Pointer       := Null_Display_Pointer;
      Window      : Window_ID             := 0;
      Visual      : X_Visual_Info_Pointer := null;
      Width       : Natural               := 0;
      Height      : Natural               := 0;
      Prior_Frame : Ada.Calendar.Time     := Never;
      App_Start   : Ada.Calendar.Time     := Never;
      Last_Start  : Ada.Calendar.Time     := Never;
      App_Frames  : Long_Integer          := 0;
      Last_Frames : Long_Integer          := 0;
      Context     : GLX_Context           := Null_Context;
      Events      : Event_Queue_Pointer   := null;
   end record;

   ---------------------------------------------------------------------------

end Lumen.Internal;
