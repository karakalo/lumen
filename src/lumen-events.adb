
-- Lumen.Events -- Manage input events in Lumen windows
--
-- Chip Richards, NiEstu, Phoenix AZ, Spring 2010

-- This code is covered by the ISC License:
--
-- Copyright (c) 2010, NiEstu
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

with Lumen.Internal;
with Lumen.Window;

package body Lumen.Events is

   ---------------------------------------------------------------------------

   -- Returns the number of events that are waiting in the event queue.
   -- Useful for more complex event loops.
   function Pending (Win : Window.Handle) return Natural is

      function X_Pending (Display : in Internal.Display_Pointer) return Natural;
      pragma Import (C, X_Pending, "XPending");

   begin  -- Pending
      return X_Pending (Win.Display);
   end Pending;

   ---------------------------------------------------------------------------

   -- Retrieve the next input event from the queue and return it
   function Next_Event (Win : Window.Handle) return Event_Data is

      ------------------------------------------------------------------------

      -- X event type codes
      X_Error          : constant :=  0;
      X_Key_Press      : constant :=  2;
      X_Key_Release    : constant :=  3;
      X_Button_Press   : constant :=  4;
      X_Button_Release : constant :=  5;
      X_Motion_Notify  : constant :=  6;
      X_Enter_Notify   : constant :=  7;
      X_Leave_Notify   : constant :=  8;
      X_Focus_In       : constant :=  9;
      X_Focus_Out      : constant := 10;
      X_Expose         : constant := 12;
      X_Resize_Request : constant := 25;
      X_Client_Message : constant := 33;

      X_First_Event    : constant := X_Error;
      X_Last_Event     : constant := X_Client_Message;

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

      type X_Event_Code is new Integer range X_First_Event .. X_Last_Event;

      Bytes     : constant := Integer'Size / 8;
      Bits      : constant := Integer'Size - 1;
      Atom_Bits : constant := Internal.Atom'Size - 1;
      type X_Event_Data (X_Event_Type : X_Event_Code := X_Error) is record
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
            when X_Resize_Request =>
               Siz_Width  : Natural;
               Siz_Height : Natural;
            when X_Client_Message =>
               Msg_Value  : Internal.Atom;
            when others =>
               Pad        : Internal.Padding;
         end case;
      end record;
      for X_Event_Data use record
         X_Event_Type at  0 * Bytes range 0 .. Bits;

         Key_X        at 16 * Bytes range 0 .. Bits;
         Key_Y        at 17 * Bytes range 0 .. Bits;
         Key_Root_X   at 18 * Bytes range 0 .. Bits;
         Key_Root_Y   at 19 * Bytes range 0 .. Bits;
         Key_State    at 20 * Bytes range 0 .. Bits;
         Key_Code     at 21 * Bytes range 0 .. Bits;

         Btn_X        at 16 * Bytes range 0 .. Bits;
         Btn_Y        at 17 * Bytes range 0 .. Bits;
         Btn_Root_X   at 18 * Bytes range 0 .. Bits;
         Btn_Root_Y   at 19 * Bytes range 0 .. Bits;
         Btn_State    at 20 * Bytes range 0 .. Bits;
         Btn_Code     at 21 * Bytes range 0 .. Bits;

         Mov_X        at 16 * Bytes range 0 .. Bits;
         Mov_Y        at 17 * Bytes range 0 .. Bits;
         Mov_Root_X   at 18 * Bytes range 0 .. Bits;
         Mov_Root_Y   at 19 * Bytes range 0 .. Bits;
         Mov_State    at 20 * Bytes range 0 .. Bits;

         Xng_X        at 16 * Bytes range 0 .. Bits;
         Xng_Y        at 17 * Bytes range 0 .. Bits;
         Xng_Root_X   at 18 * Bytes range 0 .. Bits;
         Xng_Root_Y   at 19 * Bytes range 0 .. Bits;

         Xps_X        at 10 * Bytes range 0 .. Bits;
         Xps_Y        at 11 * Bytes range 0 .. Bits;
         Xps_Width    at 12 * Bytes range 0 .. Bits;
         Xps_Height   at 13 * Bytes range 0 .. Bits;

         Siz_Width    at 10 * Bytes range 0 .. Bits;
         Siz_Height   at 11 * Bytes range 0 .. Bits;

         Msg_Value    at 14 * Bytes range 0 .. Atom_Bits;
      end record;

      ------------------------------------------------------------------------

      -- Convert an X modifier mask into a Lumen modifier set
      function Modifier_Mask_To_Set (Mask : Modifier_Mask) return Modifier_Set is
      begin  -- Modifier_Mask_To_Set
         return (
                 Mod_Shift    => (Mask and Shift_Mask)    /= 0,
                 Mod_Lock     => (Mask and Lock_Mask)     /= 0,
                 Mod_Control  => (Mask and Control_Mask)  /= 0,
                 Mod_1        => (Mask and Mod_1_Mask)    /= 0,
                 Mod_2        => (Mask and Mod_2_Mask)    /= 0,
                 Mod_3        => (Mask and Mod_3_Mask)    /= 0,
                 Mod_4        => (Mask and Mod_4_Mask)    /= 0,
                 Mod_5        => (Mask and Mod_5_Mask)    /= 0,
                 Mod_Button_1 => (Mask and Button_1_Mask) /= 0,
                 Mod_Button_2 => (Mask and Button_2_Mask) /= 0,
                 Mod_Button_3 => (Mask and Button_3_Mask) /= 0,
                 Mod_Button_4 => (Mask and Button_4_Mask) /= 0,
                 Mod_Button_5 => (Mask and Button_5_Mask) /= 0
                );
      end Modifier_Mask_To_Set;

      ------------------------------------------------------------------------

      Result  : Event_Data;
      X_Event : X_Event_Data;

      ------------------------------------------------------------------------

   begin  -- Next_Event

      -- Get the event from the X server
      Internal.X_Next_Event (Win.Display, X_Event'Address);

      -- Based on the event type, transfer and convert the event data
      case X_Event.X_Event_Type is
         when X_Key_Press =>
            Result := (Which     => Key_Press,
                       Key_Data  => (X         => X_Event.Key_X,
                                     Y         => X_Event.Key_Y,
                                     Abs_X     => X_Event.Key_Root_X,
                                     Abs_Y     => X_Event.Key_Root_Y,
                                     Modifiers => Modifier_Mask_To_Set (X_Event.Key_State),
                                     Key_Code  => Raw_Keycode (X_Event.Key_Code)));

         when X_Key_Release =>
            Result := (Which     => Key_Release,
                       Key_Data  => (X         => X_Event.Key_X,
                                     Y         => X_Event.Key_Y,
                                     Abs_X     => X_Event.Key_Root_X,
                                     Abs_Y     => X_Event.Key_Root_Y,
                                     Modifiers => Modifier_Mask_To_Set (X_Event.Key_State),
                                     Key_Code  => Raw_Keycode (X_Event.Key_Code)));

         when X_Button_Press =>
            Result := (Which        => Button_Press,
                       Button_Data  => (X         => X_Event.Btn_X,
                                        Y         => X_Event.Btn_Y,
                                        Abs_X     => X_Event.Btn_Root_X,
                                        Abs_Y     => X_Event.Btn_Root_Y,
                                        Modifiers => Modifier_Mask_To_Set (X_Event.Btn_State),
                                        Changed   => Button'Val (X_Event.Btn_Code - 1)));

         when X_Button_Release =>
            Result := (Which        => Button_Release,
                       Button_Data  => (X         => X_Event.Btn_X,
                                        Y         => X_Event.Btn_Y,
                                        Abs_X     => X_Event.Btn_Root_X,
                                        Abs_Y     => X_Event.Btn_Root_Y,
                                        Modifiers => Modifier_Mask_To_Set (X_Event.Btn_State),
                                        Changed   => Button'Val (X_Event.Btn_Code - 1)));

         when X_Motion_Notify =>
            Result := (Which       => Pointer_Motion,
                       Motion_Data => (X         => X_Event.Mov_X,
                                       Y         => X_Event.Mov_Y,
                                       Abs_X     => X_Event.Mov_Root_X,
                                       Abs_Y     => X_Event.Mov_Root_Y,
                                       Modifiers => Modifier_Mask_To_Set (X_Event.Mov_State)));

         when X_Enter_Notify =>
            Result := (Which         => Enter_Window,
                       Crossing_Data => (X         => X_Event.Xng_X,
                                         Y         => X_Event.Xng_Y,
                                         Abs_X     => X_Event.Xng_Root_X,
                                         Abs_Y     => X_Event.Xng_Root_Y));

         when X_Leave_Notify =>
            Result := (Which     => Leave_Window,
                       Crossing_Data => (X         => X_Event.Xng_X,
                                         Y         => X_Event.Xng_Y,
                                         Abs_X     => X_Event.Xng_Root_X,
                                         Abs_Y     => X_Event.Xng_Root_Y));

         when X_Focus_In =>
            Result := (Which => Focus_In);

         when X_Focus_Out =>
            Result := (Which => Focus_Out);

         when X_Expose =>
            Result := (Which       => Exposed,
                       Expose_Data => (X         => X_Event.Xps_X,
                                       Y         => X_Event.Xps_Y,
                                       Width     => X_Event.Xps_Width,
                                       Height    => X_Event.Xps_Height));

         when X_Resize_Request =>
            Result := (Which       => Resized,
                       Resize_Data => (Width     => X_Event.Siz_Width,
                                       Height    => X_Event.Siz_Height));

         when X_Client_Message =>
            declare
               use type Internal.Atom;
            begin
               if X_Event.Msg_Value = Internal.Delete_Window_Atom then
                  Result := (Which => Close_Window);
               else
                  Result := (Which => No_Event);
               end if;
            end;

         when others =>
            Result := (Which => No_Event);
      end case;

      -- Pass back our massaged result
      return Result;
   end Next_Event;

   ---------------------------------------------------------------------------

   -- Simple event loop with a single callback
   procedure Receive_Events (Win  : in Window.Handle;
                             Call : in Event_Callback) is
   begin  -- Receive_Events

      -- Get events and pass them to the callback
      loop
         Call (Next_Event (Win));
      end loop;
   end Receive_Events;

   ---------------------------------------------------------------------------

   -- Simple event loop with multiple callbacks based on event type
   procedure Select_Events (Win   : in Window.Handle;
                            Calls : in Event_Callback_Table) is

      Event : Event_Data := Next_Event (Win);

   begin  -- Select_Events

      -- Get events and pass them to the selected callback
      loop
         if Calls (Event.Which) /= No_Callback then
            Calls (Event.Which) (Event);
         end if;
      end loop;
   end Select_Events;

   ---------------------------------------------------------------------------

end Lumen.Events;
