
-- Lumen.Events -- Manage input events in Lumen windows
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

with Lumen.Internal;
with Lumen.Window;

package body Lumen.Events is

   ---------------------------------------------------------------------------

   -- Returns the number of events that are waiting in the event queue.
   -- Useful for more complex event loops.
   function Pending (Win : Window.Handle) return Natural is

      function X_Pending (Display : Internal.Display_Pointer) return Natural;
      pragma Import (C, X_Pending, "XPending");

   begin  -- Pending
      return X_Pending (Win.Display);
   end Pending;

   ---------------------------------------------------------------------------

   -- Retrieve the next input event from the queue and return it
   function Next_Event (Win : Window.Handle) return Event_Data is

      ------------------------------------------------------------------------

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
      X_Map_Notify       : constant := 18;
      X_Unmap_Notify     : constant := 19;
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

      type X_Event_Code is new Integer range X_First_Event .. X_Last_Event;

      Bytes     : constant := Internal.Word_Bytes;
      Bits      : constant := Internal.Word_Bits;
      Atom_Bits : constant := Internal.Atom'Size - 1;
      Base_1_32 : constant :=  8;
      Base_2_32 : constant :=  5;
      Base_3_32 : constant :=  6;
      Base_4_32 : constant :=  7;
      Base_1_64 : constant := 16;
      Base_2_64 : constant := 10;
      Base_3_64 : constant := 12;
      Base_4_64 : constant := 14;
      Base_1    : constant := (Base_1_32 * Internal.Is_32) + (Base_1_64 * Internal.Is_64);
      Base_2    : constant := (Base_2_32 * Internal.Is_32) + (Base_2_64 * Internal.Is_64);
      Base_3    : constant := (Base_3_32 * Internal.Is_32) + (Base_3_64 * Internal.Is_64);
      Base_4    : constant := (Base_4_32 * Internal.Is_32) + (Base_4_64 * Internal.Is_64);
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
               Xps_Count  : Natural;
            when X_Configure_Notify =>
               Cfg_X      : Natural;
               Cfg_Y      : Natural;
               Cfg_Width  : Natural;
               Cfg_Height : Natural;
            when X_Client_Message =>
               Msg_Value  : Internal.Atom;
            when others =>
               Pad        : Internal.Padding;
         end case;
      end record;
      for X_Event_Data use record
         X_Event_Type at  0 * Bytes range 0 .. Bits;

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

      X_Event : X_Event_Data;

      ------------------------------------------------------------------------

   begin  -- Next_Event

      -- Get the event from the X server
      Internal.X_Next_Event (Win.Display, X_Event'Address);

      -- Based on the event type, transfer and convert the event data
      case X_Event.X_Event_Type is

         when X_Key_Press =>
            return (Which     => Key_Press,
                    Key_Data  => (X         => X_Event.Key_X,
                                  Y         => X_Event.Key_Y,
                                  Abs_X     => X_Event.Key_Root_X,
                                  Abs_Y     => X_Event.Key_Root_Y,
                                  Modifiers => Modifier_Mask_To_Set (X_Event.Key_State),
                                  Key_Code  => Raw_Keycode (X_Event.Key_Code)));

         when X_Key_Release =>
            return (Which     => Key_Release,
                    Key_Data  => (X         => X_Event.Key_X,
                                  Y         => X_Event.Key_Y,
                                  Abs_X     => X_Event.Key_Root_X,
                                  Abs_Y     => X_Event.Key_Root_Y,
                                  Modifiers => Modifier_Mask_To_Set (X_Event.Key_State),
                                  Key_Code  => Raw_Keycode (X_Event.Key_Code)));

         when X_Button_Press =>
            return (Which        => Button_Press,
                    Button_Data  => (X         => X_Event.Btn_X,
                                     Y         => X_Event.Btn_Y,
                                     Abs_X     => X_Event.Btn_Root_X,
                                     Abs_Y     => X_Event.Btn_Root_Y,
                                     Modifiers => Modifier_Mask_To_Set (X_Event.Btn_State),
                                     Changed   => Button'Val (X_Event.Btn_Code - 1)));

         when X_Button_Release =>
            return (Which        => Button_Release,
                    Button_Data  => (X         => X_Event.Btn_X,
                                     Y         => X_Event.Btn_Y,
                                     Abs_X     => X_Event.Btn_Root_X,
                                     Abs_Y     => X_Event.Btn_Root_Y,
                                     Modifiers => Modifier_Mask_To_Set (X_Event.Btn_State),
                                     Changed   => Button'Val (X_Event.Btn_Code - 1)));

         when X_Motion_Notify =>
            return (Which       => Pointer_Motion,
                    Motion_Data => (X         => X_Event.Mov_X,
                                    Y         => X_Event.Mov_Y,
                                    Abs_X     => X_Event.Mov_Root_X,
                                    Abs_Y     => X_Event.Mov_Root_Y,
                                    Modifiers => Modifier_Mask_To_Set (X_Event.Mov_State)));

         when X_Enter_Notify =>
            return (Which         => Enter_Window,
                    Crossing_Data => (X         => X_Event.Xng_X,
                                      Y         => X_Event.Xng_Y,
                                      Abs_X     => X_Event.Xng_Root_X,
                                      Abs_Y     => X_Event.Xng_Root_Y));

         when X_Leave_Notify =>
            return (Which     => Leave_Window,
                    Crossing_Data => (X         => X_Event.Xng_X,
                                      Y         => X_Event.Xng_Y,
                                      Abs_X     => X_Event.Xng_Root_X,
                                      Abs_Y     => X_Event.Xng_Root_Y));

         when X_Focus_In =>
            return (Which => Focus_In);

         when X_Focus_Out =>
            return (Which => Focus_Out);

         when X_Expose =>
            return (Which       => Exposed,
                    Expose_Data => (X         => X_Event.Xps_X,
                                    Y         => X_Event.Xps_Y,
                                    Width     => X_Event.Xps_Width,
                                    Height    => X_Event.Xps_Height,
                                    Count     => X_Event.Xps_Count));

         when X_Map_Notify =>
            -- Fake up a "whole window exposed" event
            return (Which       => Exposed,
                    Expose_Data => (X         => 0,
                                    Y         => 0,
                                    Width     => Win.Width,
                                    Height    => Win.Height,
                                    Count     => 0));

         when X_Unmap_Notify =>
            return (Which       => Hidden);

         when X_Configure_Notify =>
            if X_Event.Cfg_Width /= Win.Width or X_Event.Cfg_Height /= Win.Height then
               Win.Width  := X_Event.Cfg_Width;
               Win.Height := X_Event.Cfg_Height;
               return (Which       => Resized,
                       Resize_Data => (Width     => X_Event.Cfg_Width,
                                       Height    => X_Event.Cfg_Height));
            else
               -- Fake up a "whole window exposed" event
               return (Which       => Exposed,
                       Expose_Data => (X         => 0,
                                       Y         => 0,
                                       Width     => X_Event.Cfg_Width,
                                       Height    => X_Event.Cfg_Height,
                                       Count     => 0));
            end if;

         when X_Client_Message =>
            declare
               use type Internal.Atom;
            begin
               if X_Event.Msg_Value = Internal.Delete_Window_Atom then
                  return (Which => Close_Window);
               else
                  return (Which => Unknown_Event);
               end if;
            end;

         when others =>
            return (Which => Unknown_Event);

      end case;

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

      -- Get events and pass them to the selected callback, if there is one
      loop
         if Calls (Event.Which) /= No_Callback then
            Calls (Event.Which) (Event);
         end if;
      end loop;
   end Select_Events;

   ---------------------------------------------------------------------------

end Lumen.Events;
