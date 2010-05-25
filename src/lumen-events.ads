
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
with Lumen.Window;

package Lumen.Events is

   -- The types of events that can be reported
   type Event_Type is (No_Event,
                       Key_Press, Key_Release, Button_Press, Button_Release, Pointer_Motion,
                       Enter_Window, Leave_Window, Focus_In, Focus_Out,
                       Exposed, Mapped, Resized, Close_Window);

   -- Raw keycode, not much use except at the very lowest level
   type Raw_Keycode is mod 2 ** Integer'Size;

   -- Keystroke and pointer modifiers
   type Modifier is (Mod_Shift, Mod_Lock, Mod_Control, Mod_1, Mod_2, Mod_3, Mod_4, Mod_5,
                     Mod_Button_1, Mod_Button_2, Mod_Button_3, Mod_Button_4, Mod_Button_5);
   type Modifier_Set is array (Modifier) of Boolean;
   No_Modifiers : Modifier_Set := (others => False);

   -- Pointer buttons
   type Button is (Button_1, Button_2, Button_3, Button_4, Button_5);
   type Button_Set is array (Button) of Boolean;

   -- The data associated with an event
   type Event_Data (Which : Event_Type := No_Event) is record
      X         : Natural;
      Y         : Natural;
      Abs_X     : Natural;
      Abs_Y     : Natural;
      Modifiers : Modifier_Set;
      case Which is
         when Key_Press | Key_Release =>
            Key_Code    : Raw_Keycode;
         when Button_Press | Button_Release =>
            Changed     : Button;
         when Exposed | Resized =>
            Width       : Natural;
            Height      : Natural;
         when others =>
            null;
      end case;
   end record;

   -- An event callback procedure
   type Event_Callback is access procedure (Event : Event_Data);
   No_Callback : constant Event_Callback := null;

   -- A table of callback procedures, used to select on event type
   type Event_Callback_Table is array (Event_Type) of Event_Callback;

   ---------------------------------------------------------------------------

   -- Returns the number of events that are waiting in the event queue.
   -- Useful for more complex event loops.
   function Pending (Win : Window.Handle) return Natural;

   -- Retrieve the next input event from the queue and return it.  Useful for
   -- constructing event loops.
   function Next_Event (Win : Window.Handle) return Event_Data;

   -- Simple event loop with a single callback
   procedure Receive_Events (Win  : in Window.Handle;
                             Call : in Event_Callback);

   -- Simple event loop with multiple callbacks based on event type
   procedure Select_Events (Win   : in Window.Handle;
                            Calls : in Event_Callback_Table);

   ---------------------------------------------------------------------------

end Lumen.Events;
