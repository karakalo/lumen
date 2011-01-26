
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
with Lumen.Window;

package Lumen.Events is

   -- Exceptions defined by this package
   Not_Character : exception;  -- key symbol is not a Latin-1 character

   -- The types of events that can be reported
   type Event_Type is (Unknown_Event,
                       Key_Press, Key_Release, Button_Press, Button_Release, Pointer_Motion,
                       Enter_Window, Leave_Window, Focus_In, Focus_Out,
                       Exposed, Hidden, Resized, Close_Window);

   -- Raw keycode, not much use except at the very lowest level
   type Raw_Keycode is mod 2 ** Integer'Size;

   -- Keystroke and pointer modifiers
   type Modifier is (Mod_Shift, Mod_Lock, Mod_Control, Mod_1, Mod_2, Mod_3, Mod_4, Mod_5,
                     Mod_Button_1, Mod_Button_2, Mod_Button_3, Mod_Button_4, Mod_Button_5);
   type Modifier_Set is array (Modifier) of Boolean;
   No_Modifiers : Modifier_Set := (others => False);

   -- Translated keysym type and value
   type Key_Category is (Key_Control, Key_Graphic, Key_Modifier, Key_Function, Key_Special, Key_Unknown, Key_Not_Translated);
   subtype Key_Symbol is Long_Integer;

   -- Pointer buttons
   type Button is (Button_1, Button_2, Button_3, Button_4, Button_5);
   type Button_Set is array (Button) of Boolean;

   -- The data associated with an event
   type Key_Event_Data is record
      X         : Natural;
      Y         : Natural;
      Abs_X     : Natural;
      Abs_Y     : Natural;
      Modifiers : Modifier_Set;
      Key_Code  : Raw_Keycode;
      Key_Type  : Key_Category;
      Key       : Key_Symbol;
   end record;

   type Button_Event_Data is record
      X         : Natural;
      Y         : Natural;
      Abs_X     : Natural;
      Abs_Y     : Natural;
      Modifiers : Modifier_Set;
      Changed   : Button;
   end record;

   type Motion_Event_Data is record
      X         : Natural;
      Y         : Natural;
      Abs_X     : Natural;
      Abs_Y     : Natural;
      Modifiers : Modifier_Set;
   end record;

   type Crossing_Event_Data is record
      X         : Natural;
      Y         : Natural;
      Abs_X     : Natural;
      Abs_Y     : Natural;
   end record;

   type Expose_Event_Data is record
      X         : Natural;
      Y         : Natural;
      Width     : Natural;
      Height    : Natural;
      Count     : Natural;
   end record;

   type Resize_Event_Data is record
      Width     : Natural;
      Height    : Natural;
   end record;

   type Event_Data (Which : Event_Type := Unknown_Event) is record
      case Which is
         when Key_Press | Key_Release =>
            Key_Data      : Key_Event_Data;
         when Button_Press | Button_Release =>
            Button_Data   : Button_Event_Data;
         when Pointer_Motion =>
            Motion_Data   : Motion_Event_Data;
         when Enter_Window | Leave_Window =>
            Crossing_Data : Crossing_Event_Data;
         when Exposed =>
            Expose_Data   : Expose_Event_Data;
         when Resized =>
            Resize_Data   : Resize_Event_Data;
         when others =>
            null;
      end case;
   end record;

   -- An event callback procedure
   type Event_Callback is access procedure (Event : in Event_Data);
   No_Callback : constant Event_Callback := null;

   -- A table of callback procedures, used to select on event type
   type Event_Callback_Table is array (Event_Type) of Event_Callback;

   ---------------------------------------------------------------------------

   -- Key translation helpers

   -- Convert a Key_Symbol into a Latin-1 character; raises Not_Character if
   -- it's not possible.  Character'Val is simpler.
   function To_Character (Symbol : in Key_Symbol) return Character;

   -- Convert a Key_Symbol into a UTF-8 encoded string; raises Not_Character
   -- if it's not possible.  Really only useful for Latin-1 hibit chars, but
   -- works for all Latin-1 chars.
   function To_UTF_8 (Symbol : in Key_Symbol) return String;

   -- Convert a normal Latin-1 character to a Key_Symbol
   function To_Symbol (Char : in Character) return Key_Symbol;

   pragma Inline (To_Character, To_Symbol);

   ---------------------------------------------------------------------------

   -- Returns the number of events that are waiting in the event queue.
   -- Useful for more complex event loops.
   function Pending (Win : Window.Handle) return Natural;

   -- Retrieve the next input event from the queue and return it.  Useful for
   -- constructing event loops.
   function Next_Event (Win       : in Window.Handle;
                        Translate : in Boolean := True) return Event_Data;

   -- Simple event loop with a single callback
   procedure Receive_Events (Win       : in Window.Handle;
                             Call      : in Event_Callback;
                             Translate : in Boolean := True);

   -- Simple event loop with multiple callbacks based on event type
   procedure Select_Events (Win       : in Window.Handle;
                            Calls     : in Event_Callback_Table;
                            Translate : in Boolean := True);

   -- Terminate internal event loops, causes Receive_Events and Select_Events to return
   procedure End_Events (Win : in Window.Handle);

   ---------------------------------------------------------------------------

end Lumen.Events;
