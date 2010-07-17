with ada.text_io; use ada.text_io;
-- Lumen.Joystick -- Support (Linux) joysticks under Lumen
--
-- Chip Richards, NiEstu, Phoenix AZ, Summer 2010

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
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
with System;

with Lumen.Binary.IO;



package body Lumen.Joystick is

   ---------------------------------------------------------------------------

   -- Serial number for open joysticks
   Serial : Positive := 1;

   ---------------------------------------------------------------------------

   -- The task type that reads events from the joystick, and a pointer to it
   task type Joystick_Input_Event_Task is
      entry Startup (Owner : in Stick_Info_Pointer);
   end Joystick_Input_Event_Task;
   type Joystick_Input_Event_Task_Pointer is access Joystick_Input_Event_Task;

   ---------------------------------------------------------------------------

   -- Our internal view of a joystick
   type Axis_Array is array (Positive range <>) of Integer;
   type Button_Array is array (Positive range <>) of Boolean;
   type Stick_Info (Name_Len : Natural;   N_Axes : Natural;   N_Buttons : Natural) is record
      File       : Binary.IO.File_Type;
      Number     : Positive;
      Name       : String (1 .. Name_Len);
      Axes       : Axis_Array (1 .. N_Axes);
      Buttons    : Button_Array (1 .. N_Buttons);
      Shutdown   : Internal.Signal;
      Event_Task : Joystick_Input_Event_Task_Pointer := null;
      Win_Events : Internal.Event_Queue_Pointer      := null;
      Our_Events : Internal.Event_Queue_Pointer      := null;
   end record;

   ---------------------------------------------------------------------------
   --
   -- The event task, reads events from the joystick, then sends them to the
   -- bound window's event queue, or queues them internally so the app can
   -- read them directly
   --
   ---------------------------------------------------------------------------

   task body Joystick_Input_Event_Task is

      ------------------------------------------------------------------------

      use type Internal.Event_Queue_Pointer;
      use type Binary.Byte;

      ------------------------------------------------------------------------

      -- The Linux joystick event
      type JS_Event_Type is (JS_Button, JS_Axis, JS_Init, JS_Init_Button, JS_Init_Axis);
      for JS_Event_Type use (JS_Button => 16#01#, JS_Axis => 16#02#,
                             JS_Init => 16#80#, JS_Init_Button => 16#81#, JS_Init_Axis => 16#82#);
      type JS_Event_Data is record
         Time   : Binary.Word;
         Value  : Binary.S_Short;
         Which  : JS_Event_Type;
         Number : Binary.Byte;
      end record;
      Base : constant := Binary.Word_Bytes;
      for JS_Event_Data use record
         Time   at 0        range 0 .. Binary.Word'Size - 1;
         Value  at Base + 0 range 0 .. Binary.S_Short'Size - 1;
         Which  at Base + 2 range 0 .. Binary.Byte'Size - 1;
         Number at Base + 3 range 0 .. Binary.Byte'Size - 1;
      end record;

      ------------------------------------------------------------------------

      Info       : Stick_Info_Pointer;
      Event_Rec  : JS_Event_Data;
      Index      : Positive;
      Send       : Boolean;
      Event_Data : Internal.Joystick_Event_Data;

      ------------------------------------------------------------------------

   begin  -- Joystick_Input_Event_Task

      -- Wait until there's a display to use, or if the app quits before
      -- creating a window then we just quit this task too
      select
         accept Startup (Owner : in Stick_Info_Pointer) do
            Info := Owner;
         end Startup;
      or
         terminate;
      end select;

      loop

         -- Wait for an event to come from the joystick, or for a shutdown
         -- signal to arrive
         select
            Info.Shutdown.Wait;
            exit;
         then abort
            JS_Event_Data'Read (Ada.Streams.Stream_IO.Stream (Info.File), Event_Rec);
         end select;

         -- Only process recogized event types
         if Event_Rec.Which'Valid then

            Index := Positive (Event_Rec.Number + 1);  -- driver uses zero-based axis and button numbers

            case Event_Rec.Which is

               when JS_Init =>
                  Send := False;  -- dunno what these are, so just ignore them

               when JS_Button | JS_Init_Button =>
                  if Boolean'Pos (Info.Buttons (Index)) /= Natural (Event_Rec.Value) then
                     Info.Buttons (Index) := not Info.Buttons (Index);
                     Event_Data := (Internal.Joystick_Button_Change, Info.Number, Index, Info.Buttons (Index));
                     Send := True;
                  end if;

               when JS_Axis | JS_Init_Axis =>
                  if Info.Axes (Index) /= Integer (Event_Rec.Value) then
                     Info.Axes (Index) := Integer (Event_Rec.Value);
                     Event_Data := (Internal.Joystick_Axis_Change, Info.Number, Index, Info.Axes (Index));
                     Send := True;
                  end if;

            end case;

            -- If we had a value change, send an event, either to the bound
            -- window or to our internal queue
            if Send then
               if Info.Win_Events /= null then
                  Info.Win_Events.Enqueue ((Which => Internal.Joystick_Event, J => Event_Data));
               else
                  Info.Our_Events.Enqueue ((Which => Internal.Joystick_Event, J => Event_Data));
               end if;
            end if;

         end if;

      end loop;

   end Joystick_Input_Event_Task;

   ---------------------------------------------------------------------------
   --
   -- Public subroutines
   --
   ---------------------------------------------------------------------------

   procedure Finalize (Stick : in out Handle) is
   begin  -- Finalize
      if Stick.Info /= null then
         Close (Stick);
      end if;
   end Finalize;

   ---------------------------------------------------------------------------

   -- Open a joystick device, and optionally bind it to a Lumen Window
   procedure Open (Stick : in out Handle;
                   Path  : in     String := Default_Pathname;
                   Win   : in     Window.Handle := Window.No_Window) is

      use type Interfaces.C.Strings.chars_ptr;
      use type Window.Info_Pointer;

      procedure Joystick_Info (Path      : in     String;
                               N_Axes    : in     System.Address;
                               N_Buttons : in     System.Address;
                               Name      :    out Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Joystick_Info, "joystick_info");

      procedure Free (Pointer : in System.Address);
      pragma Import (C, Free, "free");

      Axis_Count   : Binary.Byte;
      Button_Count : Binary.Byte;
      Name         : Interfaces.C.Strings.chars_ptr;

   begin  -- Open

      -- Ask our C helper to fetch the device's info
      Joystick_Info (Path & ASCII.NUL, Axis_Count'Address, Button_Count'Address, Name);

      -- If we can't get the name, then the other values are probably no good either
      if Name = Interfaces.C.Strings.Null_Ptr then
         Stick.Info := null;
         raise Open_Failed;
      else

         -- Local management data for the device
         Stick.Info := new Stick_Info (Name_Len  => Natural (Interfaces.C.Strings.Strlen (Name)),
                                       N_Axes    => Natural (Axis_Count),
                                       N_Buttons => Natural (Button_Count));
         Stick.Info.Number := Serial;
         Serial := Serial + 1;
         Binary.IO.Open (Stick.Info.File, Path);
         Stick.Info.Name    := Interfaces.C.Strings.Value (Name);
         Stick.Info.Axes    := (others => 0);
         Stick.Info.Buttons := (others => False);

         -- Binding events to a window, or reporting them directly?
         if Win.Info = null then
            Stick.Info.Our_Events := new Internal.Event_Queue_Pkg.Protected_Queue_Type;
         else
            Stick.Info.Win_Events := Win.Info.Events;
         end if;

         -- Create an events task for this joystick device
         Stick.Info.Event_Task := new Joystick_Input_Event_Task;
         Stick.Info.Event_Task.Startup (Stick.Info);
      end if;
   end Open;

   ---------------------------------------------------------------------------

   -- Used by Close and Bind
   procedure Free is new Ada.Unchecked_Deallocation (Internal.Event_Queue_Pkg.Protected_Queue_Type,
                                                     Internal.Event_Queue_Pointer);

   ---------------------------------------------------------------------------

   -- Close a joystick device
   procedure Close (Stick : in out Handle) is

      use type Internal.Event_Queue_Pointer;

      procedure Free is new Ada.Unchecked_Deallocation (Stick_Info, Stick_Info_Pointer);
      procedure Free is new Ada.Unchecked_Deallocation (Joystick_Input_Event_Task, Joystick_Input_Event_Task_Pointer);

   begin  -- Close

      -- Close down this joystick
      if Stick.Info /= null then

         -- Shut down and release this stick's event task if there still is one
         if Stick.Info.Event_Task /= null then
            Stick.Info.Shutdown.Set;
            Free (Stick.Info.Event_Task);
         end if;

         -- Free our internal event queue if there is one.  Never free
         -- Win_Events because that belongs to the window, not to us.
         if Stick.Info.Our_Events /= null then
            Free (Stick.Info.Our_Events);
         end if;

         -- Close the stream and free the info record
         Binary.IO.Close (Stick.Info.File);
         Free (Stick.Info);
      end if;
   end Close;

   ---------------------------------------------------------------------------

   -- Bind a joystick's events to a (possibly different) window, or convert to
   -- direct reading by passing No_Window
   procedure Bind (Stick : in out Handle;
                   Win   : in     Window.Handle) is

      use type Internal.Event_Queue_Pointer;
      use type Window.Info_Pointer;

   begin  -- Bind

      -- Binding or unbinding?
      if Win.Info = null then

         -- Unbinding, allocate new internal queue if necessary
         if Stick.Info.Our_Events = null then
            Stick.Info.Our_Events := new Internal.Event_Queue_Pkg.Protected_Queue_Type;
         end if;

         -- Indicate we're no longer bound to a window
         Stick.Info.Win_Events := null;

      else

         -- Binding to a (maybe different) window
         Stick.Info.Win_Events := Win.Info.Events;

         -- Free our internal event queue if there is one, which means we're
         -- switching from direct reading to bound events.
         if Stick.Info.Our_Events /= null then
            Free (Stick.Info.Our_Events);
         end if;
      end if;
   end Bind;

   ---------------------------------------------------------------------------

   -- Various joystick information functions
   function Name    (Stick : in Handle) return String is
   begin  -- Name
      return Stick.Info.Name;
   end Name;

   ---------------------------------------------------------------------------

   function Number  (Stick : in Handle) return Positive is
   begin  -- Number
      return Stick.Info.Number;
   end Number;

   ---------------------------------------------------------------------------

   function Axes    (Stick : in Handle) return Natural is
   begin  -- Axes
      return Stick.Info.N_Axes;
   end Axes;

   ---------------------------------------------------------------------------

   function Buttons (Stick : in Handle) return Natural is
   begin  -- Buttons
      return Stick.Info.N_Buttons;
   end Buttons;

   ---------------------------------------------------------------------------

   -- If you don't bind a joystick to a window, then you need to read its
   -- events directly
   function Next_Event (Stick : in Handle) return Internal.Joystick_Event_Data is

      use type Internal.Event_Queue_Pointer;

      Event : Internal.Queue_Event_Data;

   begin  -- Next_Event

      -- See if we're bound to a window or are keeping our own events
      if Stick.Info.Our_Events = null then
         raise Events_Bound;
      end if;

      -- Have a queue, get next event from it (may block) and return it
      Stick.Info.Our_Events.Dequeue (Event);

      return Event.J;

   end Next_Event;

   ---------------------------------------------------------------------------

end Lumen.Joystick;
