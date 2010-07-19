
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

with PQueue;
with Lumen.Binary.IO;



package body Lumen.Joystick is

   ---------------------------------------------------------------------------

   -- The task type that reads events from the joystick
   task type Joystick_Event_Task is
      entry Startup (Owner : in Stick_Info_Pointer);
   end Joystick_Event_Task;

   ---------------------------------------------------------------------------

   -- Instantiate the protected-queue package with the queue-event type
   package Event_Queue_Pkg is new PQueue (Joystick_Event_Data);

   ---------------------------------------------------------------------------

   -- A simple binary semaphore
   protected type Signal is
      procedure Set;
      entry     Wait;
   private
      Is_Set : boolean := false;
   end Signal;

   ---------------------------------------------------------------------------

   -- Our internal view of a joystick
   type Axis_Array is array (Positive range <>) of Integer;
   type Button_Array is array (Positive range <>) of Boolean;
   type Stick_Info (Name_Len : Natural;   N_Axes : Natural;   N_Buttons : Natural) is record
      File       : Binary.IO.File_Type;
      Name       : String (1 .. Name_Len);
      Axes       : Axis_Array (1 .. N_Axes);
      Buttons    : Button_Array (1 .. N_Buttons);
      Shutdown   : Signal;
      Event_Task : Joystick_Event_Task;
      Events     : Event_Queue_Pkg.Protected_Queue_Type;
   end record;

   ---------------------------------------------------------------------------
   --
   -- The event task, reads events from the joystick, then queues them
   -- internally so the app can read them
   --
   ---------------------------------------------------------------------------

   task body Joystick_Event_Task is

      ------------------------------------------------------------------------

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
      Event_Data : Joystick_Event_Data;

      ------------------------------------------------------------------------

   begin  -- Joystick_Event_Task

      -- Lets the library tell us which joystick this task belongs to
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
                     if Info.Buttons (Index) then
                        Event_Data := (Which => Joystick_Button_Release, Number => Index);
                     else
                        Event_Data := (Which => Joystick_Button_Press,    Number => Index);
                     end if;
                     Info.Buttons (Index) := not Info.Buttons (Index);
                     Send := True;
                  end if;

               when JS_Axis | JS_Init_Axis =>
                  if Info.Axes (Index) /= Integer (Event_Rec.Value) then
                     Info.Axes (Index) := Integer (Event_Rec.Value);
                     Event_Data := (Which => Joystick_Axis_Change, Number => Index, Axis_Value => Info.Axes (Index));
                     Send := True;
                  end if;

            end case;

            -- If we had a value change, push an event into our internal queue
            if Send then
               Info.Events.Enqueue (Event_Data);
            end if;

         end if;

      end loop;

   end Joystick_Event_Task;

   ---------------------------------------------------------------------------

   -- Simple binary semaphore
   protected body Signal is

      -- Indicate that a shutdown has occurred
      procedure Set is
      begin  -- Set
         Is_Set := true;
      end Set;

      -- Block until the signal is set, then clear it
      entry Wait when Is_Set is
      begin  -- Wait
         Is_Set := false;
      end Wait;

   end Signal;

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


   -- Open a joystick device
   procedure Open (Stick : in out Handle;
                   Path  : in     String := Default_Pathname) is

      use type Interfaces.C.Strings.chars_ptr;

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
         Binary.IO.Open (Stick.Info.File, Path);
         Stick.Info.Name    := Interfaces.C.Strings.Value (Name);
         Stick.Info.Axes    := (others => 0);
         Stick.Info.Buttons := (others => False);

         -- Start the events task for this joystick device
         Stick.Info.Event_Task.Startup (Stick.Info);
      end if;
   end Open;

   ---------------------------------------------------------------------------

   -- Close a joystick device
   procedure Close (Stick : in out Handle) is

      procedure Free is new Ada.Unchecked_Deallocation (Stick_Info, Stick_Info_Pointer);

   begin  -- Close

      -- Close down this joystick
      if Stick.Info /= null then

         -- Shut down this stick's event task
         Stick.Info.Shutdown.Set;

         -- Close the stream and free the info record
         Binary.IO.Close (Stick.Info.File);
         Free (Stick.Info);

      end if;

   end Close;

   ---------------------------------------------------------------------------

   -- Various joystick information functions
   function Name    (Stick : in Handle) return String is
   begin  -- Name
      return Stick.Info.Name;
   end Name;

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

   -- Returns the number of events that are waiting in the joystick's event
   -- queue.  Useful to avoid blocking while waiting for the next event to
   -- show up.
   function Pending (Stick : in Handle) return Natural is
   begin  -- Pending
      return Stick.Info.Events.Length;
   end Pending;

   ---------------------------------------------------------------------------

   -- Read a joystick event
   function Next_Event (Stick : in Handle) return Joystick_Event_Data is
   begin  -- Next_Event
      return Event : Joystick_Event_Data do
         -- Get next event from queue (may block) and return it
         Stick.Info.Events.Dequeue (Event);
      end return;
   end Next_Event;

   ---------------------------------------------------------------------------

end Lumen.Joystick;
