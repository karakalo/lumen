
-- Lumen.Window.X_Input -- Receive X windows input events
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
with Ada.Unchecked_Deallocation;
with System;


package body Lumen.Window.X_Input is

   ---------------------------------------------------------------------------

   -- This is purely a tuning constant.  It's the size of chunk at which the
   -- task's list of windows is allocated.  Each time you add this many new
   -- windows plus one, it extends the list by this much.
   Window_List_Increment : constant := 16;

   -- Types to manage a list of windows, so we can check new incoming events
   -- to the correct window
   type Handle_Array is array (Positive range <>) of Handle;
   type Window_List (Size : Natural) is record
      Curr : Natural := 0;
      List : Handle_Array (1 .. Size);
   end record;
   type Window_List_Pointer is access Window_List;

   -- The window list, started at one "chunk" big.  For many apps, this will
   -- be all they'll ever need.
   Windows : Window_List_Pointer := new Window_List (Window_List_Increment);

   -- For now, we assume all windows are on the same display.  What we
   -- *should* do is start a separate task for each display, sheesh.
   Display : Internal.Display_Pointer := Internal.Null_Display_Pointer;

   ---------------------------------------------------------------------------
   --
   -- Package objects
   --
   ---------------------------------------------------------------------------

   -- A semaphore that allows Shutdown to abort the event task
   protected Shutdown_Signal is
      procedure Set;
      entry     Wait;
   private
      Is_Set : boolean := false;
   end Shutdown_Signal;

   -- Simple binary semaphore, used to tell the event task that the app is
   -- quitting
   protected body Shutdown_Signal is

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

   end Shutdown_Signal;

   ---------------------------------------------------------------------------
   --
   -- Public task
   --
   ---------------------------------------------------------------------------

   task body X_Input_Event_Task is

      use type Internal.Window_ID;

      X_Event : Internal.X_Event_Data;

   begin  -- X_Input_Event_Task

      -- Wait until there's a display to use
      accept Startup;

      loop

         -- Wait for an event to come from the X server, or for a shutdown
         -- signal to arrive
         select
            Shutdown_Signal.Wait;
            exit;
         then abort
            Internal.X_Next_Event (Display, X_Event'Address);
         end select;

         -- Find which window it came from; events coming from unregistered
         -- windows are ignored
         for W in Windows.List'First .. Windows.List'First + Windows.Curr - 1 loop

            if Windows.List (W).Window = X_Event.Window then
               -- Found it, stick the event on the its event queue
               Windows.List (W).Events.Enqueue ((Which => Internal.X_Input_Event, X => X_Event));
            end if;

         end loop;

      end loop;

   end X_Input_Event_Task;

   ---------------------------------------------------------------------------
   --
   -- Public subroutines
   --
   ---------------------------------------------------------------------------

   -- Register a new window with the X input event task
   procedure Add_Window (Win : in Handle) is

      procedure Free is new Ada.Unchecked_Deallocation (Window_List, Window_List_Pointer);

      Extended : Window_List_Pointer;

   begin  -- Add_Window

      -- See if current list is full, and if so, extend it by allocating a new
      -- list, copying the old list to it, and throwing the old list away
      if Windows.Curr >= Windows.Size then

         Extended := new Window_List (Windows.Size + Window_List_Increment);

         for W in Windows.List'Range loop
            Extended.List (W) := Windows.List (W);
         end loop;

         Extended.Curr := Windows.Curr;

         Free (Windows);

         Windows := Extended;
      end if;

      -- Now add the new window
      Windows.Curr := Windows.Curr + 1;
      Windows.List (Windows.Curr) := Win;

      -- Tuck away the display pointer for the task to use.  Once we have a
      -- display, start the event task looking for events on it.  This will
      -- need to change when we support multiple displays.
      declare
         use type Internal.Display_Pointer;
      begin
         if Display = Internal.Null_Display_Pointer then
            Display := Win.Display;
            X_Input_Event_Task.Startup;
         end if;
      end;
   end Add_Window;

   ---------------------------------------------------------------------------

   -- De-register a window with the X input event task
   procedure Drop_Window (Win : in Handle) is
   begin  -- Drop_Window

      -- Will eventually have code here to find the given window and remove it
      -- from the list
      null;
   end Drop_Window;

   ---------------------------------------------------------------------------

   -- Terminates the X input events task
   procedure Shutdown is

      use Lumen.Internal;

      -- Binding to XUnmapWindow, which creates a final event needed to get
      -- the event task unstuck, and XFlush, to push the event through
      procedure X_Unmap_Window (Display : in Display_Pointer;   Window : in Window_ID);
      pragma Import (C, X_Unmap_Window, "XUnmapWindow");
      procedure X_Flush (Display : in Display_Pointer);
      pragma Import (C, X_Flush, "XFlush");

      Win : Window_ID := Windows.List (Windows.List'First).Window;  -- just grab the first one

   begin  -- Shutdown

      -- Terminate X input event task
      X_Unmap_Window (Display, Win);
      X_Flush (Display);
      Shutdown_Signal.Set;
   end Shutdown;

   ---------------------------------------------------------------------------

end Lumen.Window.X_Input;
