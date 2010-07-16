
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

with XArray;


package body Lumen.Window.X_Input is

   ---------------------------------------------------------------------------

   -- Create an extensible array to hold the window list, with "chunk" size
   -- 16.  For many apps, this will be all they'll ever need.
   package Window_List is new XArray (Data_Type => Info_Pointer, Increment => 16);

   -- The window list
   Windows : Window_List.Pointer := Window_List.Create;

   -- For now, we assume all windows are on the same display.  What we
   -- *should* do is start a separate task for each display, sheesh.
   Display : Internal.Display_Pointer := Internal.Null_Display_Pointer;

   -- The task that reads events from the X server
   task X_Input_Event_Task is
      entry Startup;
   end X_Input_Event_Task;

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
   -- Package subroutines
   --
   ---------------------------------------------------------------------------

   -- Terminates the X input events task
   procedure Shutdown is

      use Lumen.Internal;

      -- Binding to XUnmapWindow, which creates a final event needed to get
      -- the event task unstuck, and XFlush, to push the event through
--      procedure X_Unmap_Window (Display : in Display_Pointer;   Window : in Window_ID);
--      pragma Import (C, X_Unmap_Window, "XUnmapWindow");
      procedure X_Flush (Display : in Display_Pointer);
      pragma Import (C, X_Flush, "XFlush");

--      Win : Window_ID := Windows.Data (Windows.Data'First).Window;  -- should be the only one left

   begin  -- Shutdown

      -- Terminate X input event task
--      X_Unmap_Window (Display, Win);
      X_Flush (Display);
      Shutdown_Signal.Set;
   end Shutdown;

   ---------------------------------------------------------------------------
   --
   -- The event task, reads events from the X server, then sends them to the app
   --
   ---------------------------------------------------------------------------

   task body X_Input_Event_Task is

      use type Internal.Window_ID;

      X_Event : Internal.X_Event_Data;

   begin  -- X_Input_Event_Task

      -- Wait until there's a display to use, or if the app quits before
      -- creating a window then we just quit this task too
      select
         accept Startup;
      or
         terminate;
      end select;

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

         for W in Windows.Data'First .. Windows.Data'First + Windows.Current - 1 loop

            if Windows.Data (W).Window = X_Event.Window then
               -- Found it, stick the event on the its event queue
               Windows.Data (W).Events.Enqueue ((Which => Internal.X_Input_Event, X => X_Event));
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
   begin  -- Add_Window

      -- Add the new window
      Window_List.Append (Windows, Win.Info);

      -- Tuck away the display pointer of the first window we add, for the
      -- task to use.  Once we have a display, start the event task looking
      -- for events on it.  This will need to change when we support multiple
      -- displays.
      declare
         use type Internal.Display_Pointer;
      begin
         if Display = Internal.Null_Display_Pointer then
            Display := Win.Info.Display;
            X_Input_Event_Task.Startup;
         end if;
      end;
   end Add_Window;

   ---------------------------------------------------------------------------

   -- De-register a window with the X input event task
   procedure Drop_Window (Win : in Handle) is

      Found : Natural := Window_List.Find (Windows, Win.Info);

   begin  -- Drop_Window

      -- If window found, otherwise just ignore the request
      if Found > 0 then

            -- Found it.  First, see if it's the last one left
            if Windows.Current = Windows.Data'First then

               -- Dropping last window, so shut down the events task
               Shutdown;

            else

               -- Found it, but it's not the last window, so just remove it
               -- from the list
               Window_List.Delete (Windows, Found);

            end if;

      end if;

   end Drop_Window;

   ---------------------------------------------------------------------------

end Lumen.Window.X_Input;
