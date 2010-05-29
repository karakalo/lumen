
-- Lumen.Events.Animate -- Event loop with frame-animation callback
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
with Ada.Calendar;


package body Lumen.Events.Animate is

   ---------------------------------------------------------------------------

   -- Module-local procedure to wait until next frame time, if necessary
   procedure Wait_Frame (Win : in Window.Handle;
                         FPS : in Frame_Count) is

      use Ada.Calendar;

      SPF  : Duration := Duration (1.0) / Duration (FPS);
      Been : Duration := Clock - Win.Prior_Frame;

   begin  -- Wait_Frame

      -- No special check needed for first frame, since the initial value
      -- (Never) is a long long time ago.
      if Been > SPF then
         return;
      else
         delay SPF - Been;
      end if;

   end Wait_Frame;

   ---------------------------------------------------------------------------

   -- Simple event loop with a single event callback, plus an animate-frame
   -- callback
   procedure Receive_Events (Win   : in Window.Handle;
                             Call  : in Event_Callback;
                             FPS   : in Frame_Count;
                             Frame : in Animate_Callback) is
   begin  -- Receive_Events

      -- Get events and pass them to the callback
      while Pending (Win) > 0 loop
         Call (Next_Event (Win));
      end loop;

      -- No more events pending; wait until next frame time if necessary
      Wait_Frame (Win, FPS);

      -- Draw next frame
      Frame.all;

   end Receive_Events;

   ---------------------------------------------------------------------------

   -- Simple event loop with multiple event callbacks based on event type,
   -- plus an animate-frame callback
   procedure Select_Events (Win   : in Window.Handle;
                            Calls : in Event_Callback_Table;
                            FPS   : in Frame_Count;
                            Frame : in Animate_Callback) is

      Event : Event_Data;

   begin  -- Select_Events

      -- Get events and pass them to the selected callback, if there is one
      while Pending (Win) > 0 loop
         Event := Next_Event (Win);

         if Calls (Event.Which) /= Events.No_Callback then
            Calls (Event.Which) (Event);
         end if;
      end loop;

      -- No more events pending; wait until next frame time if necessary
      Wait_Frame (Win, FPS);

      -- Draw next frame
      Frame.all;

   end Select_Events;

   ---------------------------------------------------------------------------

   -- Procedure to fetch FPS (and reset rolling average if necessary)
   procedure FPS (Win   : in out Window.Handle;
                  Since : in     FPS_Type := FPS_Since_Prior;
                  Count :    out Float) is

      use Ada.Calendar;

      Elapsed : Duration;
      Frames  : Frame_Count;

   begin  -- FPS

      -- Pick which values to use based on caller's preference
      case Since is
         when FPS_Overall =>
            Elapsed := Clock - Win.App_Start;
            Frames := Win.App_Frames;

         when FPS_Since_Prior =>
            Elapsed := Clock - Win.Last_Start;
            Frames := Win.Last_Frames;

            -- Reset the last-called values
            Win.Last_Start  := Clock;
            Win.Last_Frames := 0;
      end case;

      -- Calculate and return the frames per second
      Count := Float (Frames) / Float (Elapsed);
   end FPS;

   ---------------------------------------------------------------------------

end Lumen.Events.Animate;
