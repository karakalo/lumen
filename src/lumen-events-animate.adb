
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
   procedure Wait_Frame (Win : in Window_Handle) is

      use Ada.Calendar;

      Been : constant Duration := Clock - Win.Prior_Frame;

   begin  -- Wait_Frame

      -- No special check needed for first frame, since the initial value
      -- (Never) is a long long time ago.
      if Been > Win.SPF then
         return;
      else
         delay Win.SPF - Been;
      end if;

   end Wait_Frame;

   ---------------------------------------------------------------------------

   -- Change FPS after window creation
   procedure Set_FPS (Win : in Window_Handle;
                      FPS : in Frame_Count) is
   begin  -- Set_FPS

      -- Check for special FPS value
      if FPS = Flat_Out then
         Win.SPF := 0.0;
      else
         Win.SPF := Duration (1.0) / Duration (FPS);
      end if;
   end Set_FPS;

   ---------------------------------------------------------------------------

   -- Function to fetch FPS (and reset rolling average if necessary)
   function FPS (Win   : Window_Handle;
                 Since : FPS_Type := FPS_Since_Prior)
   return Float is

      use Ada.Calendar;

      Elapsed : Duration;
      Frames  : Frame_Count;

   begin  -- FPS

      -- Pick which values to use based on caller's preference
      case Since is
         when FPS_Overall =>
            Elapsed := Clock - Win.App_Start;
            Frames := Frame_Count (Win.App_Frames);

         when FPS_Since_Prior =>
            Elapsed := Clock - Win.Last_Start;
            Frames := Frame_Count (Win.Last_Frames);

            -- Reset the last-called values
            Win.Last_Start  := Clock;
            Win.Last_Frames := 0;
      end case;

      -- Calculate and return the frames per second
      return Float (Frames) / Float (Elapsed);

   end FPS;

   ---------------------------------------------------------------------------

   procedure Run (Win   : in Window_Handle;
                  FPS   : in Frame_Count;
                  Frame : in Event_Frame) is
   begin

      if FPS=Flat_Out then
         Win.SPF := 0.0;
      else
         Win.SPF := Duration(1.0) / Duration(FPS);
      end if;

      while Lumen.Window.Process_Events(Win) loop
         Wait_Frame(Win);
         declare
            use Ada.Calendar;
         begin
            exit when not Frame.all(Clock-Win.Prior_Frame);
         end;
         Win.Prior_Frame := Ada.Calendar.Clock;
         Win.App_Frames  := Win.App_Frames+1;
         Win.Last_Frames := Win.Last_Frames+1;
      end loop;
   end Run;

   ---------------------------------------------------------------------------

end Lumen.Events.Animate;
