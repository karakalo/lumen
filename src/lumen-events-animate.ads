
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
with Lumen.Internal;


package Lumen.Events.Animate is

   ---------------------------------------------------------------------------

   -- Count of frames displayed from Animate
   type Frame_Count is new Long_Integer range 0 .. Long_Integer'Last;

   -- Means "display frames as fast as you can"
   Flat_Out : constant Frame_Count := 0;

   ---------------------------------------------------------------------------

   -- An animate-frame callback procedure
   type Animate_Callback is access procedure (Frame_Delta : in Duration);
   No_Callback : constant Animate_Callback := null;

   ---------------------------------------------------------------------------

   -- Simple event loop with a single event callback, plus an animate-frame
   -- callback
   procedure Receive_Events (Win       : in Window.Handle;
                             Call      : in Event_Callback;
                             FPS       : in Frame_Count;
                             Frame     : in Animate_Callback;
                             Translate : in Boolean := True);

   -- Simple event loop with multiple event callbacks based on event type,
   -- plus an animate-frame callback
   procedure Select_Events (Win       : in Window.Handle;
                            Calls     : in Event_Callback_Table;
                            FPS       : in Frame_Count;
                            Frame     : in Animate_Callback;
                            Translate : in Boolean := True);

   -- Type of frames-per-second count to fetch.  "Overall" means since the app
   -- started; "Since_Prior" means since the last time you called FPS.
   type FPS_Type is (FPS_Overall, FPS_Since_Prior);

   -- Function to fetch FPS (and reset rolling average if necessary)
   function FPS (Win   : Window.Handle;
                 Since : FPS_Type := FPS_Since_Prior)
   return Float;

   ---------------------------------------------------------------------------

end Lumen.Events.Animate;
