
-- Simple Lumen demo/test program, using earliest incomplete library

with Lumen.Window;
with Lumen.Events;
with Lumen.GL;

procedure Colors is

   Win     : Lumen.Window.Handle;
   Event   : Lumen.Events.Event_Data;

begin  -- Colors

   -- Create Lumen window, accepting most defaults; turn double buffering off
   -- for simplicity
   Lumen.Window.Create (Win, Name => "Ooh, Colors!", Animated => False,
                        Events => (Lumen.Window.Want_Key_Press => True, others => False));

   -- Loop until user hits a key or clicks the window's Close button
   Outer: Loop

      -- Process events if there are any pending
      while Lumen.Events.Pending (Win) > 0 loop
         declare
            use type Lumen.Events.Event_Type;
         begin
            Event := Lumen.Events.Next_Event (Win);

            -- Exit when they destroy the window, or when they hit any key
            exit Outer when Event.Which = Lumen.Events.Key_Press or Event.Which = Lumen.Events.Close_Window;
         end;
      end loop;

      -- Do our drawing
      declare
         use Lumen.GL;
      begin
         -- Red
         Clear_Color (1.0, 0.0, 0.0, 1.0);
         Clear (GL_COLOR_BUFFER_BIT);
         Flush;
         delay 1.0;
          -- Green
         Clear_Color (0.0, 1.0, 0.0, 1.0);
         Clear (GL_COLOR_BUFFER_BIT);
         Flush;
         delay 1.0;
          -- Blue
         Clear_Color (0.0, 0.0, 1.0, 1.0);
         Clear (GL_COLOR_BUFFER_BIT);
         Flush;
         delay 1.0;
      end;
   end loop Outer;

end Colors;
