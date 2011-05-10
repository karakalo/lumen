
-- Simple Lumen demo/test program, using earliest incomplete library.  Based
-- on an old SGI demo called "simple", which contained the following
-- BSD-flavored copyright notice:
--
-- (c) Copyright 1993, Silicon Graphics, Inc.
-- ALL RIGHTS RESERVED

with Lumen.Window;
with Lumen.Events;
with Lumen.GL;
with Lumen.GLU;

procedure SGI_Simple is

   ---------------------------------------------------------------------------

   Win     : Lumen.Window.Handle;
   Event   : Lumen.Events.Event_Data;
   Wide    : Natural := 400;
   High    : Natural := 400;

   ---------------------------------------------------------------------------

   Program_Exit : exception;

   ---------------------------------------------------------------------------

   -- Set or reset the window view parameters
   procedure Set_View (W, H : in Natural) is

      use Lumen;

      Aspect : GL.Double;

   begin  -- Set_View

      -- Viewport dimensions
      GL.Viewport (0, 0, GL.SizeI (W), GL.SizeI (H));

      -- Set up the projection matrix based on the window's shape--wider than
      -- high, or higher than wide
      GL.MatrixMode (GL.GL_PROJECTION);
      GL.LoadIdentity;

      if W <= H then
         Aspect := GL.Double (H) / GL.Double (W);
         GLU.Ortho2D (-2.0, 2.0, -2.0 * Aspect, 2.0 * Aspect);
      else
         Aspect := GL.Double (W) / GL.Double (H);
         GLU.Ortho2D (-2.0 * Aspect, 2.0 * Aspect, -2.0, 2.0);
      end if;
   end Set_View;

   ---------------------------------------------------------------------------

   -- Draw our scene
   procedure Draw is

      use Lumen;

   begin  -- Draw

      GL.ClearColor (0.0, 0.0, 0.0, 0.0);
      GL.Clear (GL.GL_COLOR_BUFFER_BIT);

      GL.Color (Float (1.0), 1.0, 1.0);

      GL.glBegin (GL.GL_POLYGON);
      begin
         GL.Vertex (Float (-1.0), -1.0);
         GL.Vertex (Float (-1.0),  1.0);
         GL.Vertex (Float ( 1.0),  1.0);
         GL.Vertex (Float ( 1.0), -1.0);
      end;
      GL.glEnd;
      GL.Flush;

      -- Now show it
      Lumen.Window.Swap (Win);

   end Draw;

   ---------------------------------------------------------------------------

   -- Simple event handler routine
   procedure Handler (Event : in Lumen.Events.Event_Data) is

      use type Lumen.Events.Event_Type;

   begin  -- Handler

      -- Check if we need to quit
      if Event.Which = Lumen.Events.Key_Press or Event.Which = Lumen.Events.Close_Window then
         raise Program_Exit;
      end if;

      -- If we were resized, adjust the viewport dimensions
      if Event.Which = Lumen.Events.Resized then
         Wide := Event.Resize_Data.Width;
         High := Event.Resize_Data.Height;
         Set_View (Wide, High);
      end if;

      -- Any other event just means "redraw" in this app
      Draw;
   end Handler;

   ---------------------------------------------------------------------------

begin  -- SGI_Simple

   -- Create Lumen window, accepting most defaults; turn double buffering off
   -- for simplicity
   Lumen.Window.Create (Win, Name   => "Simple Demo",
                        Width  => Wide,
                        Height => High,
                        Events => (Lumen.Window.Want_Key_Press => True,
                                   Lumen.Window.Want_Exposure  => True,
                                   others => False));

   -- Set up the viewport and scene parameters
   Set_View (Wide, High);

   -- Enter the event loop
   Lumen.Events.Receive_Events (Win, Handler'Unrestricted_Access);

exception
   when Program_Exit =>
      null;  -- just exit this block, which terminates the app

end SGI_Simple;
