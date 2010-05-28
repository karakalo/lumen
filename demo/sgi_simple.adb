
-- Simple Lumen demo/test program, using earliest incomplete library.  Based
-- on an old SGI demo called "simple", which contained the following
-- BSD-flavored copyright notice:
--
--  (c) Copyright 1993, Silicon Graphics, Inc.
--  ALL RIGHTS RESERVED
--
-- Build with: gnatmake -P sgi_simple.gpr

with Lumen.Window;
with Lumen.Events;
with GL;
with GLU;

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

      use GL;
      use GLU;

      Aspect : GLdouble;

   begin  -- Set_View

      -- Viewport dimensions
      glViewport (0, 0, GLsizei (W), GLsizei (H));

      -- Set up the projection matrix based on the window's shape--wider than
      -- high, or higher than wide
      glMatrixMode (GL_PROJECTION);
      glLoadIdentity;

      if W <= H then
         Aspect := GLdouble (H) / GLdouble (W);
         gluOrtho2D (-2.0, 2.0, -2.0 * Aspect, 2.0 * Aspect);
      else
         Aspect := GLdouble (W) / GLdouble (H);
         gluOrtho2D (-2.0 * Aspect, 2.0 * Aspect, -2.0, 2.0);
      end if;
   end Set_View;

   ---------------------------------------------------------------------------

   -- Draw our scene
   procedure Draw is

      use GL;

   begin  -- Draw

      glClearColor (0.0, 0.0, 0.0, 0.0);
      glClear (GL_COLOR_BUFFER_BIT);

      glColor3f (1.0, 1.0, 1.0);

      glBegin (GL_POLYGON);
      begin
         glVertex2f (-1.0, -1.0);
         glVertex2f (-1.0,  1.0);
         glVertex2f ( 1.0,  1.0);
         glVertex2f ( 1.0, -1.0);
      end;
      glEnd;
      glFlush;

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
   Win := Lumen.Window.Create (Name   => "Simple Demo",
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
