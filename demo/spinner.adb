
-- Simple Lumen demo/test program, using earliest incomplete library.

with Lumen.Window;
with Lumen.Events.Animate;
with Lumen.GL;
with Lumen.GLU;

procedure Spinner is

   ---------------------------------------------------------------------------

   -- Rotation wraps around at this point, in degrees
   Max_Rotation : constant := 359;

   -- Traditional cinema framrate, in frames per second
   Framerate    : constant := 24;

   ---------------------------------------------------------------------------

   Win      : Lumen.Window.Handle;
   Event    : Lumen.Events.Event_Data;
   Wide     : Natural := 400;
   High     : Natural := 400;
   Rotation : Natural := 0;

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

      -- Set a black background
      gl.ClearColor (0.0, 0.0, 0.0, 0.0);
      gl.Clear (GL.GL_COLOR_BUFFER_BIT);

      -- Draw a smooth-blended (the default mode) square going from red to yellow
      GL.glBegin (GL.GL_POLYGON);
      begin
         GL.Color (Float (1.0), 0.0, 0.0);  -- red
         GL.Vertex (Float (-1.0), -1.0);
         GL.Color (Float (1.0), 0.0, 0.0);  -- red
         GL.Vertex (Float (-1.0),  1.0);
         GL.Color (Float (1.0), 1.0, 0.0);  -- yellow
         GL.Vertex (Float ( 1.0),  1.0);
         GL.Color (Float (1.0), 1.0, 0.0);  -- yellow
         GL.Vertex (Float ( 1.0), -1.0);
      end;
      GL.glEnd;

      -- Rotate the square around the Z axis by the current amount
      GL.MatrixMode (GL.GL_MODELVIEW);
      GL.LoadIdentity;
      GL.Rotate (GL.Double (Rotation), 0.0, 0.0, -1.0);

      GL.Flush;

      -- Now show it
      Lumen.Window.Swap (Win);

   end Draw;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for keypresses and close-window events
   procedure Quit_Handler (Event : in Lumen.Events.Event_Data) is
   begin  -- Quit_Handler
      raise Program_Exit;
   end Quit_Handler;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for Exposed events
   procedure Expose_Handler (Event : in Lumen.Events.Event_Data) is
   begin  -- Expose_Handler
      Draw;
   end Expose_Handler;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for Resized events
   procedure Resize_Handler (Event : in Lumen.Events.Event_Data) is
   begin  -- Resize_Handler
      Wide := Event.Resize_Data.Width;
      High := Event.Resize_Data.Height;
      Set_View (Wide, High);
      Draw;
   end Resize_Handler;

   ---------------------------------------------------------------------------

   -- Our draw-a-frame routine, should get called FPS times a second
   procedure New_Frame (Frame_Delta : in Duration) is
   begin  -- New_Frame
      if Rotation >= Max_Rotation then
         Rotation := 0;
      else
         Rotation := Rotation + 1;
      end if;

      Draw;
   end New_Frame;

   ---------------------------------------------------------------------------

begin  -- Spinner

   -- Create Lumen window, accepting most defaults; turn double buffering off
   -- for simplicity
   Lumen.Window.Create (Win, Name   => "Spinning Square Demo",
                        Width  => Wide,
                        Height => High,
                        Events => (Lumen.Window.Want_Key_Press => True,
                                   Lumen.Window.Want_Exposure  => True,
                                   others => False));

   -- Set up the viewport and scene parameters
   Set_View (Wide, High);

   -- Enter the event loop
   declare
      use Lumen.Events;
   begin
      Animate.Select_Events (Win   => Win,
                             Calls => (Key_Press    => Quit_Handler'Unrestricted_Access,
                                       Exposed      => Expose_Handler'Unrestricted_Access,
                                       Resized      => Resize_Handler'Unrestricted_Access,
                                       Close_Window => Quit_Handler'Unrestricted_Access,
                                       others       => No_Callback),
                             FPS   => Framerate,
                             Frame => New_Frame'Unrestricted_Access);
   end;

exception
   when Program_Exit =>
      null;  -- just exit this block, which terminates the app

end Spinner;
