
-- Simple Lumen demo/test program, using earliest incomplete library.

with Lumen.Window;
with Lumen.Events.Animate;
with GL;
with GLU;

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

      -- Set a black background
      glClearColor (0.0, 0.0, 0.0, 0.0);
      glClear (GL_COLOR_BUFFER_BIT);

      -- Draw a hideous orange square
      glColor3f (1.0, 1.0, 0.5);

      glBegin (GL_POLYGON);
      begin
         glVertex2f (-1.0, -1.0);
         glVertex2f (-1.0,  1.0);
         glVertex2f ( 1.0,  1.0);
         glVertex2f ( 1.0, -1.0);
      end;
      glEnd;

      -- Rotate the square around the Z axis by the current amount
      glMatrixMode (GL_MODELVIEW);
      glLoadIdentity;
      glRotated (GLdouble (Rotation), 0.0, 0.0, -1.0);

      glFlush;

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
   procedure New_Frame is
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
   Win := Lumen.Window.Create (Name   => "Spinning Square Demo",
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
