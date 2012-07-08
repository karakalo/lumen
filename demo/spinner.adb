
-- Simple Lumen demo/test program, using earliest incomplete library.

with Lumen.Events; use Lumen.Events;
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

   Win      : Lumen.Window.Window_Handle;
   Wide     : Natural := 400;
   High     : Natural := 400;
   Rotation : Natural := 0;

   ---------------------------------------------------------------------------

   Terminated : Boolean:=False;

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
      GL.Matrix_Mode (GL.GL_PROJECTION);
      GL.Load_Identity;

      if W <= H then
         Aspect := GL.Double (H) / GL.Double (W);
         GLU.Ortho_2D (-2.0, 2.0, -2.0 * Aspect, 2.0 * Aspect);
      else
         Aspect := GL.Double (W) / GL.Double (H);
         GLU.Ortho_2D (-2.0 * Aspect, 2.0 * Aspect, -2.0, 2.0);
      end if;
   end Set_View;

   ---------------------------------------------------------------------------

   -- Draw our scene
   procedure Draw is

      use Lumen;

   begin  -- Draw

      -- Set a black background
      gl.Clear_Color (0.0, 0.0, 0.0, 0.0);
      gl.Clear (GL.GL_COLOR_BUFFER_BIT);

      -- Draw a smooth-blended (the default mode) square going from red to yellow
      GL.Begin_Primitive (GL.GL_POLYGON);
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
      GL.End_Primitive;

      -- Rotate the square around the Z axis by the current amount
      GL.Matrix_Mode (GL.GL_MODELVIEW);
      GL.Load_Identity;
      GL.Rotate (GL.Double (Rotation), 0.0, 0.0, -1.0);

      GL.Flush;

      -- Now show it
      Lumen.Window.Swap (Win);

   end Draw;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for keypresses and close-window events
   procedure KeyPress_Handler
     (Category  : Key_Category;
      Symbol    : Key_Symbol;
      Modifiers : Modifier_Set) is
   begin  -- Quit_Handler
      Terminated:=True;
   end KeyPress_Handler;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for Exposed events
   procedure Expose_Handler
     (Top    : Integer;
      Left   : Integer;
      Height : Natural;
      Width  : Natural) is
   begin  -- Expose_Handler
      Draw;
   end Expose_Handler;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for Resized events
   procedure Resize_Handler
     (Height : Integer;
      Width  : Integer) is
   begin  -- Resize_Handler
      Wide := Width;
      High := Height;
      Set_View (Wide, High);
      Draw;
   end Resize_Handler;

   ---------------------------------------------------------------------------

   -- Our draw-a-frame routine, should get called FPS times a second
   function New_Frame (Frame_Delta : in Duration)
     return Boolean is
   begin  -- New_Frame
      if Rotation >= Max_Rotation then
         Rotation := 0;
      else
         Rotation := Rotation + 1;
      end if;

      Draw;
      return not Terminated;
   end New_Frame;

   ---------------------------------------------------------------------------

begin  -- Spinner

   -- Create Lumen window, accepting most defaults; turn double buffering off
   -- for simplicity
   Lumen.Window.Create (Win, Name   => "Spinning Square Demo",
                        Width  => Wide,
                        Height => High);

   Win.OnExposed := Expose_Handler'Unrestricted_Access;
   Win.OnResize  := Resize_Handler'Unrestricted_Access;
   Win.OnKeyPress := KeyPress_Handler'Unrestricted_Access;
   -- Set up the viewport and scene parameters
   Set_View (Wide, High);

   -- Enter the event loop
   Lumen.Events.Animate.Run(Win,New_Frame'Unrestricted_Access);

end Spinner;
