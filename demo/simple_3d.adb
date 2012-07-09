
-- Simple template program to allow viewing of 3D objects using OpenGL.
-- All setup is done already--just add the OpenGL code to draw the objects
-- below.  You may also add any special initialization you need.  Search
-- for the text "here:" to find the insertion points.  The optimum viewing
-- area lies between -1 and +1 on all three axes.
--
-- W. M. Richards, NiEstu, Phoenix AZ, January 2001
-- Ported to Lumen February 2011 by WMR

-- This code is covered by the GNU GPL, http://www.gnu.org/copyleft/gpl.html
--
-- Copyright 2001, 2011  W. M. Richards


-- Environment
with Ada.Characters.Latin_1;
with Ada.Text_IO;

with Lumen.Events;
with Lumen.Events.Animate;
with Lumen.Events.Keys;
with Lumen.Font.Txf;
with Lumen.GL;
with Lumen.GLU;
with Lumen.Image;
with Lumen.Window;


-- Enclosing procedure
procedure Simple_3D is

   ----------------------------------------------------------------------------

   use Lumen;  -- yes, we're definitely using Lumen

   ----------------------------------------------------------------------------

   -- Constants
   Win_Start : constant GL.SizeI := 500;  -- in pixels; adjust to suit your scene's needs

   -- Keystrokes we care about
   Escape    : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.ESC));
   Letter_q  : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.LC_Q));

   ---------------------------------------------------------------------------

   -- App-specific exceptions
   Program_Error : exception;
   Program_Exit  : exception;

   ----------------------------------------------------------------------------

   -- Program variables
   Win          : Window.Window_Handle;
   Curr_W       : Natural   := Win_Start;
   Curr_H       : Natural   := Win_Start;
   Aspect       : GL.Double := GL.Double (Win_Start) / GL.Double (Win_Start);
   StartX       : GL.Double := 0.0;
   StartY       : GL.Double := 0.0;
   RotV         : GL.Double := 0.0;
   RotH         : GL.Double := 0.0;
   RotC         : GL.Double := 0.0;
   Scale        : GL.Double := 1.0;
   Curr_RotV    : GL.Double := 0.0;
   Curr_RotH    : GL.Double := 0.0;
   Curr_RotC    : GL.Double := 0.0;
   Curr_Scale   : GL.Double := 1.0;
   Terminated   : Boolean := False;


   ----------------------------------------------------------------------------
   -----                                                                  -----
   -----                         L O C A L S                              -----
   -----                                                                  -----
   ----------------------------------------------------------------------------

   -- Drop out of the main Gtk loop
   procedure Finish is
   begin  -- Finish
      Terminated := True;
   end Finish;

   ----------------------------------------------------------------------------

   -- Format a string showing current view status
   function Sts_String return String is

      ------------------------------------------------------------------------

      package IntIO is new Ada.Text_IO.Integer_IO (integer);
      package FltIO is new Ada.Text_IO.Float_IO (GL.Double);

      ------------------------------------------------------------------------

      function Adj (Angle:  GL.Double) return integer is
      begin  -- Adj
         return (360 - Integer (Angle)) mod 360;
      end Adj;

      ------------------------------------------------------------------------

      Pad       : String (1 .. 10) := "          ";
      RotVStr   : String (1 .. 6);
      RotHStr   : String (1 .. 6);
      RotCStr   : String (1 .. 6);
      SclStr    : String (1 .. 6);

      ------------------------------------------------------------------------

   begin  -- Sts_String
      IntIO.Put (RotVStr, Adj (RotV));
      IntIO.Put (RotHStr, Adj (RotH));
      IntIO.Put (RotCStr, Adj (RotC));
      FltIO.Put (SclStr,  Scale, Aft => 2, Exp => 0);
      return Pad & "Rotation:  " & RotVStr & ", " & RotHStr & ", " & RotCStr &
             Pad & "Scale:  " & SclStr;
   end Sts_String;

   ----------------------------------------------------------------------------

   -- Set or reset the window view parameters
   procedure Set_View (W, H : in Natural) is
   begin  -- Set_View

      -- Set the globals used in some calculations
      Curr_W := W;
      Curr_H := H;

      -- Viewport dimensions
      GL.Viewport (0, 0, GL.SizeI (W), GL.SizeI (H));

      -- Set up the projection matrix based on the window's shape--wider than
      -- high, or higher than wide
      GL.Matrix_Mode (GL.GL_PROJECTION);
      GL.Load_Identity;
      if W <= H then
         Aspect := GL.Double (H) / GL.Double (W);
         GL.Frustum (-1.0, 1.0, -Aspect, Aspect, 2.0, 10.0);
      else
         Aspect := GL.Double (W) / GL.Double (H);
         GL.Frustum (-Aspect, Aspect, -1.0, 1.0, 2.0, 10.0);
      end if;
   end Set_View;

   ----------------------------------------------------------------------------

   -- Update the drawing area
   procedure Display is

      ------------------------------------------------------------------------

      use type GL.Bitfield;

      ------------------------------------------------------------------------

      -- Local utility routine to draw a line segment
      procedure Draw_Line (X1, Y1, Z1, X2, Y2, Z2 : in GL.Double) is
      begin  -- Draw_Line
         GL.Begin_Primitive (GL.GL_LINES);
         GL.Vertex (X1, Y1, Z1);
         GL.Vertex (X2, Y2, Z2);
         GL.End_Primitive;
      end Draw_Line;

      ------------------------------------------------------------------------

      -- Draw visible axes in space
      procedure Draw_Axes is
      begin  -- Draw_Axes
         GL.Color (Float (1.0), 0.0, 0.0, 1.0);   Draw_Line (-1.0,  0.0,  0.0, 1.0, 0.0, 0.0);
         GL.Color (Float (0.0), 1.0, 0.0, 1.0);   Draw_Line ( 0.0, -1.0,  0.0, 0.0, 1.0, 0.0);
         GL.Color (Float (0.0), 0.0, 1.0, 1.0);   Draw_Line ( 0.0,  0.0, -1.0, 0.0, 0.0, 1.0);
      end Draw_Axes;

      ------------------------------------------------------------------------

   begin  -- Display

      -- Clear to black (the default)
      GL.Clear (GL.GL_COLOR_BUFFER_BIT or GL.GL_DEPTH_BUFFER_BIT);

      -- Set up the viewing transforms
      GL.Matrix_Mode (GL.GL_MODELVIEW);
      GL.Load_Identity;
      GL.Translate (Float (0.0), 0.0, -5.0);
      GL.Scale (Scale, Scale, Scale);
      GL.Rotate (RotV, 1.0, 0.0, 0.0);
      GL.Rotate (RotH, 0.0, 1.0, 0.0);
      GL.Rotate (RotC, 0.0, 0.0, 1.0);

      -- Draw simple 3D axes
      Draw_Axes;
      GL.Flush;

      -- Put the code to draw your objects here:

      -- Swap the drawing we just did into the display area
      Window.Swap (Win);

      -- Update the status bar

   end Display;


   ----------------------------------------------------------------------------
   -----                                                                  -----
   -----                      C A L L B A C K S                           -----
   -----                                                                  -----
   ----------------------------------------------------------------------------

   -- Callback triggered by the Resized event; tell OpenGL that the
   -- user has resized the window
   procedure Resize_Handler (Height : in Integer;
                             Width  : in Integer) is
   begin  -- Resize_Handler

      -- Set the new view parameters and redraw the scene
      Set_View (Width, Height);
      Display;

   end Resize_Handler;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for keypresses
   procedure Key_Handler (Category  : in Events.Key_Category;
                          Symbol    : in Events.Key_Symbol;
                          Modifiers : in Events.Modifier_Set) is

      use type Events.Key_Symbol;

   begin  -- Key_Handler
      case Symbol is
         when Escape | Letter_q =>
            Finish;

         when Events.Keys.Home =>
            RotV  := 0.0;
            RotH  := 0.0;
            RotC  := 0.0;
            Scale := 1.0;
            Display;

         when others =>
            null;
      end case;
   end Key_Handler;

   ----------------------------------------------------------------------------

   -- Mouse button has been pressed
   procedure Button_Handler (X         : in Integer;
                             Y         : in Integer;
                             Button    : in Window.Button_Enum;
                             Modifiers : in Events.Modifier_Set) is

      use type Window.Button_Enum;

   begin  -- Button_Handler

      -- Record starting position and current rotation/scale
      StartX := GL.Double (X);
      StartY := GL.Double (Curr_H - Y);
      Curr_RotV  := RotV;
      Curr_RotH  := RotH;
      Curr_RotC  := RotC;
      Curr_Scale := Scale;

      -- Handle mouse wheel events
      if Button = Window.Button_4 then
         Scale := Curr_Scale - 0.1;
         Display;
      elsif Button = Window.Button_5 then
         Scale := Curr_Scale + 0.1;
         Display;
      end if;
   end Button_Handler;

   ----------------------------------------------------------------------------

   -- Mouse movement
   procedure Drag_Handler (X         : in Integer;
                           Y         : in Integer;
                           Modifiers : in Events.Modifier_Set) is

      Y_Prime  : Integer;

   begin  -- Drag_Handler

      -- Get the event data
      Y_Prime := Curr_H - Y;

      -- If it's a drag, update the figure parameters
      if Modifiers (Events.Mod_Button_1) then
         RotV := GL.Double (Integer (Curr_RotV + (GL.Double (Y_Prime) - StartY)) mod 360);
         RotH := GL.Double (Integer (Curr_RotH + (GL.Double (X) - StartX)) mod 360);
         Display;
      elsif Modifiers (Events.Mod_Button_2) then
         RotC := GL.Double (integer (Curr_RotC - ((GL.Double (X) - StartX) + (StartY - GL.Double (Y_Prime)))) mod 360);
         Display;
      elsif Modifiers (Events.Mod_Button_3) then
         Scale := Curr_Scale + ((GL.Double (X) - StartX) / GL.Double (Curr_W)) -
                  ((GL.Double (Y_Prime) - StartY) / GL.Double (Curr_H));
         Display;
      end if;

   end Drag_Handler;

   ----------------------------------------------------------------------------

   -- Re-draw the view
   procedure Expose_Handler (Top    : in Integer;
                             Left   : in Integer;
                             Height : in Natural;
                             Width  : in Natural) is
   begin  -- Expose_Handler
      Display;
   end Expose_Handler;

   ----------------------------------------------------------------------------

   -- Called once per frame; just re-draws the scene
   function New_Frame (Frame_Delta : in Duration) return Boolean is
   begin  -- New_Frame
      Display;
      return not Terminated;
   end New_Frame;


   ----------------------------------------------------------------------------
   -----                                                                  -----
   -----                           S E T U P                              -----
   -----                                                                  -----
   ----------------------------------------------------------------------------

   -- Set up the main window and all its fiddly bits
   procedure Init is
   begin  -- Init

      -- Create the main window
      Window.Create (Win,
                     Name       => "3D Object Viewer",
                     Width      => Win_Start,
                     Height     => Win_Start);

      Win.Exposed    := Expose_Handler'Unrestricted_Access;
      Win.Resize     := Resize_Handler'Unrestricted_Access;
      Win.Key_Press  := Key_Handler'Unrestricted_Access;
      Win.Mouse_Down := Button_Handler'Unrestricted_Access;
      Win.Mouse_Move := Drag_Handler'Unrestricted_Access;

      Set_View (Win_Start, Win_Start);
      Display;


      -- Create a packing box to stuff everything into

      -- Create the OpenGL drawing area

      -- Connect the event handlers

      -- Put the OpenGL drawing area into the packing box

      -- The status bar

      -- The quit button

      -- Put any one-time OpenGL initialization code here:

   end Init;


-------------------------------------------------------------------------------
-----                                                                     -----
-----                             M A I N                                 -----
-----                                                                     -----
-------------------------------------------------------------------------------

-- The main procedure
begin  -- Simple_3D
   Init;

   -- Framerate assumed to be 24Hz
   Lumen.Events.Animate.Run (Win, 24, New_Frame'Unrestricted_Access);

exception
   when Program_Exit =>
      null;  -- just exit this block, which terminates the app

end Simple_3D;
