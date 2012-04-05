
-- Simple template program to allow viewing of 2D objects using OpenGL.
-- All setup is done already--just add the OpenGL code to draw the objects
-- below.  You may also add any special initialization you need.  Search
-- for the text "here:" to find the insertion points.  The optimum viewing
-- area lies between -1 and +1 on both axes.
--
-- W. M. Richards, NiEstu, Phoenix AZ, January 2001
-- Ported to Lumen February 2011 by WMR

-- This code is covered by the GNU GPL, http://www.gnu.org/copyleft/gpl.html
--
-- Copyright 2001, 2011  W. M. Richards


-- Environment
with Ada.Characters.Latin_1;
with Ada.Text_IO;

with Lumen.Events.Animate;
with Lumen.Events.Keys;
with Lumen.Font.Txf;
with Lumen.GL;
with Lumen.GLU;
with Lumen.Image;
with Lumen.Window;


-- Enclosing procedure
procedure Simple_2D is

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
   Win          : Window.Handle;
   Curr_W       : Natural   := Win_Start;
   Curr_H       : Natural   := Win_Start;
   Aspect       : GL.Double := GL.Double (Win_Start) / GL.Double (Win_Start);
   StartX       : GL.Double := 0.0;
   StartY       : GL.Double := 0.0;
   RotC         : GL.Double := 0.0;
   Scale        : GL.Double := 1.0;
   XPan         : GL.Double := 0.0;
   YPan         : GL.Double := 0.0;
   Curr_RotC    : GL.Double := 0.0;
   Curr_Scale   : GL.Double := 1.0;
   Curr_XPan    : GL.Double := 0.0;
   Curr_YPan    : GL.Double := 0.0;


   ----------------------------------------------------------------------------
   -----                                                                  -----
   -----                         L O C A L S                              -----
   -----                                                                  -----
   ----------------------------------------------------------------------------

   -- Drop out of the main Gtk loop
   procedure Finish is
   begin  -- Finish
      Events.End_Events (Win);
   end Finish;

   ----------------------------------------------------------------------------

   -- Format a string showing current view status
   function Sts_String return String is

      package IntIO is new Ada.Text_IO.Integer_IO (integer);
      package FltIO is new Ada.Text_IO.Float_IO (GL.Double);

      Pad       : String (1 .. 10) := "          ";
      XPanStr   : String (1 .. 6);
      YPanStr   : String (1 .. 6);
      RotCStr   : String (1 .. 6);
      SclStr    : String (1 .. 6);

   begin  -- Sts_String
      FltIO.Put (XPanStr, XPan, Aft => 2, Exp => 0);
      FltIO.Put (YPanStr, YPan, Aft => 2, Exp => 0);
      IntIO.Put (RotCStr, (360 - Integer (RotC)) mod 360);
      FltIO.Put (SclStr,  Scale, Aft => 2, Exp => 0);
      return Pad & "Pan:  (" & XPanStr & ", " & YPanStr & ") " &
             Pad & "Rotation:  " & RotCStr &
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
         GLU.Ortho_2D (-2.0, 2.0, -2.0 * Aspect, 2.0 * Aspect);
      else
         Aspect := GL.Double (W) / GL.Double (H);
         GLU.Ortho_2D (-2.0 * Aspect, 2.0 * Aspect, -2.0, 2.0);
      end if;
   end Set_View;

   ----------------------------------------------------------------------------

   -- Update the drawing area
   procedure Display is

      ------------------------------------------------------------------------

      -- Local utility routine to draw a line segment
      procedure Draw_Line (X1, Y1, X2, Y2 : in GL.Double) is
      begin  -- Draw_Line
         GL.Begin_Primitive (GL.GL_LINES);
         GL.Vertex (X1, Y1);
         GL.Vertex (X2, Y2);
         GL.End_Primitive;
      end Draw_Line;

      ------------------------------------------------------------------------

      -- Draw visible axes in space
      procedure Draw_Axes is
      begin  -- Draw_Axes
         GL.Color (Float (1.0), 0.0, 0.0, 1.0);   Draw_Line (-1.0,  0.0, 1.0, 0.0);
         GL.Color (Float (0.0), 1.0, 0.0, 1.0);   Draw_Line ( 0.0, -1.0, 0.0, 1.0);
      end Draw_Axes;

      ------------------------------------------------------------------------

   begin  -- Display

      -- Clear to black (the default)
      GL.Clear (GL.GL_COLOR_BUFFER_BIT);

      -- Set up the viewing transforms
      GL.Matrix_Mode (GL.GL_MODELVIEW);
      GL.Load_Identity;
      GL.Translate (XPan, YPan, 0.0);
      GL.Scale (Scale, Scale, Scale);
      GL.Rotate (RotC, 0.0, 0.0, 1.0);

      -- Draw simple 2D axes
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

   -- Callback triggered by the Close_Window event
   procedure Quit_Handler (Event : in Events.Event_Data) is
   begin  -- Quit_Handler
      Finish;
   end Quit_Handler;

   ----------------------------------------------------------------------------

   -- Callback triggered by the Resized event; tell OpenGL that the
   -- user has resized the window
   procedure Resize_Handler (Event : in Events.Event_Data) is
   begin  -- Resize_Handler

      -- Set the new view parameters and redraw the scene
      Set_View (Event.Resize_Data.Width, Event.Resize_Data.Height);
      Display;

   end Resize_Handler;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for keypresses
   procedure Key_Handler (Event : in Events.Event_Data) is

      use type Events.Key_Symbol;

   begin  -- Key_Handler
      case Event.Key_Data.Key is
         when Escape | Letter_q =>
            Finish;

         when Events.Keys.Home =>
            RotC  := 0.0;
            Scale := 1.0;
            XPan  := 0.0;
            YPan  := 0.0;
            Display;

         when others =>
            null;
      end case;
   end Key_Handler;

   ----------------------------------------------------------------------------

   -- Mouse button has been pressed
   procedure Button_Handler (Event : in Events.Event_Data) is

      use type Events.Button;

   begin  -- Button_Handler

      -- Record starting position and current rotation/scale
      StartX := GL.Double (Event.Button_Data.X);
      StartY := GL.Double (Curr_H - Event.Button_Data.Y);
      Curr_RotC  := RotC;
      Curr_Scale := Scale;
      Curr_XPan  := XPan;
      Curr_YPan  := YPan;

      -- Handle mouse wheel events
      if Event.Button_Data.Changed = Events.Button_4 then
         Scale := Curr_Scale - 0.1;
         Display;
      elsif Event.Button_Data.Changed = Events.Button_5 then
         Scale := Curr_Scale + 0.1;
         Display;
      end if;
   end Button_Handler;

   ----------------------------------------------------------------------------

   -- Mouse movement
   procedure Drag_Handler (Event : in Events.Event_Data) is

      X, Y  : Natural;

   begin  -- Drag_Handler

      -- Get the event data
      X := Event.Motion_Data.X;
      Y := Curr_H - Event.Motion_Data.Y;

      -- If it's a drag, update the figure parameters
      if Event.Motion_Data.Modifiers (Events.Mod_Button_1) then
         XPan := Curr_XPan + ((GL.Double (X) - StartX) / GL.Double (Curr_W));
         YPan := Curr_YPan + ((GL.Double (Y) - StartY) / GL.Double (Curr_H));
         Display;
      elsif Event.Motion_Data.Modifiers (Events.Mod_Button_2) then
         RotC := GL.Double (integer (Curr_RotC - ((GL.Double (X) - StartX) + (StartY - GL.Double (Y)))) mod 360);
         Display;
      elsif Event.Motion_Data.Modifiers (Events.Mod_Button_3) then
         Scale := Curr_Scale + ((GL.Double (X) - StartX) / GL.Double (Curr_W)) -
                  ((GL.Double (Y) - StartY) / GL.Double (Curr_H));
         Display;
      end if;

   end Drag_Handler;

   ----------------------------------------------------------------------------

   -- Re-draw the view
   procedure Expose_Handler (Event : in Events.Event_Data) is
   begin  -- Expose_Handler
      Display;
   end Expose_Handler;


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
                     Name       => "2D Object Viewer",
                     Width      => Win_Start,
                     Height     => Win_Start,
                     Events     => (Window.Want_Key_Press     => True,
                                    Window.Want_Button_Press  => True,
                                    Window.Want_Pointer_Drag  => True,
                                    Window.Want_Exposure      => True,
                                    others => False));
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
begin  -- Simple_2D
   Init;

   declare
      use Events;
   begin
      Select_Events (Win   => Win,
                     Calls => (Key_Press      => Key_Handler'Unrestricted_Access,
                               Button_Press   => Button_Handler'Unrestricted_Access,
                               Pointer_Motion => Drag_Handler'Unrestricted_Access,
                               Exposed        => Expose_Handler'Unrestricted_Access,
                               Resized        => Resize_Handler'Unrestricted_Access,
                               Close_Window   => Quit_Handler'Unrestricted_Access,
                               others         => No_Callback));
   end;

exception
   when Program_Exit =>
      null;  -- just exit this block, which terminates the app

end Simple_2D;
