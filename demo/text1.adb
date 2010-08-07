
-- Simple Lumen demo/test program to illustrate how to display text, using the
-- texture-mapped font facility.

with Ada.Characters.Latin_1;
with Ada.Command_Line;

with Lumen.Window;
with Lumen.Events;
with Lumen.Font.Txf;
with Lumen.GL;
with Lumen.GLU;

use Lumen;

procedure Text1 is

   ---------------------------------------------------------------------------

   Default_Font_Path : constant String := "fsb.txf";

   ---------------------------------------------------------------------------

   Win     : Window.Handle;
   Event   : Events.Event_Data;
   Tx_Font : Font.Txf.Handle;
   Wide    : Natural := 400;
   High    : Natural := 400;
   Object  : GL.UInt;

   ---------------------------------------------------------------------------

   Program_Exit  : exception;
   Program_Error : exception;

   ---------------------------------------------------------------------------

   -- Set or reset the window view parameters
   procedure Set_View (W, H : in Natural) is

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

      ------------------------------------------------------------------------

      MW    : Natural;
      MA    : Integer;
      MD    : Integer;
      Scale : Float;

      ------------------------------------------------------------------------

      procedure Draw_Msg (Msg : in String;
                          Y   : in Float) is
      begin  -- Draw_Msg
         GL.PushMatrix;
         Font.Txf.Get_String_Metrics (Tx_Font, Msg, MW, MA, MD);
         GL.Translate (-(Scale * Float (MW)) / 2.0, Y, 0.0);  -- center each string
         GL.Scale (Scale, Scale, Scale);
         Font.Txf.Render (Tx_Font, Msg);
         GL.PopMatrix;
      end Draw_Msg;

      ------------------------------------------------------------------------

   begin  -- Draw

      -- Draw the white square
      GL.ClearColor (0.0, 0.0, 0.0, 0.0);
      GL.Clear (GL.GL_COLOR_BUFFER_BIT);

      GL.Disable (GL.GL_TEXTURE_2D);
      GL.Disable (GL.GL_BLEND);
      GL.Disable (GL.GL_ALPHA_TEST);

      GL.Color (Float (1.0), 1.0, 1.0);
      GL.glBegin (GL.GL_POLYGON);
      GL.Vertex (Float (-1.0), -1.0);
      GL.Vertex (Float (-1.0),  1.0);
      GL.Vertex (Float ( 1.0),  1.0);
      GL.Vertex (Float ( 1.0), -1.0);
      GL.glEnd;

      -- Draw the text messages
      GL.PushMatrix;
      GL.Enable (GL.GL_TEXTURE_2D);
      GL.Enable (GL.GL_ALPHA_TEST);
      GL.AlphaFunc (GL.GL_GEQUAL, 0.0625);
      GL.Enable (GL.GL_BLEND);
      GL.BlendFunc (GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);
      GL.Enable (GL.GL_POLYGON_OFFSET_FILL);
      GL.PolygonOffset (0.0, -3.0);
      GL.Color (Float (0.0), 0.0, 0.7);
      Font.Txf.Get_String_Metrics (Tx_Font, "OpenGL", MW, MA, MD);
      Scale := 1.5 / Float (MW);  -- draw all messages at the same size
      Draw_Msg ("OpenGL", 0.3);  -- these values were just hand-jobbed, but could be calculated
      Draw_Msg ("does",  -0.1);
      Draw_Msg ("text!", -0.5);
      GL.PopMatrix;

      -- Now show it
      Window.Swap (Win);

   end Draw;

   ---------------------------------------------------------------------------

   -- Simple event handler routine
   procedure Handler (Event : in Events.Event_Data) is

      use Ada.Characters.Latin_1;
      use Events;

   begin  -- Handler

      -- Check if we need to quit
      if Event.Which = Close_Window then
         raise Program_Exit;
      end if;
      if Event.Which = Key_Press and then
         (Event.Key_Data.Key = To_Symbol (ESC) or Event.Key_Data.Key = To_Symbol ('q')) then
         raise Program_Exit;
      end if;

      -- If we were resized, adjust the viewport dimensions
      if Event.Which = Events.Resized then
         Wide := Event.Resize_Data.Width;
         High := Event.Resize_Data.Height;
         Set_View (Wide, High);
      end if;

      -- Any other event just means "redraw" in this app
      Draw;
   end Handler;

   ---------------------------------------------------------------------------

begin  -- Text1

   -- Load the font we'll be using
   if Ada.Command_Line.Argument_Count > 0 then
      declare
         Font_Path : String := Ada.Command_Line.Argument (1);
      begin
         Font.Txf.Load (Tx_Font, Font_Path);

      exception
         when others =>
            raise Program_Error with "cannot find font file """ & Font_Path & """";
      end;
   else
      begin
         Font.Txf.Load (Tx_Font, Default_Font_Path);
      exception
         when others =>
            begin
               Font.Txf.Load (Tx_Font, "demo/" & Default_Font_Path);
            exception
               when others =>
                  raise Program_Error with "cannot find default font file """ & Default_Font_Path & """";
            end;
      end;
   end if;

   -- Create Lumen window, accepting most defaults; turn double buffering off
   -- for simplicity
   Window.Create (Win, Name   => "Text Demo #1",
                  Width  => Wide,
                  Height => High,
                  Events => (Window.Want_Key_Press => True,
                             Window.Want_Exposure  => True,
                             others => False));

   -- Set up the viewport and scene parameters
   Set_View (Wide, High);
   Object := Font.Txf.Establish_Texture (Tx_Font, 0, True);

   -- Enter the event loop
   Events.Receive_Events (Win, Handler'Unrestricted_Access);

exception
   when Program_Exit =>
      null;  -- just exit this block, which terminates the app

end Text1;
