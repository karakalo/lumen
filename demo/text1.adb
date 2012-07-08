
-- Simple Lumen demo/test program to illustrate how to display text, using the
-- texture-mapped font facility.

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Command_Line;

with Lumen.Window;
with Lumen.Events; use Lumen.Events;
with Lumen.Font.Txf;
with Lumen.GL;
with Lumen.GLU;

use Lumen;

procedure Text1 is

   ---------------------------------------------------------------------------

   Default_Font_Path : constant String := "fsb.txf";

   ---------------------------------------------------------------------------

   Win     : Lumen.Window.Window_Handle;
   Tx_Font : Font.Txf.Handle;
   Wide    : Natural := 400;
   High    : Natural := 400;
   Object  : GL.UInt;

   ---------------------------------------------------------------------------

   Terminated : Boolean:=False;
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

      ------------------------------------------------------------------------

      MW    : Natural;
      MA    : Integer;
      MD    : Integer;
      Scale : Float;

      ------------------------------------------------------------------------

      procedure Draw_Msg (Msg : in String;
                          Y   : in Float) is
      begin  -- Draw_Msg
         GL.Push_Matrix;
         Font.Txf.Get_String_Metrics (Tx_Font, Msg, MW, MA, MD);
         GL.Translate (-(Scale * Float (MW)) / 2.0, Y, 0.0);  -- center each string
         GL.Scale (Scale, Scale, Scale);
         Font.Txf.Render (Tx_Font, Msg);
         GL.Pop_Matrix;
      end Draw_Msg;

      ------------------------------------------------------------------------

   begin  -- Draw

      -- Draw the white square
      GL.Clear_Color (0.0, 0.0, 0.0, 0.0);
      GL.Clear (GL.GL_COLOR_BUFFER_BIT);

      GL.Disable (GL.GL_TEXTURE_2D);
      GL.Disable (GL.GL_BLEND);
      GL.Disable (GL.GL_ALPHA_TEST);

      GL.Color (Float (1.0), 1.0, 1.0);
      GL.Begin_Primitive (GL.GL_POLYGON);
      GL.Vertex (Float (-1.0), -1.0);
      GL.Vertex (Float (-1.0),  1.0);
      GL.Vertex (Float ( 1.0),  1.0);
      GL.Vertex (Float ( 1.0), -1.0);
      GL.End_Primitive;

      -- Draw the text messages
      GL.Push_Matrix;
      GL.Enable (GL.GL_TEXTURE_2D);
      GL.Enable (GL.GL_ALPHA_TEST);
      GL.Alpha_Func (GL.GL_GEQUAL, 0.0625);
      GL.Enable (GL.GL_BLEND);
      GL.Blend_Func (GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);
      GL.Enable (GL.GL_POLYGON_OFFSET_FILL);
      GL.Polygon_Offset (0.0, -3.0);
      GL.Color (Float (0.0), 0.0, 0.7);
      Font.Txf.Get_String_Metrics (Tx_Font, "OpenGL", MW, MA, MD);
      Scale := 1.5 / Float (MW);  -- draw all messages at the same size
      Draw_Msg ("OpenGL", 0.3);  -- these values were just hand-jobbed, but could be calculated
      Draw_Msg ("does",  -0.1);
      Draw_Msg ("text!", -0.5);
      GL.Pop_Matrix;

      -- Now show it
      Window.Swap (Win);

   end Draw;

   ---------------------------------------------------------------------------

   procedure Key_Press
     (Category  : Key_Category;
      Symbol    : Key_Symbol;
      Modifiers : Modifier_Set) is
   begin
      if Symbol=To_Symbol(Esc) or
        Symbol=To_Symbol('q') then
         Terminated:=True;
      end if;
   end Key_Press;

   procedure Window_Resize
     (Height : Integer;
      Width  : Integer) is
   begin
      Wide:=Width;
      High:=Height;
      Set_View(Width,Height);
      Draw;
   end Window_Resize;

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
               Font.Txf.Load (Tx_Font, "data/" & Default_Font_Path);
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
                  Height => High);
   Win.Key_Press := Key_Press'Unrestricted_Access;
   Win.Resize    := Window_Resize'Unrestricted_Access;

   -- Set up the viewport and scene parameters
   Set_View (Wide, High);
   Object := Font.Txf.Establish_Texture (Tx_Font, 0, True);
   Draw;

   -- Enter the event loop
   while Lumen.Window.Process_Events(Win) loop
      exit when Terminated;
   end loop;

end Text1;
