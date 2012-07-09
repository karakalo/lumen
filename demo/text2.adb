with ada.text_io;   use ada.text_io;
-- Simple Lumen demo/test program to illustrate how to display text, using the
-- texture-mapped font facility.

with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Float_Text_IO;

with Lumen.Events; use Lumen.Events;
with Lumen.Events.Animate;
with Lumen.Events.Keys;
with Lumen.Window;
with Lumen.Font.Txf;
with Lumen.GL;
with Lumen.GLU;

use Lumen;

procedure Text2 is

   ---------------------------------------------------------------------------

   -- Rotation wraps around at this point, in degrees
   Max_Rotation      : constant := 360.0;

   -- Nice peppy game-style framerate, in frames per second
   Framerate         : constant := 60;

   -- A font to fall back on
   Default_Font_Path : constant String := "fsb.txf";

   -- Keystrokes we care about
   Escape   : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.ESC));
   Space    : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.Space));
   Letter_q : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.LC_Q));

   ---------------------------------------------------------------------------

   Win       : Lumen.Window.Window_Handle;
   Direct    : Boolean := True;  -- want direct rendering by default
   Wide      : Natural := 400;
   High      : Natural := 400;
   Img_Wide  : Float;
   Img_High  : Float;
   Rotation  : Float := 0.0;
   Increment : Float := 1.0;
   Rotating  : Boolean := True;
   Tx_Font   : Font.Txf.Handle;
   Object    : GL.UInt;
   Frame     : Natural := 0;

   Attrs     : Window.Context_Attributes := Lumen.Window.Default_Context_Attributes;

   Terminated : Boolean:=False;

   ---------------------------------------------------------------------------

   Program_Error : exception;

   ---------------------------------------------------------------------------

   -- Return number blank-padded on the left out to Width; returns full
   -- number if it's wider than Width.
   function Img (Number : in Integer;
                 Width  : in Positive := 1) return String is

      use Ada.Strings.Fixed;

      Image : String := Trim (Integer'Image (Number), Side => Ada.Strings.Left);

   begin  -- Img
      if Image'Length >= Width then
         return Image;
      else
         return ((Width - Image'Length) * ' ') & Image;
      end if;
   end Img;

   ---------------------------------------------------------------------------

   -- Set or reset the window view parameters
   procedure Set_View (W, H : in Natural) is

      Aspect : GL.Double;

   begin  -- Set_View

      -- Viewport dimensions
      GL.Viewport (0, 0, GL.SizeI (W), GL.SizeI (H));

      -- Size of rectangle upon which text is displayed
      if Wide > High then
         Img_Wide := 1.0;
         Img_High := Float (High) / Float (Wide);
      else
         Img_Wide := Float (Wide) / Float (High);
         Img_High := 1.0;
      end if;

      -- Set up the projection matrix based on the window's shape--wider than
      -- high, or higher than wide
      GL.Matrix_Mode (GL.GL_PROJECTION);
      GL.Load_Identity;

      -- Set up a 3D viewing frustum, which is basically a truncated pyramid
      -- in which the scene takes place.  Roughly, the narrow end is your
      -- screen, and the wide end is 10 units away from the camera.
      if W <= H then
         Aspect := GL.Double (H) / GL.Double (W);
         GL.Frustum (-1.0, 1.0, -Aspect, Aspect, 2.0, 10.0);
      else
         Aspect := GL.Double (W) / GL.Double (H);
         GL.Frustum (-Aspect, Aspect, -1.0, 1.0, 2.0, 10.0);
      end if;

   end Set_View;

   ---------------------------------------------------------------------------

   -- Draw our scene
   procedure Draw is

      use type GL.Bitfield;

      MW    : Natural;
      MA    : Natural;
      MD    : Natural;
      Scale : Float;
      Pad   : Float := Img_Wide / 10.0;  -- margin width

      FNum  : String := Img (Frame, 6);
      FRate : String (1 .. 6);

   begin  -- Draw

      -- Set a light grey background
      GL.Clear_Color (0.85, 0.85, 0.85, 0.0);
      GL.Clear (GL.GL_COLOR_BUFFER_BIT or GL.GL_DEPTH_BUFFER_BIT);

      -- Draw a black rectangle, disabling texturing so we can do plain colors
      GL.Disable (GL.GL_TEXTURE_2D);
      GL.Disable (GL.GL_BLEND);
      GL.Disable (GL.GL_ALPHA_TEST);
      GL.Color (Float (0.0), 0.0, 0.0);
      GL.Begin_Primitive (GL.GL_POLYGON);
      begin
         GL.Vertex (-Img_Wide, -Img_High, 0.0);
         GL.Vertex (-Img_Wide,  Img_High, 0.0);
         GL.Vertex ( Img_Wide,  Img_High, 0.0);
         GL.Vertex ( Img_Wide, -Img_High, 0.0);
      end;
      GL.End_Primitive;

      -- Turn texturing back on and set up to draw the text messages
      GL.Push_Matrix;
      GL.Enable (GL.GL_TEXTURE_2D);
      GL.Enable (GL.GL_ALPHA_TEST);
      GL.Alpha_Func (GL.GL_GEQUAL, 0.0625);
      GL.Enable (GL.GL_BLEND);
      GL.Blend_Func (GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);
      GL.Enable (GL.GL_POLYGON_OFFSET_FILL);
      GL.Polygon_Offset (0.0, -3.0);
      GL.Color (Float (0.1), 0.8, 0.1);

      -- Draw the frame number, right-justified in upper half of rectangle
      GL.Push_Matrix;
      Font.Txf.Get_String_Metrics (Tx_Font, FNum, MW, MA, MD);
      Scale := Img_High / (Float (MA) * 3.0);
      GL.Translate (Img_Wide - (Pad + Float (MW) * Scale), Float (MA) * Scale, 0.0);
      GL.Scale (Scale, Scale, Scale);
      Font.Txf.Render (Tx_Font, FNum);
      GL.Pop_Matrix;

      -- Draw the frame number label, left-justified in upper half of
      -- rectangle, and one-third the size of the number itself
      GL.Push_Matrix;
      Font.Txf.Get_String_Metrics (Tx_Font, "Frame", MW, MA, MD);
      GL.Translate (-(Img_Wide - Pad), Float (MA) * Scale, 0.0);
      GL.Scale (Scale / 3.0, Scale / 3.0, Scale / 3.0);
      Font.Txf.Render (Tx_Font, "Frame");
      GL.Pop_Matrix;

      -- Draw the frame rate, right-justified in lower half of rectangle
      GL.Push_Matrix;
      -- Guard against out-of-range values, and display all question marks if so
      begin
         Ada.Float_Text_IO.Put (FRate, Events.Animate.FPS (Win), Aft => 3, Exp => 0);

      exception
         when others =>
            FRate := (others => '?');
      end;
      Font.Txf.Get_String_Metrics (Tx_Font, FRate, MW, MA, MD);
      GL.Translate (Img_Wide - (Pad + Float (MW) * Scale), -Float (MA) * Scale, 0.0);
      GL.Scale (Scale, Scale, Scale);
      Font.Txf.Render (Tx_Font, FRate);
      GL.Pop_Matrix;

      -- Draw the frame rate label, left-justified in lower half of
      -- rectangle, and one-third the size of the number itself
      GL.Push_Matrix;
      Font.Txf.Get_String_Metrics (Tx_Font, "FPS", MW, MA, MD);
      GL.Translate (-(Img_Wide - Pad), -Float (MA) * Scale, 0.0);
      GL.Scale (Scale / 3.0, Scale / 3.0, Scale / 3.0);
      Font.Txf.Render (Tx_Font, "FPS");
      GL.Pop_Matrix;

      GL.Pop_Matrix;

      -- Rotate the object around the Y and Z axes by the current amount, to
      -- give a "tumbling" effect.
      GL.Matrix_Mode (GL.GL_MODELVIEW);
      GL.Load_Identity;
      GL.Translate (GL.Double (0.0), 0.0, -4.0);
      GL.Rotate (GL.Double (Rotation), 0.0, 1.0, 0.0);
      GL.Rotate (GL.Double (Rotation), 0.0, 0.0, 1.0);

      -- Now show it
      Window.Swap (Win);

   end Draw;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for keypresses
   procedure Key_Handler
     (Category  : Key_Category;
      Symbol    : Key_Symbol;
      Modifiers : Modifier_Set) is

   begin  -- Key_Handler
      case Symbol is
         when Escape | Letter_q =>
            Terminated:=True;
         when Space =>
            Rotating := not Rotating;
         when Events.Keys.Up =>
            if Increment * 2.0 < Float'Last then
               Increment := Increment * 2.0;
            end if;
         when Events.Keys.Down =>
            if Increment / 2.0 > 0.0 then
               Increment := Increment / 2.0;
            end if;
         when others =>
            null;
      end case;
   end Key_Handler;

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
      if Rotating then
         if Rotation >= Max_Rotation then
            Rotation := 0.0;
         else
            Rotation := Rotation + Increment;
         end if;
      end if;

      Frame := Frame + 1;

      Draw;
      return not Terminated;
   end New_Frame;

   ---------------------------------------------------------------------------

begin  -- Text2

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

   -- If other command-line arguments were given then process them
   for Index in 2 .. Ada.Command_Line.Argument_Count loop

      declare
         use Window;
         Arg : String := Ada.Command_Line.Argument (Index);
      begin
         case Arg (Arg'First) is

            when 'a' =>
               Attrs.Alpha_Size := Integer'Value (Arg (Arg'First + 1 .. Arg'Last));

            when 'c' =>
               Attrs.Red_Size:=Integer'Value (Arg (Arg'First + 1 .. Arg'Last));
               Attrs.Blue_Size:=Integer'Value (Arg (Arg'First + 1 .. Arg'Last));
               Attrs.Green_Size:=Integer'Value (Arg (Arg'First + 1 .. Arg'Last));

            when 'd' =>
               Attrs.Depth_Size:=Integer'Value (Arg (Arg'First + 1 .. Arg'Last));

            when 'n' =>
               Direct := False;

            when others =>
               null;

         end case;
      end;

   end loop;

   -- Create Lumen window, accepting most defaults
   Window.Create (Win,
                  Name       => "Text Demo #2, Revenge of the Text",
                  Width      => Wide,
                  Height     => High,
                  Direct     => Direct,
                  Attributes => Attrs);

   Win.Resize    := Resize_Handler'Unrestricted_Access;
   Win.Key_Press := Key_Handler'Unrestricted_Access;
   Win.Exposed   := Expose_Handler'Unrestricted_Access;

   -- Set up the viewport and scene parameters
   Set_View (Wide, High);
   Object := Font.Txf.Establish_Texture (Tx_Font, 0, True);

   -- Enter the event loop
   Lumen.Events.Animate.Run(Win,FrameRate,New_Frame'Unrestricted_Access);

end Text2;
