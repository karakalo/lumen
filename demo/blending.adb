
-- Demo of the blending modes in GL.
-- Uses BlendEquation & BlendFunc to show how texture 2 can be combined with texture 1, which
-- is rendered normally.
--
-- Keys:
--   s   - Change the source blending factor
--   d   - Change the destinsation blending factor
--   e   - Change the blending equation
--   ESC - Quit
--
-- TODO:
--   * Add LOGIC_OP to the Equations.

with Ada.Command_Line;
with Ada.Characters.Latin_1;
with Ada.Text_IO;
with Lumen.Window;
with Lumen.Events;
with Lumen.Events.Keys;
with Lumen.Image;
with Lumen.GL;
with Lumen.GLU;

procedure Blending is

   ---------------------------------------------------------------------------

   type Enum_Array is array (Positive range <>) of Lumen.GL.Enum;

   subtype Factors_Range   is Positive range 1 .. 15;
   subtype Equations_Range is Positive range 1 .. 5;

   Win     : Lumen.Window.Handle;
   Event   : Lumen.Events.Event_Data;
   Wide    : Natural := 640;
   High    : Natural := 480;

   Source_Factor : Factors_Range := Factors_Range'First;
   Dest_Factor   : Factors_Range := Factors_Range'First;
   Factors       : Enum_Array(Factors_Range) :=
     (Lumen.GL.GL_ZERO,
      Lumen.GL.GL_ONE,
      Lumen.GL.GL_SRC_COLOR,
      Lumen.GL.GL_ONE_MINUS_SRC_COLOR,
      Lumen.GL.GL_DST_COLOR,
      Lumen.GL.GL_ONE_MINUS_DST_COLOR,
      Lumen.GL.GL_SRC_ALPHA,
      Lumen.GL.GL_ONE_MINUS_SRC_ALPHA,
      Lumen.GL.GL_DST_ALPHA,
      Lumen.GL.GL_ONE_MINUS_DST_ALPHA,
      Lumen.GL.GL_CONSTANT_COLOR,
      Lumen.GL.GL_ONE_MINUS_CONSTANT_COLOR,
      Lumen.GL.GL_CONSTANT_ALPHA,
      Lumen.GL.GL_ONE_MINUS_CONSTANT_ALPHA,
      Lumen.GL.GL_SRC_ALPHA_SATURATE);
   Factor_Names  : array (Factors_Range) of String(1 .. 27) :=
     ("GL_ZERO                    ",
      "GL_ONE                     ",
      "GL_SRC_COLOR               ",
      "GL_ONE_MINUS_SRC_COLOR     ",
      "GL_DST_COLOR               ",
      "GL_ONE_MINUS_DST_COLOR     ",
      "GL_SRC_ALPHA               ",
      "GL_ONE_MINUS_SRC_ALPHA     ",
      "GL_DST_ALPHA               ",
      "GL_ONE_MINUS_DST_ALPHA     ",
      "GL_CONSTANT_COLOR          ",
      "GL_ONE_MINUS_CONSTANT_COLOR",
      "GL_CONSTANT_ALPHA          ",
      "GL_ONE_MINUS_CONSTANT_ALPHA",
      "GL_SRC_ALPHA_SATURATE      ");
   Equation   : Equations_Range := Equations_Range'First;
   Equations  : Enum_Array(Equations_Range) :=
     (Lumen.GL.GL_FUNC_ADD,
      Lumen.GL.GL_FUNC_SUBTRACT,
      Lumen.GL.GL_FUNC_REVERSE_SUBTRACT,
      Lumen.GL.GL_MIN,
      Lumen.GL.GL_MAX);
   Equation_Names : array (Equations_Range) of String(1 .. 24) :=
     ("GL_FUNC_ADD             ",
      "GL_FUNC_SUBTRACT        ",
      "GL_FUNC_REVERSE_SUBTRACT",
      "GL_MIN                  ",
      "GL_MAX                  ");

   Bitmap1, Bitmap2 : Lumen.Image.Descriptor;
   Tx1, Tx2         : Lumen.GL.UInt;

   ---------------------------------------------------------------------------

   Program_Exit : exception;

   ---------------------------------------------------------------------------

   procedure Next_Factor(Factor : in out Factors_Range) is
   begin  -- Next_Factor

      if Factor = Factors_Range'Last then
	 Factor := Factors_Range'First;
      else
	 Factor := Factor + 1;
      end if;

   end Next_Factor;

   procedure Set_Blend_Function(Source, Dest : in Factors_Range) is
   begin  -- Set_Blend_Function

      Lumen.GL.Blend_Func(Factors(Source), Factors(Dest));

   end Set_Blend_Function;

   procedure Next_Equation(Equation : in out Equations_Range) is
   begin  -- Next_Equation

      if Equation = Equations_Range'Last then
	 Equation := Equations_Range'First;
      else
	 Equation := Equation + 1;
      end if;

   end Next_Equation;

   procedure Set_Equation_Function(Equation : in Equations_Range) is
   begin  -- Set_Equation_Function

      Lumen.GL.Blend_Equation(Equations(Equation));

   end Set_Equation_Function;

   -- Create a texture and bind a 2D image to it
   function Create_Texture(Bitmap : Lumen.Image.Descriptor) return Lumen.GL.UInt is
      use Lumen;

      Result : aliased GL.UInt;
   begin  -- Create_Texture

      -- Allocate a texture name
      GL.Gen_Textures (1, Result'Address);

      -- Bind texture operations to the newly-created texture name
      Gl.Bind_Texture (GL.GL_TEXTURE_2D, Result);

      -- Select modulate to mix texture with color for shading
      GL.Tex_Env (GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_MODULATE);

      -- Wrap textures at both edges
      GL.Tex_Parameter (GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_REPEAT);
      GL.Tex_Parameter (GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_REPEAT);

      -- How the texture behaves when minified and magnified
      GL.Tex_Parameter (GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_NEAREST);--BILINEAR);
      GL.Tex_Parameter (GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_NEAREST);--BILINEAR);

      -- Build our texture from the image we loaded earlier
      GL.Tex_Image (GL.GL_TEXTURE_2D, 0, GL.GL_RGBA,
		   GL.SizeI (Bitmap.Width), GL.SizeI (Bitmap.Height), 0,
                   GL.GL_RGBA, GL.GL_UNSIGNED_BYTE, Bitmap.Values.all'Address);

      return Result;

   end Create_Texture;

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

      use type GL.Bitfield;

   begin  -- Draw

      GL.Clear_Color (0.0, 0.0, 0.0, 0.0);
      GL.Clear (GL.GL_COLOR_BUFFER_BIT);

      GL.Blend_Func(GL.GL_ONE, GL.GL_ZERO);

      GL.Bind_Texture(GL.GL_TEXTURE_2D, Tx1);

      -- GL.Color (Float (1.0), 1.0, 1.0);

      GL.Begin_Primitive (GL.GL_POLYGON);
      begin
	 GL.Tex_Coord (Float (0.0), 0.0);
         GL.Vertex (Float (-1.0), -1.0);

	 GL.Tex_Coord (Float (0.0), 1.0);
         GL.Vertex (Float (-1.0),  1.0);

	 GL.Tex_Coord (Float (1.0), 1.0);
         GL.Vertex (Float ( 1.0),  1.0);

	 GL.Tex_Coord (Float (1.0), 0.0);
         GL.Vertex (Float ( 1.0), -1.0);
      end;
      GL.End_Primitive;

      Set_Equation_Function(Equation);
      Set_Blend_Function(Source_Factor, Dest_Factor);

      GL.Bind_Texture(GL.GL_TEXTURE_2D, Tx2);

      -- GL.Color (Float (1.0), 1.0, 1.0);

      GL.Begin_Primitive (GL.GL_POLYGON);
      begin
	 GL.Tex_Coord (Float (0.0), 0.0);
         GL.Vertex (Float (-0.5), -0.5);

	 GL.Tex_Coord (Float (0.0), 1.0);
         GL.Vertex (Float (-0.5),  1.5);

	 GL.Tex_Coord (Float (1.0), 1.0);
         GL.Vertex (Float ( 1.5),  1.5);

	 GL.Tex_Coord (Float (1.0), 0.0);
         GL.Vertex (Float ( 1.5), -0.5);
      end;
      GL.End_Primitive;
      GL.Flush;

      -- Now show it
      Lumen.Window.Swap (Win);

   end Draw;

   ---------------------------------------------------------------------------

   procedure WindowResize
     (Height : Integer;
      Width  : Integer) is
   begin
      Wide:=Width;
      High:=Height;
      set_View(Wide,High);
      Draw;
   end WindowResize;

   procedure KeyPress
     (Category  : Key_Category;
      Symbol    : Key_Symbol;
      Modifiers : Modifier_Set) is
   begin
      if Symbol=Escape then
         Terminated:=True;
      end if;
      Draw;
   end KeyPress;

   procedure KeyCharacter
     (Character : Character) is
   begin
      case Character is
         when 's' =>
            Next_Factor(Source_Factor);
         when 'd' =>
            Next_Factor(Dest_Factor);
         when 'e' =>
            Next_Equation(Equation);
         when others =>
            null;
      end case;
      Draw;
   end KeyCharacter;

   ---------------------------------------------------------------------------

begin  -- Blending

   if Ada.Command_Line.Argument_Count < 2 then
      raise Program_Error with "Usage: blending <texture 1> <texture 2>";
   end if;

   Ada.Text_IO.Put_Line("Bitmap --> " & Ada.Command_Line.Argument(1));
   Ada.Text_IO.Put_Line("Bitmap --> " & Ada.Command_Line.Argument(2));

   Bitmap1 := Lumen.Image.From_File(Ada.Command_Line.Argument(1));
   Bitmap2 := Lumen.Image.From_File(Ada.Command_Line.Argument(2));

   -- Create Lumen window, accepting most defaults; turn double buffering off
   -- for simplicity
   Lumen.Window.Create (Win,
			Name   => "Blending Demo",
                        Width  => Wide,
                        Height => High,
			Direct => True,
                        Events => (Lumen.Window.Want_Key_Press   => True,
				   Lumen.Window.Want_Key_Release => True,
                                   Lumen.Window.Want_Exposure    => True,
                                   others => False));

   -- Set up the viewport and scene parameters
   Set_View (Wide, High);

   Tx1     := Create_Texture(Bitmap1);
   Tx2     := Create_Texture(Bitmap2);

   Lumen.GL.Enable(Lumen.GL.GL_TEXTURE_2D);
   Lumen.GL.Enable(Lumen.GL.GL_BLEND);
   Lumen.GL.Blend_Color(0.0, 1.0, 0.0, 1.0);  -- Example green for blending with.

   -- Enter the event loop
   Lumen.Events.Receive_Events (Win, Handler'Unrestricted_Access);

exception
   when Program_Exit =>
      null;  -- just exit this block, which terminates the app

end Blending;
