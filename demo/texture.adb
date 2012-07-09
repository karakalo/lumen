
-- Simple Lumen demo/test program, using earliest incomplete library.

with Ada.Command_Line;

with Lumen.Events; use Lumen.Events;
with Lumen.Events.Animate;
with Lumen.Image;
with Lumen.Window;
with Lumen.GL;
with Lumen.GLU;

with Ada.Text_IO; use Ada.Text_IO;
use Lumen;  -- so we can say "GL.Whatever" anyplace we want


procedure Texture is

   ---------------------------------------------------------------------------

   -- Rotation wraps around at this point, in degrees
   Max_Rotation : constant := 359;

   -- Traditional cinema framrate, in frames per second
   Framerate    : constant := 24;

   ---------------------------------------------------------------------------

   Win      : Lumen.Window.Window_Handle;
   Direct   : Boolean := True;  -- want direct rendering by default
   Wide     : Natural;  -- no longer have default values since they're now set by the image size
   High     : Natural;
   Rotation : Natural := 0;
   Image    : Lumen.Image.Descriptor;
   Img_Wide : Float;
   Img_High : Float;
   Tx_Name  : aliased GL.UInt;

   Terminated : Boolean:=False;

   Attrs    : Lumen.Window.Context_Attributes :=Lumen.Window.Default_Context_Attributes;
   ---------------------------------------------------------------------------

   Program_Error : exception;

   ---------------------------------------------------------------------------

   -- Create a texture and bind a 2D image to it
   procedure Create_Texture is
   begin  -- Create_Texture

      -- Allocate a texture name
      GL.Gen_Textures (1, Tx_Name'Address);

      -- Bind texture operations to the newly-created texture name
      Gl.Bind_Texture (GL.GL_TEXTURE_2D, Tx_Name);

      -- Select modulate to mix texture with color for shading
      GL.Tex_Env (GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_MODULATE);

      -- Wrap textures at both edges
      GL.Tex_Parameter (GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_REPEAT);
      GL.Tex_Parameter (GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_REPEAT);

      -- How the texture behaves when minified and magnified
      GL.Tex_Parameter (GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_NEAREST);
      GL.Tex_Parameter (GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_NEAREST);

      -- Build our texture from the image we loaded earlier
      GL.Tex_Image (GL.GL_TEXTURE_2D, 0, GL.GL_RGBA, GL.SizeI (Image.Width), GL.SizeI (Image.Height), 0,
                    GL.GL_RGBA, GL.GL_UNSIGNED_BYTE, Image.Values.all'Address);

   end Create_Texture;

   ---------------------------------------------------------------------------

   -- Set or reset the window view parameters
   procedure Set_View (W, H : in Natural) is

      Aspect : GL.Double;

   begin  -- Set_View

      -- Viewport dimensions
      GL.Viewport (0, 0, GL.SizeI (W), GL.SizeI (H));

      -- Size of rectangle upon which image is mapped
      if Wide > High then
         Img_Wide := 1.5;
         Img_High := 1.5 * (Float (High) / Float (Wide));
      else
         Img_Wide := 1.5 * (Float (Wide) / Float (High));
         Img_High := 1.5;
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

   begin  -- Draw

      -- Set a light grey background
      GL.Clear_Color (0.8, 0.8, 0.8, 1.0);
      GL.Clear (GL.GL_COLOR_BUFFER_BIT or GL.GL_DEPTH_BUFFER_BIT);

      -- Draw a texture-mapped rectangle with the same aspect ratio as the
      -- original image
      GL.Begin_Primitive (GL.GL_POLYGON);
      begin
         GL.Tex_Coord (Float (0.0), 1.0, 0.0);
         GL.Vertex (-Img_Wide, -Img_High, 0.0);

         GL.Tex_Coord (Float (0.0), 0.0, 0.0);
         GL.Vertex (-Img_Wide,  Img_High, 0.0);

         GL.Tex_Coord (Float (1.0), 0.0, 0.0);
         GL.Vertex ( Img_Wide,  Img_High, 0.0);

         GL.Tex_Coord (Float (1.0), 1.0, 0.0);
         GL.Vertex ( Img_Wide, -Img_High, 0.0);
      end;
      GL.End_Primitive;

      -- Rotate the object around the Y and Z axes by the current amount, to
      -- give a "tumbling" effect.
      GL.Matrix_Mode (GL.GL_MODELVIEW);
      GL.Load_Identity;
      GL.Translate (Float (0.0), 0.0, -4.0);
      GL.Rotate (GL.Double (Rotation), 0.0, 1.0, 0.0);
      GL.Rotate (GL.Double (Rotation), 0.0, 0.0, 1.0);

      GL.Flush;

      -- Now show it
      Lumen.Window.Swap (Win);

   end Draw;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for keypresses and close-window events
   procedure Key_Down_Handler
     (Category  : Key_Category;
      Symbol    : Key_Symbol;
      Modifiers : Modifier_Set) is
   begin  -- Quit_Handler
      Terminated:=True;
   end Key_Down_Handler;

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
      -- Tell the animation procedure wether or not to continue
      -- True => Continue
      return not Terminated;
   end New_Frame;

   ---------------------------------------------------------------------------

begin  -- Texture

   -- If we haven't been given an image to work with, just do nothing
   if Ada.Command_Line.Argument_Count < 1 then
      raise Program_Error with "You did not supply an image file pathname on the command line";
   end if;

   -- If other command-line arguments were given then process them
   for Index in 2 .. Ada.Command_Line.Argument_Count loop

      declare
         use Lumen.Window;
         Arg : String := Ada.Command_Line.Argument (Index);
      begin
         case Arg (Arg'First) is

            when 'a' =>
               Attrs.Alpha_Size:=Integer'Value (Arg (Arg'First + 1 .. Arg'Last));

            when 'c' =>
               Attrs.Red_Size   := Integer'Value (Arg (Arg'First + 1 .. Arg'Last));
               Attrs.Blue_Size  := Integer'Value (Arg (Arg'First + 1 .. Arg'Last));
               Attrs.Green_Size := Integer'Value (Arg (Arg'First + 1 .. Arg'Last));

            when 'd' =>
               Attrs.Depth_Size := Integer'Value (Arg (Arg'First + 1 .. Arg'Last));

            when 'n' =>
               Direct := False;

            when others =>
               null;

         end case;
      end;

   end loop;

   -- Read image and use it to size the window.  This will suck if your image
   -- is very large.
   Image := Lumen.Image.From_File (Ada.Command_Line.Argument (1));
   Wide := Image.Width;
   High := Image.Height;

   -- Create Lumen window, accepting most defaults; turn double buffering off
   -- for simplicity
   Lumen.Window.Create (Win,
                        Name       => "Spinning Picture Demo",
                        Width      => Wide,
                        Height     => High,
                        Direct     => Direct,
                        Attributes => Attrs);

   Win.Exposed   := Expose_Handler'Unrestricted_Access;
   Win.Key_Press := Key_Down_Handler'Unrestricted_Access;
   Win.Resize    := Resize_Handler'Unrestricted_Access;
   -- Set up the viewport and scene parameters
   Set_View (Wide, High);

   -- Now create the texture and set up to use it
   Create_Texture;
   GL.Enable (GL.GL_TEXTURE_2D);
   GL.Bind_Texture (GL.GL_TEXTURE_2D, Tx_Name);

   -- Enter the event loop, which will terminate when the Quit_Handler calls End_Events
   Lumen.Events.Animate.Run(Win,FrameRate,New_Frame'Unrestricted_Access);

   Put_Line("Ordinary Termination");
   -- Try these just to make sure they work
   Lumen.Window.Destroy (Win);

end Texture;
