
-- Simple Lumen demo/test program to illustrate how to use two separate
-- windows

with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Float_Text_IO;

with Lumen.Events.Animate;
with Lumen.Events.Keys;
with Lumen.Window;
with Lumen.Font.Txf;
with Lumen.GL;
with Lumen.GLU;
with Lumen.Image;

use Lumen;

procedure Multi is

   ---------------------------------------------------------------------------

   -- Initial window dimensions
   Scene_Win_Width   : constant := 400;
   Scene_Win_Height  : constant := 400;
   Data_Win_Width    : constant := 200;
   Data_Win_Height   : constant := 100;

   -- Rotation wraps around at this point, in degrees
   Max_Rotation      : constant := 360.0;

   -- Default rotation per frame, in degrees
   Default_Increment : constant := 1.0;

   -- Good-enough framerate for what we're doing
   Framerate         : constant := 30;

   -- A font to fall back on
   Default_Font_Path : constant String := "fsb.txf";

   -- Colors used to construct our texture image
   Red               : constant Image.Pixel := (255, 0, 0, 0);
   White             : constant Image.Pixel := (0, 0, 255, 0);  -- actually blue right now

   -- Keystrokes we care about
   Escape   : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.ESC));
   Space    : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.Space));
   Equals   : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.Equals_Sign));
   Letter_q : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.LC_Q));

   ---------------------------------------------------------------------------

   Scene      : Window.Handle;
   Data       : Window.Handle;
   Direct     : Boolean := True;  -- want direct rendering by default
   Event      : Events.Event_Data;
   Scene_Wide : Float := Float (Scene_Win_Width);
   Scene_High : Float := Float (Scene_Win_Height);
   Rotation   : Float := 0.0;
   Increment  : Float := Default_Increment;
   Rotating   : Boolean := True;
   Tx_Font    : Font.Txf.Handle;
   Object     : GL.UInt;
   Frame      : Natural := 0;
   Quad       : GLU.Quadric := GLU.NewQuadric;
   Checks     : Image.Pixel_Matrix (1 .. 32, 1 .. 32);
   Check_Tx   : GL.UInt;

   Attrs     : Window.Context_Attributes :=
      (
       (Window.Attr_Red_Size,    8),
       (Window.Attr_Green_Size,  8),
       (Window.Attr_Blue_Size,   8),
       (Window.Attr_Alpha_Size,  8),
       (Window.Attr_Depth_Size, 24)
      );

   ---------------------------------------------------------------------------

   Program_Error : exception;
   Program_Exit  : exception;

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

   -- Set or reset the scene window's view parameters
   procedure Set_Scene_View (W, H : in Natural) is

      Aspect : GL.Double;

   begin  -- Set_Scene_View

      -- Set the scene window
      Window.Make_Current (Scene);

      -- Presere these for drawing use
      Scene_Wide := Float (W);
      Scene_High := Float (H);

      -- Viewport dimensions
      GL.Viewport (0, 0, GL.SizeI (W), GL.SizeI (H));

      -- Size of scene window in world units
      if W > H then
         Scene_Wide := 1.0;
         Scene_High := Float (H) / Float (W);
      else
         Scene_Wide := Float (W) / Float (H);
         Scene_High := 1.0;
      end if;

      -- Set up the projection matrix based on the window's shape--wider than
      -- high, or higher than wide
      GL.MatrixMode (GL.GL_PROJECTION);
      GL.LoadIdentity;

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

   end Set_Scene_View;

   ---------------------------------------------------------------------------

   -- Set or reset the data window's view parameters
   procedure Set_Data_View (W, H : in Natural) is

      Aspect : GL.Double;

   begin  -- Set_Data_View

      -- Set the data window
      Window.Make_Current (Data);

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

      -- This makes one world unit equal one screen pixel (the "duh" projection)
      --glOrtho (0.0, GLdouble (Window_Width), 0.0, GLdouble (Window_Height), -1.0, 1.0);

   end Set_Data_View;

   ---------------------------------------------------------------------------

   -- Draw our data
   procedure Draw_Data is

      use type GL.Bitfield;

      MW    : Natural;
      MA    : Natural;
      MD    : Natural;
      Scale : Float;
      Pad   : Float := Scene_Wide / 10.0;  -- margin width

      FNum  : String := Img (Frame, 6);
      FRate : String (1 .. 6);

   begin  -- Draw_Data

      -- Draw the text in the data window
      Window.Make_Current (Data);

      -- Set a black background
      GL.ClearColor (0.0, 0.0, 0.0, 0.0);
      GL.Clear (GL.GL_COLOR_BUFFER_BIT or GL.GL_DEPTH_BUFFER_BIT);

      -- Turn texturing back on and set up to draw the text messages
      GL.PushMatrix;
      GL.Enable (GL.GL_TEXTURE_2D);
      GL.Enable (GL.GL_ALPHA_TEST);
      GL.AlphaFunc (GL.GL_GEQUAL, 0.0625);
      GL.Enable (GL.GL_BLEND);
      GL.BlendFunc (GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);
      GL.Enable (GL.GL_POLYGON_OFFSET_FILL);
      GL.PolygonOffset (0.0, -3.0);
      GL.Color (Float (0.1), 0.8, 0.1);
      Font.Txf.Bind_Font_Texture (Tx_Font);

      -- Draw the frame number, right-justified in upper half of rectangle
      GL.PushMatrix;
      Font.Txf.Get_String_Metrics (Tx_Font, FNum, MW, MA, MD);
      Scale := 2.0 / (Float (MA) * 3.0);
      GL.Translate (2.0 - (Pad + Float (MW) * Scale), Float (MA) * Scale, 0.0);
      GL.Scale (Scale, Scale, Scale);
      Font.Txf.Render (Tx_Font, FNum);
      GL.PopMatrix;

      -- Draw the frame number label, left-justified in upper half of
      -- rectangle, and one-third the size of the number itself
      GL.PushMatrix;
      Font.Txf.Get_String_Metrics (Tx_Font, "Frame", MW, MA, MD);
      GL.Translate (-(2.0 - Pad), Float (MA) * Scale, 0.0);
      GL.Scale (Scale / 3.0, Scale / 3.0, Scale / 3.0);
      Font.Txf.Render (Tx_Font, "Frame");
      GL.PopMatrix;

      -- Draw the frame rate, right-justified in lower half of rectangle
      GL.PushMatrix;
      -- Guard against out-of-range values, and display all question marks if so
      begin
         Ada.Float_Text_IO.Put (FRate, Events.Animate.FPS (Scene), Aft => 3, Exp => 0);

      exception
         when others =>
            FRate := (others => '?');
      end;
      Font.Txf.Get_String_Metrics (Tx_Font, FRate, MW, MA, MD);
      GL.Translate (2.0 - (Pad + Float (MW) * Scale), -Float (MA) * Scale, 0.0);
      GL.Scale (Scale, Scale, Scale);
      Font.Txf.Render (Tx_Font, FRate);
      GL.PopMatrix;

      -- Draw the frame rate label, left-justified in lower half of
      -- rectangle, and one-third the size of the number itself
      GL.PushMatrix;
      Font.Txf.Get_String_Metrics (Tx_Font, "FPS", MW, MA, MD);
      GL.Translate (-(2.0 - Pad), -Float (MA) * Scale, 0.0);
      GL.Scale (Scale / 3.0, Scale / 3.0, Scale / 3.0);
      Font.Txf.Render (Tx_Font, "FPS");
      GL.PopMatrix;

      GL.PopMatrix;

      -- Now show it
      Window.Swap (Data);

   end Draw_Data;

   ---------------------------------------------------------------------------

   -- Draw our scene
   procedure Draw_Scene is

      use type GL.Bitfield;

   begin  -- Draw_Scene

      -- Draw the spinning square in the data window
      Window.Make_Current (Scene);

      -- Set a light grey background
      GL.ClearColor (0.85, 0.85, 0.85, 0.0);
      GL.Clear (GL.GL_COLOR_BUFFER_BIT or GL.GL_DEPTH_BUFFER_BIT);

      -- Draw a textured sphere
      GL.Color (Float (1.0), 1.0, 1.0);  -- white
      GL.Enable (GL.GL_TEXTURE_2D);
      GL.BindTexture (GL.GL_TEXTURE_2D, Check_Tx);
      GLU.Sphere (Quad, 1.0, 32, 32);
      -- GL.glBegin (GL.GL_POLYGON);
      -- begin
      --    GL.TexCoord (Float (0.0), 1.0);
      --    GL.Vertex (Float (-1.0), -1.0);

      --    GL.TexCoord (Float (0.0), 0.0);
      --    GL.Vertex (Float (-1.0),  1.0);

      --    GL.TexCoord (Float (1.0), 0.0);
      --    GL.Vertex ( Float (1.0),  1.0);

      --    GL.TexCoord (Float (1.0), 1.0);
      --    GL.Vertex ( Float (1.0), -1.0);
      -- end;
      -- GL.glEnd;

      -- Rotate the sphere around the Z axis by the current amount
      GL.MatrixMode (GL.GL_MODELVIEW);
      GL.LoadIdentity;

      -- Move the scene back a bit from the viewing plane so we can actually see it
      GL.Translate (GL.Double (0.0), 0.0, -4.0);
      GL.Rotate (Rotation, 1.0,  0.0, 0.0);
      GL.Rotate (Rotation, 0.0, -1.0, 0.0);

      -- Now show it
      Window.Swap (Scene);

   end Draw_Scene;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for close-window events
   procedure Quit_Handler (Event : in Events.Event_Data) is
   begin  -- Quit_Handler
      raise Program_Exit;
   end Quit_Handler;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for keypresses
   procedure Key_Handler (Event : in Events.Event_Data) is

      use type Events.Key_Symbol;

   begin  -- Key_Handler
      case Event.Key_Data.Key is
         when Escape | Letter_q =>
            raise Program_Exit;
         when Space =>
            Rotating := not Rotating;
         when Equals =>
            Increment := Default_Increment;
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
   procedure Expose_Handler (Event : in Events.Event_Data) is
   begin  -- Expose_Handler
      Draw_Scene;
   end Expose_Handler;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for Resized events
   procedure Resize_Handler (Event : in Events.Event_Data) is
   begin  -- Resize_Handler
      Set_Scene_View (Event.Resize_Data.Width, Event.Resize_Data.Height);
      Draw_Scene;
   end Resize_Handler;

   ---------------------------------------------------------------------------

   -- Our draw-a-frame routine, should get called FPS times a second
   procedure New_Frame (Frame_Delta : in Duration) is

      use type Events.Event_Data;

      Event_2 : Events.Event_Data;

   begin  -- New_Frame

      -- Check for events from our data window
      while Events.Pending (Data) > 0 loop
         Event_2 := Events.Next_Event (Data);
         case Event_2.Which is
            when Events.Key_Press =>
               Key_Handler (Event_2);  -- same keystrokes work in both windows
            when Events.Exposed =>
               Draw_Data;
            when Events.Resized =>
               Set_Data_View (Event_2.Resize_Data.Width, Event_2.Resize_Data.Height);
            when Events.Close_Window =>
               raise Program_Exit;  -- closing either terminates the app
            when others =>
               null;
         end case;
      end loop;

      -- Now update our "scene"
      if Rotating then
         if Rotation >= Max_Rotation - Increment then
            Rotation := 0.0;
         else
            Rotation := Rotation + Increment;
         end if;
      end if;
      Frame := Frame + 1;
      Draw_Scene;
      Draw_Data;
   end New_Frame;

   ---------------------------------------------------------------------------

begin  -- Multi

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

   -- If other command-line arguments were given then process them
   for Index in 2 .. Ada.Command_Line.Argument_Count loop

      declare
         use Window;
         Arg : String := Ada.Command_Line.Argument (Index);
      begin
         case Arg (Arg'First) is

            when 'a' =>
               Attrs (4) := (Attr_Alpha_Size, Integer'Value (Arg (Arg'First + 1 .. Arg'Last)));

            when 'c' =>
               Attrs (1) := (Attr_Red_Size,   Integer'Value (Arg (Arg'First + 1 .. Arg'Last)));
               Attrs (2) := (Attr_Blue_Size,  Integer'Value (Arg (Arg'First + 1 .. Arg'Last)));
               Attrs (3) := (Attr_Green_Size, Integer'Value (Arg (Arg'First + 1 .. Arg'Last)));

            when 'd' =>
               Attrs (5) := (Attr_Depth_Size, Integer'Value (Arg (Arg'First + 1 .. Arg'Last)));

            when 'n' =>
               Direct := False;

            when others =>
               null;

         end case;
      end;

   end loop;

   -- Create Lumen windows, accepting most defaults
   Window.Create (Scene,
                  Name       => "Two-Window Demo, Scene Window",
                  Width      => Scene_Win_Width,
                  Height     => Scene_Win_Height,
                  Direct     => Direct,
                  Attributes => Attrs,
                  Events     => (Window.Want_Key_Press => True,
                                 Window.Want_Exposure  => True,
                                 others => False));
   Window.Create (Data,
                  Name       => "Two-Window Demo, Data Window",
                  Width      => Data_Win_Width,
                  Height     => Data_Win_Height,
                  Direct     => Direct,
                  Attributes => Attrs,
                  Events     => (Window.Want_Key_Press => True,
                                 Window.Want_Exposure  => True,
                                 others => False));

   -- Set up the viewport and scene parameters
   Window.Make_Current (Scene);
   Set_Scene_View (Scene_Win_Width, Scene_Win_Height);

   -- Make our texture
   for Row in Checks'Range (1) loop
      for Col in Checks'Range (2) loop
         if Row <= Checks'Last (2) / 2 then
            if Col <= Checks'Last (1) / 2 then
               Checks (Row, Col) := White;
            else
               Checks (Row, Col) := Red;
            end if;
         else
            if Col <= Checks'Last (1) / 2 then
               Checks (Row, Col) := Red;
            else
               Checks (Row, Col) := White;
            end if;
         end if;
      end loop;
   end loop;
   GL.GenTextures (1, Check_Tx'Address);
   GL.BindTexture (GL.GL_TEXTURE_2D, Check_Tx);
   GL.TexEnv (GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_MODULATE);
   GL.TexParameter (GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_REPEAT);
   GL.TexParameter (GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_REPEAT);
   GL.TexParameter (GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_NEAREST);
   GL.TexParameter (GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_NEAREST);
   GL.Enable (GL.GL_DEPTH_TEST);
   GL.DepthFunc (GL.GL_LEQUAL);
   GL.TexImage (GL.GL_TEXTURE_2D, 0, GL.GL_RGB, Checks'Last (1), Checks'Last (2), 0,
                GL.GL_RGBA, GL.GL_UNSIGNED_BYTE, Checks'Address);
   GLU.QuadricTexture (Quad, GL.GL_TRUE);

   -- Set up data window
   Window.Make_Current (Data);
   Set_Data_View (Data_Win_Width, Data_Win_Height);
   Object := Font.Txf.Establish_Texture (Tx_Font, 0, True);

   -- Enter the event loop, which is hooked to the scene window
   declare
      use Events;
   begin
      Animate.Select_Events (Win   => Scene,
                             Calls => (Key_Press    => Key_Handler'Unrestricted_Access,
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

end Multi;
