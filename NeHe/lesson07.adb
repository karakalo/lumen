--
-- Copyright (c) 2011 Julian Leyh <julian@vgai.de>
--
-- Permission to use, copy, modify, and distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--

with Ada.Characters.Latin_1;
with Lumen.Window;
with Lumen.Events.Animate;
with Lumen.Events.Keys;
with Lumen.GL;
with Lumen.GLU;
with Lumen.Image;
with System;

procedure Lesson07 is

   The_Window : Lumen.Window.Handle;
   Framerate  : constant := 24;

   X_Rotation, Y_Rotation : Float := 0.0;
   X_Speed, Y_Speed       : Float := 0.0;
   Z_Position             : Float := -5.0;

   Light_Ambient  : constant Lumen.GL.Float_Params := (0.5, 0.5, 0.5, 1.0);
   Light_Diffuse  : constant Lumen.GL.Float_Params := (1.0, 1.0, 1.0, 1.0);
   Light_Position : constant Lumen.GL.Float_Params := (0.0, 0.0, 2.0, 1.0);

   Textures         : Lumen.GL.UInts_3;
   Selected_Texture : Positive := Textures'First;
   Light_Enabled    : Boolean  := False;

   -- simply exit this program
   procedure Quit_Handler (Event : in Lumen.Events.Event_Data) is
      pragma Unreferenced (Event);
   begin
      Lumen.Events.End_Events (The_Window);
   end Quit_Handler;

   -- Resize the scene
   procedure Resize_Scene (Width, Height : in Natural) is
      use Lumen.GL;
      use Lumen.GLU;
   begin
      -- reset current viewport
      Viewport (0, 0, Width, Height);

      -- select projection matrix and reset it
      MatrixMode (GL_PROJECTION);
      LoadIdentity;

      -- calculate aspect ratio
      Perspective (45.0, Double (Width) / Double (Height), 0.1, 100.0);

      -- select modelview matrix and reset it
      MatrixMode (GL_MODELVIEW);
   end Resize_Scene;

   procedure Load_GL_Textures is
      use Lumen.GL;
      IP              : Pointer;
      Image           : constant Lumen.Image.Descriptor :=
        Lumen.Image.From_File ("data/Crate.bmp");
      Texture_Pointer : constant System.Address := Textures'Address;
   begin
      -- Allocate a texture name
      GenTextures (1, Texture_Pointer);

      -- Bind texture operations to the newly-created texture name
      BindTexture (GL_TEXTURE_2D, Textures (Selected_Texture));

      -- Select modulate to mix texture with color for shading
      TexEnv (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

      -- Wrap textures at both edges
      TexParameter (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      TexParameter (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

      -- How the texture behaves when minified and magnified
      TexParameter (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      TexParameter (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

      -- Create a pointer to the image.  This sort of horror show is going to
      -- be disappearing once Lumen includes its own OpenGL bindings.
      IP := Image.Values.all'Address;

      -- Build our texture from the image we loaded earlier
      TexImage (GL_TEXTURE_2D, 0, GL_RGBA, Image.Width, Image.Height,
                0, GL_RGBA, GL_UNSIGNED_BYTE, IP);
   end Load_GL_Textures;

   procedure Init_GL is
      use Lumen.GL;
   begin
      -- load textures (new)
      Load_GL_Textures;
      -- enable texture mapping (new)
      Enable (GL_TEXTURE_2D);
      -- smooth shading
      ShadeModel (GL_SMOOTH);

      -- black background
      ClearColor (0.0, 0.0, 0.0, 0.5);

      -- depth buffer setup
      ClearDepth (1.0);
      -- enable depth testing
      Enable (GL_DEPTH_TEST);
      -- type of depth test
      DepthFunc (GL_LEQUAL);

      Hint (GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

      -- set up lighting
      Light (GL_LIGHT1, GL_AMBIENT, Light_Ambient);
      Light (GL_LIGHT1, GL_DIFFUSE, Light_Diffuse);
      Light (GL_LIGHT1, GL_POSITION, Light_Position);
      Enable (GL_LIGHT1);
   end Init_GL;

   -- Resize and Initialize the GL window
   procedure Resize_Handler (Event : in Lumen.Events.Event_Data) is
      Height : Natural          := Event.Resize_Data.Height;
      Width  : constant Natural := Event.Resize_Data.Width;
   begin
      -- prevent div by zero
      if Height = 0 then
         Height := 1;
      end if;

      Resize_Scene (Width, Height);
   end Resize_Handler;

   procedure Draw is
      use Lumen.GL;
      use type Bitfield;
   begin
      -- clear screen and depth buffer
      Clear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      -- reset current modelview matrix
      LoadIdentity;
      -- move into the screen 5 units
      Translate (0.0, 0.0, Z_Position);

      -- rotate on the X axis
      Rotate (X_Rotation, 1.0, 0.0, 0.0);
      -- rotate on the Y axis
      Rotate (Y_Rotation, 0.0, 1.0, 0.0);

      -- select the texture
      BindTexture (GL_TEXTURE_2D, Textures (Selected_Texture));

      -- draw square
      glBegin (GL_QUADS);
      begin
         -- front
         Normal (Float (0.0), 0.0, 1.0);
         TexCoord (Float (0.0), 0.0); Vertex (Float (-1.0), -1.0, 1.0);
         TexCoord (Float (0.0), 1.0); Vertex (Float (1.0), -1.0, 1.0);
         TexCoord (Float (1.0), 1.0); Vertex (Float (1.0), 1.0, 1.0);
         TexCoord (Float (1.0), 0.0); Vertex (Float (-1.0), 1.0, 1.0);
         -- back
         Normal (Float (0.0), 0.0, -1.0);
         TexCoord (Float (0.0), 0.0); Vertex (Float (1.0), -1.0, -1.0);
         TexCoord (Float (0.0), 1.0); Vertex (Float (-1.0), -1.0, -1.0);
         TexCoord (Float (1.0), 1.0); Vertex (Float (-1.0), 1.0, -1.0);
         TexCoord (Float (1.0), 0.0); Vertex (Float (1.0), 1.0, -1.0);
         -- top
         Normal (Float (0.0), 1.0, 0.0);
         TexCoord (Float (0.0), 0.0); Vertex (Float (-1.0), 1.0, 1.0);
         TexCoord (Float (0.0), 1.0); Vertex (Float (1.0), 1.0, 1.0);
         TexCoord (Float (1.0), 1.0); Vertex (Float (1.0), 1.0, -1.0);
         TexCoord (Float (1.0), 0.0); Vertex (Float (-1.0), 1.0, -1.0);
         -- bottom
         Normal (Float (0.0), -1.0, 0.0);
         TexCoord (Float (0.0), 0.0); Vertex (Float (1.0), -1.0, 1.0);
         TexCoord (Float (0.0), 1.0); Vertex (Float (-1.0), -1.0, 1.0);
         TexCoord (Float (1.0), 1.0); Vertex (Float (-1.0), -1.0, -1.0);
         TexCoord (Float (1.0), 0.0); Vertex (Float (1.0), -1.0, -1.0);
         -- right
         Normal (Float (1.0), 0.0, 0.0);
         TexCoord (Float (0.0), 0.0); Vertex (Float (1.0), -1.0, 1.0);
         TexCoord (Float (0.0), 1.0); Vertex (Float (1.0), -1.0, -1.0);
         TexCoord (Float (1.0), 1.0); Vertex (Float (1.0), 1.0, -1.0);
         TexCoord (Float (1.0), 0.0); Vertex (Float (1.0), 1.0, 1.0);
         -- left
         Normal (Float (-1.0), 0.0, 0.0);
         TexCoord (Float (0.0), 0.0); Vertex (Float (-1.0), -1.0, -1.0);
         TexCoord (Float (0.0), 1.0); Vertex (Float (-1.0), -1.0, 1.0);
         TexCoord (Float (1.0), 1.0); Vertex (Float (-1.0), 1.0, 1.0);
         TexCoord (Float (1.0), 0.0); Vertex (Float (-1.0), 1.0, -1.0);
      end;
      glEnd;

      X_Rotation := X_Rotation + X_Speed;
      Y_Rotation := Y_Rotation + Y_Speed;
   end Draw;

   procedure Frame_Handler (Frame_Delta : in Duration) is
      pragma Unreferenced (Frame_Delta);
   begin
      Draw;
      Lumen.Window.Swap (The_Window);
   end Frame_Handler;

   procedure Key_Handler (Event : in Lumen.Events.Event_Data) is
      use Lumen.Events;
      Key_Data : Key_Event_Data renames Event.Key_Data;
   begin
      case Key_Data.Key is
         when Keys.Page_Up =>
            -- zoom in
            Z_Position := Z_Position - 0.2;
         when Keys.Page_Down =>
            -- zoom out
            Z_Position := Z_Position + 0.2;

         when Keys.Up =>
            -- Up: decrease X_Speed
            X_Speed := X_Speed - 0.1;
         when Keys.Down =>
            -- Down: increase X_Speed
            X_Speed := X_Speed + 0.1;
         when Keys.Left =>
            -- Left: decrease Y_Speed
            Y_Speed := Y_Speed - 0.1;
         when Keys.Right =>
            -- Right: increase Y_Speed
            Y_Speed := Y_Speed + 0.1;

         when others =>
            if Key_Data.Key = To_Symbol (Ada.Characters.Latin_1.ESC) then
               -- Escape: quit
               End_Events (The_Window);
            end if;
            -- check character code
            declare
               Key_Char : constant Character := To_Character (Key_Data.Key);
            begin
               case Key_Char is
                  when 'l' =>
                     -- toggle lighting
                     Light_Enabled := not Light_Enabled;
                     if Light_Enabled then
                        Lumen.GL.Enable (Lumen.GL.GL_LIGHTING);
                     else
                        Lumen.GL.Disable (Lumen.GL.GL_LIGHTING);
                     end if;

                  when 't' =>
                     -- cycle textures
                     Selected_Texture := Selected_Texture + 1;
                     if Selected_Texture > Textures'Last then
                        Selected_Texture := Textures'First;
                     end if;
                  when others =>
                     null;
               end case;
            end;
      end case;
   exception
      when Not_Character =>
         null;
   end Key_Handler;

begin
   Lumen.Window.Create
     (Win    => The_Window,
      Name   => "NeHe Lesson 7",
      Width  => 640,
      Height => 480,
      Events => (Lumen.Window.Want_Key_Press => True,
                 Lumen.Window.Want_Exposure  => True,
                 others                      => False));

   Resize_Scene (640, 480);
   Init_GL;

   Lumen.Events.Animate.Select_Events
     (Win   => The_Window,
      FPS   => Framerate,
      Frame => Frame_Handler'Unrestricted_Access,
      Calls =>
        (Lumen.Events.Resized      => Resize_Handler'Unrestricted_Access,
         Lumen.Events.Close_Window => Quit_Handler'Unrestricted_Access,
         Lumen.Events.Key_Press    => Key_Handler'Unrestricted_Access,
         others                    => Lumen.Events.No_Callback));

   Lumen.Window.Destroy_Context (The_Window);
   Lumen.Window.Destroy (The_Window);
end Lesson07;
