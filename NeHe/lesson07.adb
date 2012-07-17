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

   The_Window : Lumen.Window.Window_Handle;
   Framerate  : constant := 24;
   Terminated : Boolean := False;

   X_Rotation, Y_Rotation : Float := 0.0;
   X_Speed, Y_Speed       : Float := 0.0;
   Z_Position             : Float := -5.0;

   Light_Ambient  : constant Lumen.GL.Float_Params := (0.5, 0.5, 0.5, 1.0);
   Light_Diffuse  : constant Lumen.GL.Float_Params := (1.0, 1.0, 1.0, 1.0);
   Light_Position : constant Lumen.GL.Float_Params := (0.0, 0.0, 2.0, 1.0);

   Textures         : Lumen.GL.UInts_3;
   Selected_Texture : Positive := Textures'First;
   Light_Enabled    : Boolean  := False;

   -- Resize the scene
   procedure Resize_Scene (Height,Width : in Natural) is
      use Lumen.GL;
      use Lumen.GLU;
   begin
      -- reset current viewport
      Viewport (0, 0, Width, Height);

      -- select projection matrix and reset it
      Matrix_Mode (GL_PROJECTION);
      Load_Identity;

      -- calculate aspect ratio
      Perspective (45.0, Double (Width) / Double (Height), 0.1, 100.0);

      -- select modelview matrix and reset it
      Matrix_Mode (GL_MODELVIEW);
   end Resize_Scene;

   procedure Load_GL_Textures is
      use Lumen.GL;
      use Lumen.GLU;
      IP              : Pointer;
      Image           : constant Lumen.Image.Descriptor :=
         Lumen.Image.From_File ("data/Crate.bmp");
      Texture_Pointer : constant System.Address := Textures'Address;
   begin
      -- create three textures
      Gen_Textures (3, Texture_Pointer);

      -- Bind texture operations to the newly-created texture name
      Bind_Texture (GL_TEXTURE_2D, Textures (Textures'First));
      Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      IP := Image.Values.all'Address;
      Tex_Image (GL_TEXTURE_2D, 0, GL_RGB, Image.Width, Image.Height,
                0, GL_RGBA, GL_UNSIGNED_BYTE, IP);

      -- Bind texture operations to the newly-created texture name
      Bind_Texture (GL_TEXTURE_2D, Textures (Textures'First + 1));
      Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      IP := Image.Values.all'Address;
      Tex_Image (GL_TEXTURE_2D, 0, GL_RGB, Image.Width, Image.Height,
                0, GL_RGBA, GL_UNSIGNED_BYTE, IP);

      -- Bind texture operations to the newly-created texture name
      Bind_Texture (GL_TEXTURE_2D, Textures (Textures'First + 2));
      Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR_MIPMAP_NEAREST);
      IP := Image.Values.all'Address;
      declare
         X : constant Int :=
               Build_2D_Mipmaps (GL_TEXTURE_2D, Int (GL_RGB), Image.Width, Image.Height,
                               GL_RGBA, GL_UNSIGNED_BYTE, IP);
         pragma Unreferenced (X);
      begin
         null;
      end;
   end Load_GL_Textures;

   procedure Init_GL is
      use Lumen.GL;
   begin
      -- load textures (new)
      Load_GL_Textures;
      -- enable texture mapping (new)
      Enable (GL_TEXTURE_2D);
      -- smooth shading
      Shade_Model (GL_SMOOTH);

      -- black background
      Clear_Color (0.0, 0.0, 0.0, 0.5);

      -- depth buffer setup
      Clear_Depth (1.0);
      -- enable depth testing
      Enable (GL_DEPTH_TEST);
      -- type of depth test
      Depth_Func (GL_LEQUAL);

      Hint (GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

      -- set up lighting
      Light (GL_LIGHT1, GL_AMBIENT, Light_Ambient);
      Light (GL_LIGHT1, GL_DIFFUSE, Light_Diffuse);
      Light (GL_LIGHT1, GL_POSITION, Light_Position);
      Enable (GL_LIGHT1);
   end Init_GL;

   -- Resize and Initialize the GL window
   procedure Resize_Handler (Height : in Integer;
                             Width  : in Integer) is

      H : Natural := Height;

   begin
      -- prevent div by zero
      if Height = 0 then
         H := 1;
      end if;

      Resize_Scene (Width, H);
   end Resize_Handler;

   procedure Draw is
      use Lumen.GL;
      use type Bitfield;
   begin
      -- clear screen and depth buffer
      Clear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      -- reset current modelview matrix
      Load_Identity;
      -- move into the screen 5 units
      Translate (0.0, 0.0, Z_Position);

      -- rotate on the X axis
      Rotate (X_Rotation, 1.0, 0.0, 0.0);
      -- rotate on the Y axis
      Rotate (Y_Rotation, 0.0, 1.0, 0.0);

      -- select the texture
      Bind_Texture (GL_TEXTURE_2D, Textures (Selected_Texture));

      -- draw square
      Begin_Primitive (GL_QUADS);
      begin
         -- front
         Normal (Float (0.0), 0.0, 1.0);
         Tex_Coord (Float (0.0), 0.0); Vertex (Float (-1.0), -1.0, 1.0);
         Tex_Coord (Float (0.0), 1.0); Vertex (Float (1.0), -1.0, 1.0);
         Tex_Coord (Float (1.0), 1.0); Vertex (Float (1.0), 1.0, 1.0);
         Tex_Coord (Float (1.0), 0.0); Vertex (Float (-1.0), 1.0, 1.0);
         -- back
         Normal (Float (0.0), 0.0, -1.0);
         Tex_Coord (Float (0.0), 0.0); Vertex (Float (1.0), -1.0, -1.0);
         Tex_Coord (Float (0.0), 1.0); Vertex (Float (-1.0), -1.0, -1.0);
         Tex_Coord (Float (1.0), 1.0); Vertex (Float (-1.0), 1.0, -1.0);
         Tex_Coord (Float (1.0), 0.0); Vertex (Float (1.0), 1.0, -1.0);
         -- top
         Normal (Float (0.0), 1.0, 0.0);
         Tex_Coord (Float (0.0), 0.0); Vertex (Float (-1.0), 1.0, 1.0);
         Tex_Coord (Float (0.0), 1.0); Vertex (Float (1.0), 1.0, 1.0);
         Tex_Coord (Float (1.0), 1.0); Vertex (Float (1.0), 1.0, -1.0);
         Tex_Coord (Float (1.0), 0.0); Vertex (Float (-1.0), 1.0, -1.0);
         -- bottom
         Normal (Float (0.0), -1.0, 0.0);
         Tex_Coord (Float (0.0), 0.0); Vertex (Float (1.0), -1.0, 1.0);
         Tex_Coord (Float (0.0), 1.0); Vertex (Float (-1.0), -1.0, 1.0);
         Tex_Coord (Float (1.0), 1.0); Vertex (Float (-1.0), -1.0, -1.0);
         Tex_Coord (Float (1.0), 0.0); Vertex (Float (1.0), -1.0, -1.0);
         -- right
         Normal (Float (1.0), 0.0, 0.0);
         Tex_Coord (Float (0.0), 0.0); Vertex (Float (1.0), -1.0, 1.0);
         Tex_Coord (Float (0.0), 1.0); Vertex (Float (1.0), -1.0, -1.0);
         Tex_Coord (Float (1.0), 1.0); Vertex (Float (1.0), 1.0, -1.0);
         Tex_Coord (Float (1.0), 0.0); Vertex (Float (1.0), 1.0, 1.0);
         -- left
         Normal (Float (-1.0), 0.0, 0.0);
         Tex_Coord (Float (0.0), 0.0); Vertex (Float (-1.0), -1.0, -1.0);
         Tex_Coord (Float (0.0), 1.0); Vertex (Float (-1.0), -1.0, 1.0);
         Tex_Coord (Float (1.0), 1.0); Vertex (Float (-1.0), 1.0, 1.0);
         Tex_Coord (Float (1.0), 0.0); Vertex (Float (-1.0), 1.0, -1.0);
      end;
      End_Primitive;

      X_Rotation := X_Rotation + X_Speed;
      Y_Rotation := Y_Rotation + Y_Speed;
   end Draw;

   -- Called once per frame; just re-draws the scene
   function Frame_Handler (Frame_Delta : in Duration) return Boolean is
      pragma Unreferenced (Frame_Delta);
   begin
      Draw;
      Lumen.GL.Finish;
      Lumen.Window.Swap (The_Window);
      return not Terminated;
   end Frame_Handler;

   procedure Key_Handler (Category  : in Lumen.Events.Key_Category;
                          Symbol    : in Lumen.Events.Key_Symbol;
                          Modifiers : in Lumen.Events.Modifier_Set) is
      pragma Unreferenced (Category, Modifiers);
      use Lumen.Events;
   begin
      case Symbol is
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
            if Symbol = To_Symbol (Ada.Characters.Latin_1.ESC) then
               -- Escape: quit
               Terminated := True;
            elsif Symbol = To_Symbol ('l') then
               -- toggle lighting
               Light_Enabled := not Light_Enabled;
               if Light_Enabled then
                  Lumen.GL.Enable (Lumen.GL.GL_LIGHTING);
               else
                  Lumen.GL.Disable (Lumen.GL.GL_LIGHTING);
               end if;
            elsif Symbol = To_Symbol ('t') then
               -- cycle textures
               Selected_Texture := Selected_Texture + 1;
               if Selected_Texture > Textures'Last then
                  Selected_Texture := Textures'First;
               end if;
            end if;
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
      Height => 480);
   The_Window.Resize    := Resize_Handler'Unrestricted_Access;
   The_Window.Key_Press := Key_Handler'Unrestricted_Access;

   Resize_Scene (480, 640);
   Init_GL;

   Lumen.Events.Animate.Run (The_Window, 24, Frame_Handler'Unrestricted_Access);
end Lesson07;
