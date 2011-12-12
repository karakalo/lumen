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
with Lumen.GL;
with Lumen.GLU;
with Lumen.Image;
with System;

procedure Lesson06 is

   The_Window : Lumen.Window.Handle;
   Framerate : constant := 24;

   X_Rotation, Y_Rotation, Z_Rotation : Float := 0.0;
   Z_Position : constant Float := -5.0;
   The_Texture : Lumen.GL.UInt;

   -- simply exit this program
   procedure Quit_Handler (Event : in Lumen.Events.Event_Data) is
      pragma Unreferenced (Event);
   begin
      Lumen.Events.End_Events (The_Window);
   end;

   -- Resize the scene
   procedure Resize_Scene (Width, Height : in Natural) is
      use Lumen.GL;
      use Lumen.GLU;
   begin

      -- reset current viewport
      Viewport (0, 0, Width, Height);

      -- select projection matrix and reset it
      Matrix_Mode (GL_PROJECTION);
      Load_Identity;

      Perspective (45.0, Long_Float (Width) / Long_Float (Height), 0.1, 100.0);

      -- select modelview matrix and reset it
      Matrix_Mode (GL_MODELVIEW);
   end Resize_Scene;

   procedure Load_GL_Textures is
      use Lumen.GL;
      IP              : Pointer;
      Image           : constant Lumen.Image.Descriptor := Lumen.Image.From_File ("data/NeHe.bmp");
      Texture_Pointer : constant System.Address := The_Texture'Address;
   begin
      -- Allocate a texture name
      Gen_Textures (1, Texture_Pointer);

      -- Bind texture operations to the newly-created texture name
      Bind_Texture (GL_TEXTURE_2D, The_Texture);

      -- Select modulate to mix texture with color for shading
      Tex_Env (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

      -- Wrap textures at both edges
      Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

      -- How the texture behaves when minified and magnified
      Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

      -- Create a pointer to the image.  This sort of horror show is going to
      -- be disappearing once Lumen includes its own OpenGL bindings.
      IP := Image.Values.all'Address;

      -- Build our texture from the image we loaded earlier
      Tex_Image (GL_TEXTURE_2D, 0, GL_RGBA, Image.Width, Image.Height, 0,
                GL_RGBA, GL_UNSIGNED_BYTE, IP);
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
   end Init_GL;

   -- Resize and Initialize the GL window
   procedure Resize_Handler (Event : in Lumen.Events.Event_Data) is
      Height : Natural := Event.Resize_Data.Height;
      Width  : constant Natural := Event.Resize_Data.Width;
   begin
      -- prevent div by zero
      if Height = 0 then
         Height := 1;
      end if;

      Resize_Scene (Width, Height);
   end;

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
      -- rotate on the Z axis
      Rotate (Z_Rotation, 0.0, 0.0, 1.0);

      -- select the texture
      Bind_Texture (GL_TEXTURE_2D, The_Texture);

      -- draw square
      Begin_Primitive (GL_QUADS);
         -- front
      Tex_Coord (Float (0.0), 0.0); Vertex (Float (-1.0), -1.0,  1.0);
      Tex_Coord (Float (0.0), 1.0); Vertex (Float (1.0), -1.0,  1.0);
      Tex_Coord (Float (1.0), 1.0); Vertex (Float (1.0),  1.0,  1.0);
      Tex_Coord (Float (1.0), 0.0); Vertex (Float (-1.0),  1.0,  1.0);
         -- back
      Tex_Coord (Float (0.0), 0.0); Vertex (Float (1.0), -1.0, -1.0);
      Tex_Coord (Float (0.0), 1.0); Vertex (Float (-1.0), -1.0, -1.0);
      Tex_Coord (Float (1.0), 1.0); Vertex (Float (-1.0),  1.0, -1.0);
      Tex_Coord (Float (1.0), 0.0); Vertex (Float (1.0),  1.0, -1.0);
         -- top
      Tex_Coord (Float (0.0), 0.0); Vertex (Float (-1.0),  1.0,  1.0);
      Tex_Coord (Float (0.0), 1.0); Vertex (Float (1.0),  1.0,  1.0);
      Tex_Coord (Float (1.0), 1.0); Vertex (Float (1.0),  1.0, -1.0);
      Tex_Coord (Float (1.0), 0.0); Vertex (Float (-1.0),  1.0, -1.0);
         -- bottom
      Tex_Coord (Float (0.0), 0.0); Vertex (Float (1.0), -1.0,  1.0);
      Tex_Coord (Float (0.0), 1.0); Vertex (Float (-1.0), -1.0,  1.0);
      Tex_Coord (Float (1.0), 1.0); Vertex (Float (-1.0), -1.0, -1.0);
      Tex_Coord (Float (1.0), 0.0); Vertex (Float (1.0), -1.0, -1.0);
         -- right
      Tex_Coord (Float (0.0), 0.0); Vertex (Float (1.0), -1.0,  1.0);
      Tex_Coord (Float (0.0), 1.0); Vertex (Float (1.0), -1.0, -1.0);
      Tex_Coord (Float (1.0), 1.0); Vertex (Float (1.0),  1.0, -1.0);
      Tex_Coord (Float (1.0), 0.0); Vertex (Float (1.0),  1.0,  1.0);
         -- left
      Tex_Coord (Float (0.0), 0.0); Vertex (Float (-1.0), -1.0, -1.0);
      Tex_Coord (Float (0.0), 1.0); Vertex (Float (-1.0), -1.0,  1.0);
      Tex_Coord (Float (1.0), 1.0); Vertex (Float (-1.0),  1.0,  1.0);
      Tex_Coord (Float (1.0), 0.0); Vertex (Float (-1.0),  1.0, -1.0);
      End_Primitive;

      X_Rotation := X_Rotation + 1.5;
      Y_Rotation := Y_Rotation + 1.0;
      Z_Rotation := Z_Rotation + 2.0;
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
      if Key_Data.Key = To_Symbol (Ada.Characters.Latin_1.ESC) then
               -- Escape: quit
         End_Events (The_Window);
      end if;
   end Key_Handler;

begin
   Lumen.Window.Create
     (Win    => The_Window,
      Name   => "NeHe Lesson 6",
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
end Lesson06;
