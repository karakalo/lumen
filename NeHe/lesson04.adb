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

procedure Lesson04 is

   The_Window : Lumen.Window.Handle;
   Framerate : constant := 24;

   Triangle_Rotation : Float := 0.0;
   Quad_Rotation     : Float := 0.0;

   Program_Exit : Exception;

   -- simply exit this program
   procedure Quit_Handler (Event : in Lumen.Events.Event_Data) is
   begin
      raise Program_Exit;
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

      -- calculate aspect ratio
      Perspective (45.0, Long_Float (Width) / Long_Float (Height), 0.1, 100.0);

      -- select modelview matrix and reset it
      Matrix_Mode (GL_MODELVIEW);
      Load_Identity;
   end Resize_Scene;

   procedure Init_GL is
      use Lumen.GL;
      use Lumen.GLU;
   begin
      -- smooth shading
      Shade_Model (GL_SMOOTH);

      -- black background
      Clear_Color (0.0, 0.0, 0.0, 0.0);

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
      -- move to the left half of the screen
      Translate (Float (-1.5), 0.0, -6.0);
      -- rotate the Triangle
      Rotate (Triangle_Rotation, 0.0, 1.0, 0.0);
      -- draw triangle
      Begin_Primitive (GL_TRIANGLES);
      begin
         Color (Float (1.0), 0.0, 0.0);
         Vertex (Float (0.0),  1.0, 0.0);
         Color (Float (0.0), 1.0, 0.0);
         Vertex (Float (-1.0), -1.0, 0.0);
         Color (Float (0.0), 0.0, 1.0);
         Vertex (Float (1.0), -1.0, 0.0);
      end;
      End_Primitive;

      -- reset current modelview matrix
      Load_Identity;
      -- move to the right half of the screen
      Translate (Float (1.5), 0.0, -6.0);
      -- rotate the Quad
      Rotate (Quad_Rotation, 1.0, 0.0, 0.0);
      -- draw square
      Color (Float (0.5), 0.5, 1.0);
      Begin_Primitive (GL_QUADS);
      begin
         Vertex (Float (-1.0),  1.0, 0.0);
         Vertex (Float (1.0),  1.0, 0.0);
         Vertex (Float (1.0), -1.0, 0.0);
         Vertex (Float (-1.0), -1.0, 0.0);
      end;
      End_Primitive;

      Triangle_Rotation := Triangle_Rotation + 10.0;
      Quad_Rotation := Quad_Rotation - 7.5;
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
      Name   => "NeHe Lesson 4",
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
end Lesson04;
