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

procedure Lesson02 is

   The_Window : Lumen.Window.Window_Handle;
   Framerate  : constant := 24;
   Terminated : Boolean := False;

   Program_Exit : Exception;

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
      Perspective(45.0, Long_Float(Width)/Long_Float(Height), 0.1, 100.0);

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
   procedure Resize_Handler (Height : in Integer;
                             Width  : in Integer) is

      H : Natural := Height;

   begin
      -- prevent div by zero
      if Height = 0 then
         H := 1;
      end if;

      Resize_Scene (Width, H);
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

      -- draw triangle
      Begin_Primitive (GL_TRIANGLES);
      begin
         Vertex (Float (0.0),  1.0, 0.0);
         Vertex (Float (-1.0), -1.0, 0.0);
         Vertex (Float (1.0), -1.0, 0.0);
      end;
      End_Primitive;

      -- move to the right half of the screen
      Translate (Float (3.0), 0.0, 0.0);

      -- draw square
      Begin_Primitive (GL_QUADS);
      begin
         Vertex (Float (-1.0),  1.0, 0.0);
         Vertex (Float (1.0),  1.0, 0.0);
         Vertex (Float (1.0), -1.0, 0.0);
         Vertex (Float (-1.0), -1.0, 0.0);
      end;
      End_Primitive;
   end Draw;

   -- Called once per frame; just re-draws the scene
   function Frame_Handler (Frame_Delta : in Duration) return Boolean is
      pragma Unreferenced (Frame_Delta);
   begin
      Draw;
      Lumen.Window.Swap (The_Window);
      return not Terminated;
   end Frame_Handler;

   procedure Key_Handler (Category  : in Lumen.Events.Key_Category;
                          Symbol    : in Lumen.Events.Key_Symbol;
                          Modifiers : in Lumen.Events.Modifier_Set) is
      pragma Unreferenced (Category, Modifiers);
      use Lumen.Events;
   begin
      if Symbol = To_Symbol (Ada.Characters.Latin_1.ESC) then
         Terminated := True;
      end if;
   end Key_Handler;

begin
   Lumen.Window.Create
     (Win    => The_Window,
      Name   => "NeHe Lesson 2",
      Width  => 640,
      Height => 480);
   The_Window.Resize    := Resize_Handler'Unrestricted_Access;
   The_Window.Key_Press := Key_Handler'Unrestricted_Access;


   Resize_Scene (640, 480);
   Init_GL;

   Lumen.Events.Animate.Run (The_Window, 24, Frame_Handler'Unrestricted_Access);

end Lesson02;
