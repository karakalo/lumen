---------------------------------------------------------------------------
--
--  A minimal example of vertex and fragment shaders
--
---------------------------------------------------------------------------

---------------------------------------------------------------------------
--
--  Copyright (c) 2011, David Bouchain <david@bouchain.de>
--
--  Permission to use, copy, modify, and/or distribute this software
--  for any purpose with or without fee is hereby granted, provided
--  that the above copyright notice and this permission notice appear
--  in all copies.
--
--  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
--  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
--  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
--  AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
--  CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
--  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
--  NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
--  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--
---------------------------------------------------------------------------

---------------------------------------------------------------------------
--
--  This program requires OpenGL 2.0 or later.
--
--  The expected output is a blue quad in the center of the window
--  with the same proportions as the window. If the window is
--  completely white, something went wrong with the shaders.
--
---------------------------------------------------------------------------

with System;
with Ada.Characters.Latin_1;

with Lumen.Window;
with Lumen.Events;
with Lumen.Events.Animate;
with Lumen.GL;
with Lumen.Binary;

with Ada.Text_IO; use Ada.Text_IO;

procedure Shaders is

   -- Keystrokes we care about
   Escape    : constant Lumen.Events.Key_Symbol := Lumen.Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.ESC));
   Letter_q  : constant Lumen.Events.Key_Symbol := Lumen.Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.LC_Q));

   ------------------------------------------------------------------------
   --
   --  These are the 'names' of the vertex shader, the fragment
   --  shader, and the shader program, respectively. They serve as
   --  handles to the internal objects in the OpenGL implementation.
   --
   Vertex_Shader        : Lumen.GL.Uint;
   Fragment_Shader      : Lumen.GL.Uint;
   Shader_Program       : Lumen.GL.Uint;

   ------------------------------------------------------------------------
   --
   --  This is the window handle of our application window.
   --
   Win : Lumen.Window.Window_Handle;

   -- The master event-loop flag
   Terminated : Boolean := False;

   ------------------------------------------------------------------------
   --
   --  This draws a quad that would normally cover the whole
   --  window. It is set to be drawn in white. However, the vertex
   --  shader scales by 0.5 and the fragment shader outputs a constant
   --  color, so the result is a smaller quad in blue.
   --
   procedure Render_Scene is
      use Lumen.GL;
   begin
      Clear_Color (0.0, 0.0, 0.0, 1.0);
      Clear (GL_COLOR_BUFFER_BIT);
      Matrix_Mode (GL_MODELVIEW);
      Load_Identity;
      Color (Float (1.0), 1.0, 1.0, 1.0);
      Begin_Primitive (GL_QUADS);
      Vertex (Float (-1.0), -1.0);
      Vertex (Float (1.0), -1.0);
      Vertex (Float (1.0), 1.0);
      Vertex (Float (-1.0), 1.0);
      End_Primitive;
      Lumen.GL.Finish;
      Lumen.Window.Swap (Win);
   end Render_Scene;

   ------------------------------------------------------------------------
   --
   --  This sets the viewport and re-renders the scene. We don't touch
   --  the projection matrix so it is left as identity.
   --
   procedure Resize_Scene (Height,Width : in Integer) is
   begin
      Lumen.GL.Viewport (0, 0, Width, Height);
      Render_Scene;
   end Resize_Scene;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for keypresses
   procedure Key_Handler (Category  : in Lumen.Events.Key_Category;
                          Symbol    : in Lumen.Events.Key_Symbol;
                          Modifiers : in Lumen.Events.Modifier_Set) is

      use type Lumen.Events.Key_Symbol;

   begin  -- Key_Handler
      if Symbol = Escape or Symbol = Letter_q then
         Terminated := True;
      end if;
   end Key_Handler;

   ----------------------------------------------------------------------------
   -- Called once per frame; just re-draws the scene
   function New_Frame (Frame_Delta : in Duration) return Boolean is
   begin  -- New_Frame
      Render_Scene;
      return not Terminated;
   end New_Frame;


begin
   Lumen.Window.Create (Win, Name => "Minimal Shader Demo");

   Win.Resize     := Resize_Scene'Unrestricted_Access;
   Win.Key_Press  := Key_Handler'Unrestricted_Access;

   if not Lumen.GL.Load_GL_2_0 then
      Put_Line("OpenGL 2.0 functions missing");
      return;
   end if;

   declare
      use Lumen.GL;
      use type Lumen.Binary.Word;

      ---------------------------------------------------------------------
      --
      --  These are the shader source codes which OpenGL requires as a
      --  set of strings. The pointers are required because OpenGL
      --  expects a pointer to a pointer. This is cumbersome to do in
      --  Ada and there should really be a minimal wrapper function,
      --  even in the thin binding.
      --
      Vertex_Shader_Source : String :=
        "attribute vec4 in_vertex;" &
        "void main() {" &
        "    gl_Position = 0.5*in_vertex;" &
        "    gl_Position.w = 1.0;" &
        "}" & Character'Val (0);
      Vertex_Shader_Source_Pointer : Pointer :=
        Vertex_Shader_Source'Address;
      Fragment_Shader_Source : String :=
        "void main() {" &
        "    gl_FragColor = vec4(0.0, 0.0, 0.75, 1.0);" &
        "}" & Character'Val (0);
      Fragment_Shader_Source_Pointer : Pointer :=
        Fragment_Shader_Source'Address;
   begin

      ---------------------------------------------------------------------
      --
      --  This creates the two new shader objects, one for the vertex
      --  shader and one for the fragment shader.
      --
      Vertex_Shader     := Create_Shader (GL_VERTEX_SHADER);
      Fragment_Shader   := Create_Shader (GL_FRAGMENT_SHADER);

      ---------------------------------------------------------------------
      --
      --  This sends the source code strings to the GLSL compiler
      --  which resides in the OpenGL driver.
      --
      Shader_Source (Vertex_Shader, 1,
                     Vertex_Shader_Source_Pointer'Address,
                     System'To_Address (0));
      Shader_Source (Fragment_Shader, 1,
                     Fragment_Shader_Source_Pointer'Address,
                     System'To_Address (0));

      ---------------------------------------------------------------------
      --
      --  This compiles the shaders into GPU machine code. Error
      --  checks have been omitted for simplicity here. Whether or not
      --  the compiling was successful can be queried by
      --  Get_Error. Textual error messages can be retrieved through
      --  Get_Shader_Info_Log.
      --
      Compile_Shader (Vertex_Shader);
      Compile_Shader (Fragment_Shader);

      ---------------------------------------------------------------------
      --
      --  This creates the program object.
      --
      Shader_Program    := Create_Program.all;

      ---------------------------------------------------------------------
      --
      --  This assigns the 'shader object code', which was generated by
      --  compiling the sources, to the shader program.
      --
      Attach_Shader (Shader_Program, Vertex_Shader);
      Attach_Shader (Shader_Program, Fragment_Shader);

      ---------------------------------------------------------------------
      --
      --  This links the shaders to create a complete GPU program.
      --
      Link_Program (Shader_Program);

      ---------------------------------------------------------------------
      --
      --  This sends the program to the GPU so it is used in the
      --  OpenGL pipeline.
      --
      Use_Program (Shader_Program);
   end;

   Resize_Scene (Lumen.Window.Height (Win),
                 Lumen.Window.Width (Win));

   ------------------------------------------------------------------------
   --
   --  This enters the main loop, using the resize handler and frame procedures
   --  defined above as event callbacks.
   --
   Lumen.Events.Animate.Run (Win, 24, New_Frame'Unrestricted_Access);

end Shaders;
