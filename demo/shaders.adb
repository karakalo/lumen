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

with Lumen.Window;
with Lumen.Events;
with Lumen.GL;
with Lumen.Binary;

procedure Shaders is

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
   Window               : Lumen.Window.Handle;

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
      Lumen.Window.Swap (Window);
   end Render_Scene;

   ------------------------------------------------------------------------
   --
   --  This sets the viewport and re-renders the scene. We don't touch
   --  the projection matrix so it is left as identity.
   --
   procedure Resize_Scene (Width, Height : Natural) is
   begin
      Lumen.GL.Viewport (0, 0, Width, Height);
      Render_Scene;
   end Resize_Scene;

   ------------------------------------------------------------------------
   --
   --  This is the event handler for the main loop. The loop is exited
   --  when the window is closed or a key is pressed, which terminates
   --  the program.
   --
   procedure Event_Handler (Event : Lumen.Events.Event_Data) is
      use Lumen.Window;
      use Lumen.Events;
      use type Lumen.Events.Event_Type;
   begin
      if Event.Which = Close_Window or Event.Which = Key_Press then
         End_Events (Window);
      elsif Event.Which = Resized then
         Resize_Scene (Event.Resize_Data.Width,
                       Event.Resize_Data.Height);
      end if;
   end Event_Handler;
begin
   Lumen.Window.Create (Window,
                        Name => "Minimal Shader Demo",
                        Events => (Lumen.Window.Want_Key_Press => True,
                                   Others => False));

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
      Shader_Program    := Create_Program;

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

   Resize_Scene (Lumen.Window.Width (Window),
                 Lumen.Window.Height (Window));

   ------------------------------------------------------------------------
   --
   --  This enters the main loop, using the event handler procedure
   --  defined above as event callback.
   --
   Lumen.Events.Receive_Events (Window,
                                Event_Handler'Unrestricted_Access);
end Shaders;
