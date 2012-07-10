---------------------------------------------------------------------------
--
--  Shows a very simple vertex buffer using the Lumen thin binding
--
---------------------------------------------------------------------------

---------------------------------------------------------------------------
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

with Ada.Characters.Latin_1;
with Ada.Real_Time;

with Lumen.Window;
with Lumen.Events;
with Lumen.GL;

procedure Vertex_Buffers is

   use Lumen;

   -- Keystrokes we care about
   Escape    : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.ESC));
   Letter_q  : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.LC_Q));

   ------------------------------------------------------------------------
   --
   --  This is the vertex buffer ID. It is a simple number that OpenGL
   --  driver uses internally to keep track of the buffer.
   --
   Buffer : Lumen.GL.Uint;

   ------------------------------------------------------------------------
   --
   -- This draws white square in the center of the window. This is
   -- done by drawing a certain number of vertices which we store in
   -- an allocated buffer below.
   --
   procedure Render_Scene is
      use Lumen.GL;
   begin
      Matrix_Mode (GL_MODELVIEW);
      Load_Identity;

      ---------------------------------------------------------------------
      --
      --  This is a very old way of drawing vertex buffers. Modern
      --  vertex buffer operations require writing shaders, which is
      --  not yet implemented.
      --
      Bind_Buffer (GL_ARRAY_BUFFER, Buffer);
      Enable_Client_State (GL_VERTEX_ARRAY);

      ---------------------------------------------------------------------
      --
      --  This tells OpenGL where the vertices are.  The last
      --  parameter is the offset from the beginning of the currently
      --  active vertex buffer.
      --
      Vertex_Pointer (2, GL_FLOAT, 0, 0);

      ---------------------------------------------------------------------
      --
      --  This draws 6 vertices in a row, with triangle mode. This
      --  results in two triangles which form our square.
      --
      Draw_Arrays (GL_TRIANGLES, 0, 6);
      Bind_Buffer (GL_ARRAY_BUFFER, 0);
   end Render_Scene;

   ------------------------------------------------------------------------
   --
   --  This is all that is needed to run a Lumen GL window.
   --
   Win        : Window.Window_Handle;
   Terminated : Boolean := False;

   ------------------------------------------------------------------------
   --
   --  This is the resize handler. It sets the GL viewport to the new
   --  dimensions of the window and sets up a simple orthographic
   --  projection for scene.
   --
   procedure Resize_Scene (Width, Height : in Integer) is
      use Lumen.GL;
      Aspect : Long_Float := Long_Float (Width) / Long_Float (Height);
   begin
      Viewport (0, 0, Width, Height);
      Matrix_Mode (GL_PROJECTION);
      Load_Identity;
      Ortho (-2.0 * Aspect, 2.0 * Aspect, -2.0, 2.0, -1.0, 1.0);
   end Resize_Scene;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for keypresses
   procedure Key_Handler (Category  : in Events.Key_Category;
                          Symbol    : in Events.Key_Symbol;
                          Modifiers : in Events.Modifier_Set) is

      use type Events.Key_Symbol;

   begin  -- Key_Handler
      if Symbol = Escape or Symbol = Letter_q then
         Terminated := True;
      end if;
   end Key_Handler;

   ------------------------------------------------------------------------
   --
   --  This is for real-time rendering.
   --
   Start_Time   : Ada.Real_Time.Time    := Ada.Real_Time.Clock;
   End_Time     : Ada.Real_Time.Time;
   Delta_Time   : Duration;

   ------------------------------------------------------------------------
   --
   --  This is for computing the frame rate every second.
   --
   FPS_Base_Time        : Ada.Real_Time.Time    := Start_Time;
   FPS_Counter          : Integer               := 0;

begin

   ------------------------------------------------------------------------
   --
   --  Creates the window using reasonable defaults and creates and
   --  activates the OpenGL context.
   --
   Lumen.Window.Create (Win, Name => "FPS: 0");
   Win.Resize    := Resize_Scene'Unrestricted_Access;
   Win.Key_Press := Key_Handler'Unrestricted_Access;

   ------------------------------------------------------------------------
   --
   --  This triggers a resize event which doesn't happen automatically
   --  on start-up.
   --
   Resize_Scene (Window.Width (Win),
                 Window.Height (Win));


   ------------------------------------------------------------------------
   --
   --  This creates the vertex buffer object and allocates video
   --  memory for it by filling it with data.
   --
   declare
      use Lumen.GL;
      Vertices : array (1 .. 12) of Float;
   begin

      ---------------------------------------------------------------------
      --
      --  This generates a new valid buffer ID.
      --
      Gen_Buffers (1, Buffer'Address);

      ---------------------------------------------------------------------
      --
      --  This binds the buffer object as the current vertex buffer.
      --
      Bind_Buffer (GL_ARRAY_BUFFER, Buffer);

      ---------------------------------------------------------------------
      --
      --  These are 6 2D vertices stored in an array so we have
      --  consecutive vertex data for OpenGL. The layout is (x1, y1,
      --  x2, y2, ...).
      --
      Vertices := (-1.0, -1.0, 1.0, -1.0, -1.0, 1.0,
                   1.0, 1.0, -1.0, 1.0, 1.0, -1.0);

      ---------------------------------------------------------------------
      --
      --  This writes data into the buffer in. Memory (in video RAM)
      --  is allocated by OpenGL as necessary. The last parameter
      --  tells OpenGL to optimize the buffer for reading.
      --
      Buffer_Data (GL_ARRAY_BUFFER,
                   12 * Float'Size,
                   Vertices'Address,
                   GL_STATIC_DRAW);

      ---------------------------------------------------------------------
      --
      --  This unbinds the vertex buffer. This is not necessary here
      --  because we don't call any draw operations besides drawing
      --  this one vertex buffer. But it is usually a good idea to
      --  unbind a buffer wenn done.
      --
      Bind_Buffer (GL_ARRAY_BUFFER, 0);
   end;

   ------------------------------------------------------------------------
   --
   --  This is the main application loop.
   --
   while Lumen.Window.Process_Events (Win) loop
      exit when Terminated;

      declare
         use Ada.Real_Time;
      begin

         ------------------------------------------------------------------
         --
         --  This computes the delta time, i.e. the time the last frame
         --  took to complete.
         --
         End_Time := Clock;
         Delta_Time := To_Duration (End_Time - Start_Time);
         Start_Time := End_Time;

         ------------------------------------------------------------------
         --
         --  This counts how many frames are rendered in one
         --  second. When one second is over the frames-per-second
         --  value is displayed as the window name (usually in the
         --  title bar) and the counter is reset.
         --
         if To_Duration (Start_Time - FPS_Base_Time) >= 1.0 then
            Lumen.Window.Set_Names (Win,
                                    Name => "FPS: "
                                      & Integer'Image (FPS_Counter));
            FPS_Base_Time := Start_Time;
            FPS_Counter := 0;
         else
            FPS_Counter := FPS_Counter + 1;
         end if;
      end;

      ---------------------------------------------------------------------
      --
      --  This calls the main rendering function
      --
      declare
         use Lumen;
      begin
         GL.Clear_Color (0.0, 0.0, 0.0, 1.0);
         GL.Clear (GL.GL_COLOR_BUFFER_BIT);
         Render_Scene;
         GL.Flush;
      end;

      ---------------------------------------------------------------------
      --
      --  This swaps the double buffer and is usually the last thing
      --  in an animation loop.
      --
      Lumen.Window.Swap (Win);
   end loop;
end Vertex_Buffers;
