
-- Simple Lumen demo/test program, using earliest incomplete library.  Based
-- on an old SGI demo called "simple", which contained the following
-- BSD-flavored copyright notice:
--
-- (c) Copyright 1993, Silicon Graphics, Inc.
-- ALL RIGHTS RESERVED

with Lumen.Window;
with Lumen.Events; use Lumen.Events;
with Lumen.GL;
with Lumen.GLU;

procedure SGI_Simple is

   ---------------------------------------------------------------------------

   Win     : Lumen.Window.Window_Handle;
   Wide    : Natural := 400;
   High    : Natural := 400;

   ---------------------------------------------------------------------------

   Terminated : Boolean:=False;

   ---------------------------------------------------------------------------

   -- Set or reset the window view parameters
   procedure Set_View (W, H : in Natural) is

      use Lumen;

      Aspect : GL.Double;

   begin  -- Set_View

      -- Viewport dimensions
      GL.Viewport (0, 0, GL.SizeI (W), GL.SizeI (H));

      -- Set up the projection matrix based on the window's shape--wider than
      -- high, or higher than wide
      GL.Matrix_Mode (GL.GL_PROJECTION);
      GL.Load_Identity;

      if W <= H then
         Aspect := GL.Double (H) / GL.Double (W);
         GLU.Ortho_2D (-2.0, 2.0, -2.0 * Aspect, 2.0 * Aspect);
      else
         Aspect := GL.Double (W) / GL.Double (H);
         GLU.Ortho_2D (-2.0 * Aspect, 2.0 * Aspect, -2.0, 2.0);
      end if;
   end Set_View;

   ---------------------------------------------------------------------------

   -- Draw our scene
   procedure Draw is

      use Lumen;

   begin  -- Draw

      GL.Clear_Color (0.0, 0.0, 0.0, 0.0);
      GL.Clear (GL.GL_COLOR_BUFFER_BIT);

      GL.Color (Float (1.0), 1.0, 1.0);

      GL.Begin_Primitive (GL.GL_POLYGON);
      begin
         GL.Vertex (Float (-1.0), -1.0);
         GL.Vertex (Float (-1.0),  1.0);
         GL.Vertex (Float ( 1.0),  1.0);
         GL.Vertex (Float ( 1.0), -1.0);
      end;
      GL.End_Primitive;
      GL.Flush;

      -- Now show it
      Lumen.Window.Swap (Win);

   end Draw;

   ---------------------------------------------------------------------------

   procedure Window_Resize
     (Height : Integer;
      Width  : Integer) is
   begin
      Wide:=Width;
      High:=Height;
      Set_View(Wide,High);
   end Window_Resize;

   procedure Key_Press
     (Category  : Key_Category;
      Symbol    : Key_Symbol;
      Modifiers : Modifier_Set) is
   begin
      Terminated:=True;
   end Key_Press;

   procedure Exposed
     (Top    : Integer;
      Left   : Integer;
      Height : Integer;
      Width  : Integer) is
   begin
      Draw;
   end Exposed;

   ---------------------------------------------------------------------------

begin  -- SGI_Simple

   -- Create Lumen window, accepting most defaults; turn double buffering off
   -- for simplicity
   Lumen.Window.Create (Win, Name   => "Simple Demo",
                        Width  => Wide,
                        Height => High);
   -- Set up the viewport and scene parameters
   Set_View (Wide, High);

   Win.Resize    := Window_Resize'Unrestricted_Access;
   Win.Key_Press := Key_Press'Unrestricted_Access;

   while Lumen.Window.Process_Events(Win) loop
      exit when Terminated;
   end loop;

end SGI_Simple;
