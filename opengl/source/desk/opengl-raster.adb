
package body opengl.Raster
--
--
--
is
   use GL;      -- glu.Binding;


   procedure set_window_pos (x, y : in Real;
                             z    : in Real := 0.0;
                             w    : in Real := 1.0)
   is
      pragma Unreferenced (z, w);
      use type glInt, Real;

      x1 : Real := x;
      y1 : Real := y;

      fx, fy : GLfloat;
   begin

--        -- Push current matrix mode and viewport attributes
--        --
--        glPushAttrib (GL_TRANSFORM_BIT or GL_VIEWPORT_BIT);
--
--
--        -- Setup projection parameters
--        --
--        glMatrixMode (GL_PROJECTION);
--        glPushMatrix;
--        glLoadIdentity;
--
--        glMatrixMode (GL_MODELVIEW);
--        glPushMatrix;
--        glLoadIdentity;
--
--
--        glDepthRange (glDouble (z), glDouble (z));
--
--        X1 := Real'Min (X, Real (glInt'Last) - 100.0);
--        y1 := Real'Min (y, Real (glInt'Last) - 100.0);
--
--        X1 := Real'Max (X1, Real (glInt'First) + 100.0);
--        y1 := Real'Max (y1, Real (glInt'First) + 100.0);
--
--        glViewport   (glInt (x1) - 1,
--                      glInt (y1) - 1,  2, 2);
--
--
--        -- set the raster (window) position
--        --
--        fx := x1 - glFloat (glInt (x1));
--        fy := y1 - glFloat (glInt (y1));
--
--        glRasterPos4f (fx, fy, 0.0, w);
--
--
--        -- restore matrices, viewport and matrix mode
--        --
--        glPopMatrix;
--        glMatrixMode (GL_PROJECTION);
--        glPopMatrix;
--
--        glPopAttrib;
      null;
      raise Program_Error with "unimplemented";
   end set_window_pos;




end opengl.Raster;
