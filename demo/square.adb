
-- Simple Lumen demo/test program, using earliest incomplete library
--
-- Build with: gnatmake square

with Ada.Text_IO;

with Lumen.Window;
with Lumen.Events.Keys;
with Lumen.Joystick;

with GL;
with GLU;

procedure Square is

   ---------------------------------------------------------------------------

   Win     : Lumen.Window.Handle;
   Event   : Lumen.Events.Event_Data;
   Stick   : Lumen.Joystick.Handle;
   Joy     : Lumen.Joystick.Joystick_Event_Data;

   ---------------------------------------------------------------------------

   -- Set or reset the window view parameters
   procedure Set_View (W, H : in GL.GLsizei) is

      use GL;
      use GLU;

      Aspect : GLdouble;

   begin  -- Set_View

      -- Viewport dimensions
      glViewport (0, 0, W, H);

      -- Set up the projection matrix based on the window's shape--wider than
      -- high, or higher than wide
      glMatrixMode (GL_PROJECTION);
      glLoadIdentity;
      if W <= H then
         Aspect := GLdouble (H) / GLdouble (W);
         gluOrtho2D (-2.0, 2.0, -2.0 * Aspect, 2.0 * Aspect);
      else
         Aspect := GLdouble (W) / GLdouble (H);
         gluOrtho2D (-2.0 * Aspect, 2.0 * Aspect, -2.0, 2.0);
      end if;
   end Set_View;

   ---------------------------------------------------------------------------

   package IIO is new Ada.Text_IO.Integer_IO (Lumen.Events.Key_Symbol);

   ---------------------------------------------------------------------------

begin  -- Square

   -- Create Lumen window, accepting most defaults; turn double buffering off
   -- for simplicity
   Lumen.Window.Create (Win, Name => "Ooh, Square!", Animated => False,
                        Events => (Lumen.Window.Want_Key_Press => True, others => False));

   Set_View (400, 400);

   -- Try opening first joystick
   Lumen.Joystick.Open (Stick);

   -- Loop until user hits a key or clicks the window's Close button
   Outer: loop

      -- Process window events if there are any pending
      while Lumen.Events.Pending (Win) > 0 loop
         declare
            use type Lumen.Events.Event_Type;
         begin
            Event := Lumen.Events.Next_Event (Win);

            -- Exit when they destroy the window
            exit Outer when Event.Which = Lumen.Events.Close_Window;

            -- Check for keypresses
            if Event.Which = Lumen.Events.Key_Press then
               declare
                  use Ada.Text_IO;
                  use Lumen.Events;
               begin
                  Put ("Got ");
                  IIO.Put (Event.Key_Data.Key, Width => 1, Base => 16);
                  if Event.Key_Data.Key_Type = Key_Graphic then
                     Put (" '" & To_Character (Event.Key_Data.Key) & "'");
                  end if;
                  New_Line;

                  exit Outer when
                     (Event.Key_Data.Key_Type = Key_Control and then To_Character (Event.Key_Data.Key) = ASCII.ESC) or
                     (Event.Key_Data.Key_Type = Key_Graphic and then To_Character (Event.Key_Data.Key) = 'q');
               end;
            end if;

            -- If we were resized, adjust the viewport dimensions
            if Event.Which = Lumen.Events.Resized then
               Set_View (GL.GLsizei (Event.Resize_Data.Width), GL.GLsizei (Event.Resize_Data.Height));
            end if;
         end;
      end loop;

      -- Process joystick events if there are any pending
      while Lumen.Joystick.Pending (Stick) > 0 loop
         declare
            use Ada.Text_IO;
            use Lumen.Joystick;
         begin
            Joy := Next_Event (Stick);
            case Joy.Which is
               when Joystick_Button_Press =>
                  Put_Line ("Button" & Positive'Image (Joy.Number) & " press");

               when Joystick_Button_Release =>
                  Put_Line ("Button" & Positive'Image (Joy.Number) & " release");

               when Joystick_Axis_Change =>
                  Put_Line ("Axis" & Positive'Image (Joy.Number) & " --> " & Integer'Image (Joy.Axis_Value));
            end case;
         end;
      end loop;

      -- Do our drawing
      declare
         use GL;
      begin
         glClearColor (0.0, 0.0, 0.0, 0.0);
         glClear (GL_COLOR_BUFFER_BIT);

         glColor3f (1.0, 1.0, 1.0);

         glBegin (GL_POLYGON);
         begin
            glVertex2f (-0.5, -0.5);
            glVertex2f (-0.5, 0.5);
            glVertex2f (0.5, 0.5);
            glVertex2f (0.5, -0.5);
         end;
         glEnd;

         glRotated (20.0, 0.0, 0.0, 1.0);

         glFlush;
         delay 0.25;
      end;
   end loop Outer;

end Square;
