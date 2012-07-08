
-- Simple Lumen demo/test program, using earliest incomplete library

with Lumen.Window;
with Lumen.GL;
with Lumen.Events; use Lumen.Events;

procedure Colors is

   Win        : Lumen.Window.Window_Handle;
   Terminated : Boolean:=False;

   procedure Key_Press
     (Category  : Key_Category;
      Symbol    : Key_Symbol;
      Modifiers : Modifier_Set) is
   begin
      Terminated:=True;
   end Key_Press;

begin  -- Colors

   -- Create Lumen window, accepting most defaults; turn double buffering off
   -- for simplicity
   Lumen.Window.Create (Win, Name => "Ooh, Colors!", Animated => False);
   Win.Key_Press:=Key_Press'Unrestricted_Access;

   -- Loop until user hits a key or clicks the window's Close button
   while Lumen.Window.Process_Events(Win) loop

      exit when Terminated;
      -- Exit when they destroy the window, or when they hit any key
--    exit Outer when Event.Which = Lumen.Events.Key_Press or Event.Which = Lumen.Events.Close_Window;

      -- Do our drawing
      declare
         use Lumen.GL;
      begin
         -- Red
         Clear_Color (1.0, 0.0, 0.0, 1.0);
         Clear (GL_COLOR_BUFFER_BIT);
         Flush;
         delay 1.0;
          -- Green
         Clear_Color (0.0, 1.0, 0.0, 1.0);
         Clear (GL_COLOR_BUFFER_BIT);
         Flush;
         delay 1.0;
          -- Blue
         Clear_Color (0.0, 0.0, 1.0, 1.0);
         Clear (GL_COLOR_BUFFER_BIT);
         Flush;
         delay 1.0;
      end;
   end loop;

end Colors;
