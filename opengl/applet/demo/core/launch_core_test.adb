with
     openGL.Errors,
     openGL.Math,
     openGL.Palette,
     openGL.Server,
     openGL.Sprite,
     openGL.Texture.Coordinates,
     openGL.Viewport,
     Lumen.Window,
     Ada.Text_IO;

procedure launch_core_Test
--
--  Exercise basic subprograms common to all GL profiles.
--
--  todo: Complete this.
--
is
   use ada.Text_IO;
   Win : Lumen.Window.Window_Handle;

begin
   -- Create Lumen window to provide a current GL context.
   --
   Lumen.Window.Create (Win,
                        Name     => "GL Core Test",
                        Animated => False);

   put_Line ("openGL Server: " & openGL.Server.Version);
end launch_core_Test;
