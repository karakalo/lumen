with Lumen.Window;
with Lumen.Events;
with Lumen.Events.Animate;
with Lumen.Image;
with GL;
with GLU;
with System.Address_To_Access_Conversions;

procedure Lesson06 is

   The_Window : Lumen.Window.Handle;

   X_Rotation, Y_Rotation, Z_Rotation : GL.GLfloat := 0.0;
   The_Texture : aliased GL.GLuint;

   Program_Exit : Exception;

   -- simply exit this program
   procedure Quit_Handler (Event : in Lumen.Events.Event_Data) is
   begin
      raise Program_Exit;
   end;

   -- Resize the scene
   procedure Resize_Scene (Width, Height : in Natural) is
      use GL;
      use GLU;
   begin

      -- reset current viewport
      glViewport (0, 0, GLsizei(Width), GLsizei(Height));

      -- select projection matrix and reset it
      glMatrixMode (GL_PROJECTION);
      glLoadIdentity;

      -- calculate aspect ratio
      gluPerspective(45.0, GLdouble(Width)/GLdouble(Height), 0.1, 100.0);

      -- select modelview matrix and reset it
      glMatrixMode (GL_MODELVIEW);
   end Resize_Scene;

   procedure Load_GL_Textures is
      use GL;
      use GLU;
      package GLB is new System.Address_To_Access_Conversions (GLubyte);
      IP : GLpointer;
      Image : constant Lumen.Image.Descriptor := Lumen.Image.From_File ("data/NeHe.bmp");
   begin
      -- Allocate a texture name
      glGenTextures (1, The_Texture'Unchecked_Access);

      -- Bind texture operations to the newly-created texture name
      glBindTexture (GL_TEXTURE_2D, The_Texture);

      -- Select modulate to mix texture with color for shading
      glTexEnvi (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

      -- Wrap textures at both edges
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

      -- How the texture behaves when minified and magnified
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

      -- Create a pointer to the image.  This sort of horror show is going to
      -- be disappearing once Lumen includes its own OpenGL bindings.
      IP := GLB.To_Pointer (Image.Values.all'Address).all'Unchecked_Access;

      -- Build our texture from the image we loaded earlier
      glTexImage2D (GL_TEXTURE_2D, 0, GL_RGBA, GLsizei (Image.Width), GLsizei (Image.Height), 0,
                    GL_RGBA, GL_UNSIGNED_BYTE, IP);
   end Load_GL_Textures;

   procedure Init_GL is
      use GL;
      use GLU;
   begin
      -- load textures (new)
      Load_GL_Textures;
      -- enable texture mapping (new)
      glEnable (GL_TEXTURE_2D);
      -- smooth shading
      glShadeModel (GL_SMOOTH);

      -- black background
      glClearColor (0.0, 0.0, 0.0, 0.5);

      -- depth buffer setup
      glClearDepth (1.0);
      -- enable depth testing
      glEnable (GL_DEPTH_TEST);
      -- type of depth test
      glDepthFunc (GL_LEQUAL);

      glHint (GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
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
      use GL;
   begin
      -- clear screen and depth buffer
      glClear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      -- reset current modelview matrix
      glLoadIdentity;
      -- move into the screen 5 units
      glTranslatef (0.0, 0.0, -5.0);

      -- rotate on the X axis
      glRotatef (X_Rotation, 1.0, 0.0, 0.0);
      -- rotate on the Y axis
      glRotatef (Y_Rotation, 0.0, 1.0, 0.0);
      -- rotate on the Z axis
      glRotatef (Z_Rotation, 0.0, 0.0, 1.0);

      -- select the texture
      glBindTexture (GL_TEXTURE_2D, The_Texture);

      -- draw square
      glBegin (GL_QUADS);
         -- front
         glTexCoord2f (0.0, 0.0); glVertex3f (-1.0, -1.0,  1.0);
         glTexCoord2f (0.0, 1.0); glVertex3f ( 1.0, -1.0,  1.0);
         glTexCoord2f (1.0, 1.0); glVertex3f ( 1.0,  1.0,  1.0);
         glTexCoord2f (1.0, 0.0); glVertex3f (-1.0,  1.0,  1.0);
         -- back
         glTexCoord2f (0.0, 0.0); glVertex3f ( 1.0, -1.0, -1.0);
         glTexCoord2f (0.0, 1.0); glVertex3f (-1.0, -1.0, -1.0);
         glTexCoord2f (1.0, 1.0); glVertex3f (-1.0,  1.0, -1.0);
         glTexCoord2f (1.0, 0.0); glVertex3f ( 1.0,  1.0, -1.0);
         -- top
         glTexCoord2f (0.0, 0.0); glVertex3f (-1.0,  1.0,  1.0);
         glTexCoord2f (0.0, 1.0); glVertex3f ( 1.0,  1.0,  1.0);
         glTexCoord2f (1.0, 1.0); glVertex3f ( 1.0,  1.0, -1.0);
         glTexCoord2f (1.0, 0.0); glVertex3f (-1.0,  1.0, -1.0);
         -- bottom
         glTexCoord2f (0.0, 0.0); glVertex3f ( 1.0, -1.0,  1.0);
         glTexCoord2f (0.0, 1.0); glVertex3f (-1.0, -1.0,  1.0);
         glTexCoord2f (1.0, 1.0); glVertex3f (-1.0, -1.0, -1.0);
         glTexCoord2f (1.0, 0.0); glVertex3f ( 1.0, -1.0, -1.0);
         -- right
         glTexCoord2f (0.0, 0.0); glVertex3f ( 1.0, -1.0,  1.0);
         glTexCoord2f (0.0, 1.0); glVertex3f ( 1.0, -1.0, -1.0);
         glTexCoord2f (1.0, 1.0); glVertex3f ( 1.0,  1.0, -1.0);
         glTexCoord2f (1.0, 0.0); glVertex3f ( 1.0,  1.0,  1.0);
         -- left
         glTexCoord2f (0.0, 0.0); glVertex3f (-1.0, -1.0, -1.0);
         glTexCoord2f (0.0, 1.0); glVertex3f (-1.0, -1.0,  1.0);
         glTexCoord2f (1.0, 1.0); glVertex3f (-1.0,  1.0,  1.0);
         glTexCoord2f (1.0, 0.0); glVertex3f (-1.0,  1.0, -1.0);
      glEnd;

      X_Rotation := X_Rotation + 0.3;
      Y_Rotation := Y_Rotation + 0.2;
      Z_Rotation := Z_Rotation + 0.4;
   end Draw;

   procedure Frame_Handler (Frame_Delta : in Duration) is
      pragma Unreferenced (Frame_Delta);
   begin
      Draw;
      Lumen.Window.Swap (The_Window);
   end Frame_Handler;

begin
   Lumen.Window.Create (Win    => The_Window,
                        Name   => "NeHe Lesson 6",
                        Width  => 640,
                        Height => 480,
                        Events => (Lumen.Window.Want_Key_Press => True,
                                   Lumen.Window.Want_Exposure  => True,
                                   others => False));

   Resize_Scene (640, 480);
   Init_GL;

   Lumen.Events.Animate.Select_Events (Win   => The_Window,
                                       FPS   => Lumen.Events.Animate.Flat_Out,
                                       Frame => Frame_Handler'Unrestricted_Access,
                                       Calls => (Lumen.Events.Resized       => Resize_Handler'Unrestricted_Access,
                                                 Lumen.Events.Close_Window  => Quit_Handler'Unrestricted_Access,
                                                 others => Lumen.Events.No_Callback));
exception
   when Program_Exit =>
      null; -- normal termination
end Lesson06;
