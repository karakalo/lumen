
-- Simple Lumen demo/test program, using earliest incomplete library.

with Ada.Command_Line;
with System.Address_To_Access_Conversions;

with Lumen.Events.Animate;
with Lumen.Image;
with Lumen.Window;

with GL;
with GLU;

procedure Texture is

   ---------------------------------------------------------------------------

   -- Rotation wraps around at this point, in degrees
   Max_Rotation : constant := 359;

   -- Traditional cinema framrate, in frames per second
   Framerate    : constant := 24;

   ---------------------------------------------------------------------------

   Win      : Lumen.Window.Handle;
   Event    : Lumen.Events.Event_Data;
   Wide     : Natural;  -- no longer have default values since they're now set by the image size
   High     : Natural;
   Rotation : Natural := 0;
   Image    : Lumen.Image.Descriptor;
   Img_Wide : GL.glFloat;
   Img_High : GL.glFloat;
   Tx_Name  : aliased GL.GLuint;

   ---------------------------------------------------------------------------

   Program_Exit : exception;

   ---------------------------------------------------------------------------

   -- Create a texture and bind a 2D image to it
   procedure Create_Texture is

      use GL;
      use GLU;

      package GLB is new System.Address_To_Access_Conversions (GLubyte);

      IP : GLpointer;

   begin  -- Create_Texture

      -- Allocate a texture name
      glGenTextures (1, Tx_Name'Unchecked_Access);

      -- Bind texture operations to the newly-created texture name
      glBindTexture (GL_TEXTURE_2D, Tx_Name);

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
      IP := GLB.To_Pointer (Image.Values (0, 0)'Address).all'Unchecked_Access;

      -- Build our texture from the image we loaded earlier
      glTexImage2D (GL_TEXTURE_2D, 0, GL_RGBA, GLsizei (Image.Width), GLsizei (Image.Height), 0,
                    GL_RGBA, GL_UNSIGNED_BYTE, IP);

   end Create_Texture;

   ---------------------------------------------------------------------------

   -- Set or reset the window view parameters
   procedure Set_View (W, H : in Natural) is

      use GL;
      use GLU;

      Aspect : GLdouble;

   begin  -- Set_View

      -- Viewport dimensions
      glViewport (0, 0, GLsizei (W), GLsizei (H));

      -- Size of rectangle upon which image is mapped
      if Wide > High then
         Img_Wide := glFloat (1.5);
         Img_High := glFloat (1.5) * glFloat (Float (High) / Float (Wide));
      else
         Img_Wide := glFloat (1.5) * glFloat (Float (Wide) / Float (High));
         Img_High := glFloat (1.5);
      end if;

      -- Set up the projection matrix based on the window's shape--wider than
      -- high, or higher than wide
      glMatrixMode (GL_PROJECTION);
      glLoadIdentity;

      -- Set up a 3D viewing frustum, which is basically a truncated pyramid
      -- in which the scene takes place.  Roughly, the narrow end is your
      -- screen, and the wide end is 10 units away from the camera.
      if W <= H then
         Aspect := GLdouble (H) / GLdouble (W);
         glFrustum (-1.0, 1.0, -Aspect, Aspect, 2.0, 10.0);
      else
         Aspect := GLdouble (W) / GLdouble (H);
         glFrustum (-Aspect, Aspect, -1.0, 1.0, 2.0, 10.0);
      end if;

   end Set_View;

   ---------------------------------------------------------------------------

   -- Draw our scene
   procedure Draw is

      use GL;

   begin  -- Draw

      -- Set a black background
      glClearColor (0.8, 0.8, 0.8, 1.0);
      glClear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

      -- Draw a texture-mapped rectangle with the same aspect ratio as the
      -- original image
      glBegin (GL_POLYGON);
      begin
         glTexCoord3f (0.0, 1.0, 0.0);
         glVertex3f (-Img_Wide, -Img_High, 0.0);

         glTexCoord3f (0.0, 0.0, 0.0);
         glVertex3f (-Img_Wide,  Img_High, 0.0);

         glTexCoord3f (1.0, 0.0, 0.0);
         glVertex3f ( Img_Wide,  Img_High, 0.0);

         glTexCoord3f (1.0, 1.0, 0.0);
         glVertex3f ( Img_Wide, -Img_High, 0.0);
      end;
      glEnd;

      -- Rotate the object around the Y and Z axes by the current amount, to
      -- give a "tumbling" effect.
      glMatrixMode (GL_MODELVIEW);
      glLoadIdentity;
      glTranslated (0.0, 0.0, -4.0);
      glRotated (GLdouble (Rotation), 0.0, 1.0, 0.0);
      glRotated (GLdouble (Rotation), 0.0, 0.0, 1.0);

      glFlush;

      -- Now show it
      Lumen.Window.Swap (Win);

   end Draw;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for keypresses and close-window events
   procedure Quit_Handler (Event : in Lumen.Events.Event_Data) is
   begin  -- Quit_Handler
      raise Program_Exit;
   end Quit_Handler;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for Exposed events
   procedure Expose_Handler (Event : in Lumen.Events.Event_Data) is
   begin  -- Expose_Handler
      Draw;
   end Expose_Handler;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for Resized events
   procedure Resize_Handler (Event : in Lumen.Events.Event_Data) is
   begin  -- Resize_Handler
      Wide := Event.Resize_Data.Width;
      High := Event.Resize_Data.Height;
      Set_View (Wide, High);
      Draw;
   end Resize_Handler;

   ---------------------------------------------------------------------------

   -- Our draw-a-frame routine, should get called FPS times a second
   procedure New_Frame (Frame_Delta : in Duration) is
   begin  -- New_Frame
      if Rotation >= Max_Rotation then
         Rotation := 0;
      else
         Rotation := Rotation + 1;
      end if;

      Draw;
   end New_Frame;

   ---------------------------------------------------------------------------

begin  -- Texture

   -- If we haven't been given an image to work with, just do nothing
   if Ada.Command_Line.Argument_Count < 1 then
      raise Program_Exit;
   end if;

   -- Read image and use it to size the window.  This will suck if your image
   -- is very large.
   Image := Lumen.Image.From_File (Ada.Command_Line.Argument (1));
   Wide := Image.Width;
   High := Image.Height;

   -- Create Lumen window, accepting most defaults; turn double buffering off
   -- for simplicity
   Lumen.Window.Create (Win, Name   => "Spinning Picture Demo",
                        Width  => Wide,
                        Height => High,
                        Events => (Lumen.Window.Want_Key_Press => True,
                                   Lumen.Window.Want_Exposure  => True,
                                   others => False));

   -- Set up the viewport and scene parameters
   Set_View (Wide, High);

   -- Now create the texture and set up to use it
   Create_Texture;
   GL.glEnable (GL.GL_TEXTURE_2D);
   GL.glBindTexture (GL.GL_TEXTURE_2D, Tx_Name);

   -- Enter the event loop
   declare
      use Lumen.Events;
   begin
      Animate.Select_Events (Win   => Win,
                             Calls => (Key_Press    => Quit_Handler'Unrestricted_Access,
                                       Exposed      => Expose_Handler'Unrestricted_Access,
                                       Resized      => Resize_Handler'Unrestricted_Access,
                                       Close_Window => Quit_Handler'Unrestricted_Access,
                                       others       => No_Callback),
                             FPS   => Framerate,
                             Frame => New_Frame'Unrestricted_Access);
   end;

exception
   when Program_Exit =>
      null;  -- just exit this block, which terminates the app

end Texture;
