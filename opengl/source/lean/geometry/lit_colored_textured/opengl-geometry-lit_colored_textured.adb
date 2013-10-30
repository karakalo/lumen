with
     openGL.Program.lit_colored_textured,
     openGL.Palette,
     openGL.Shader,
     openGL.Buffer.general,
     openGL.Attribute,
     opengl.Texture,

     GL.lean,
     GL.Pointers,

     System,
     interfaces.c.Strings,
     system.Storage_Elements;


package body opengl.Geometry.lit_colored_textured
is
   use GL.lean,
       GL.Pointers,
       Interfaces,
       System;


   --  Shader Program
   --

   type program_Id is (rgba_Texture, alpha_Texture);

   type Program is
      record
         vertex_Shader   : aliased openGL.Shader.item;
         fragment_Shader : aliased openGL.Shader.item;
         Program         :         openGL.Program.lit_colored_textured.view;
      end record;

   type Programs is array (program_Id) of aliased Program;



   --- Globals
   --

   the_Programs         :         Programs;

   Attribute_1_Name     : aliased          C.char_array        := "aSite";
   Attribute_1_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_1_Name'Access);

   Attribute_2_Name     : aliased          C.char_array        := "aNormal";
   Attribute_2_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_2_Name'Access);

   Attribute_3_Name     : aliased          C.char_array        := "aColor";
   Attribute_3_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_3_Name'Access);

   Attribute_4_Name     : aliased          C.char_array        := "aCoords";
   Attribute_4_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_4_Name'Access);

   white_Texture        :         openGL.Texture.Object;


   --  Forge
   --

   type Geometry_view is access all Geometry.lit_colored_textured.item'class;


   function new_Geometry (texture_is_Alpha : in Boolean) return access Geometry.lit_colored_textured.item'class
   is
      use type openGL.Program.lit_colored_textured.view;

      Self : constant Geometry_view := new Geometry.lit_colored_textured.item;

      procedure define (the_Program         : access Program;
                        use_fragment_Shader : in     String)
      is
         use openGL.Palette,
             system.Storage_Elements;

         sample_Vertex : Vertex;

         Attribute_1   : openGL.Attribute.view;
         Attribute_2   : openGL.Attribute.view;
         Attribute_3   : openGL.Attribute.view;
         Attribute_4   : openGL.Attribute.view;

         white_Image   : constant openGL.Image := (1 .. 2 => (1 .. 2 => White));
      begin
         white_Texture       := openGL.Texture.to_Texture (white_Image);
         the_Program.Program := new openGL.Program.lit_colored_textured.item;

         the_Program.vertex_Shader  .define (openGL.Shader.Vertex,    "assets/mmi/shader/lit_colored_textured.vert");
         the_Program.fragment_Shader.define (openGL.Shader.Fragment,  use_fragment_Shader);

         the_Program.Program.define (the_Program.vertex_Shader  'Access,
                                     the_Program.fragment_Shader'Access);
         the_Program.Program.enable;


         Attribute_1 := attribute.Forge.new_Attribute
                          (name        => "aSite",
                           gl_location => the_Program.Program.attribute_Location ("aSite"),
                           size        => 3,
                           data_kind   => attribute.GL_FLOAT,
                           stride      => lit_colored_textured.Vertex'Size / 8,
                           offset      => 0,
                           normalized  => False);

         Attribute_2 := attribute.Forge.new_Attribute
                          (name        => "aNormal",
                           gl_location => the_Program.Program.attribute_Location ("aNormal"),
                           size        => 3,
                           data_kind   => attribute.GL_FLOAT,
                           stride      => lit_colored_textured.Vertex'Size / 8,
                           offset      =>   sample_Vertex.Normal (1)'Address
                                          - sample_Vertex.Site   (1)'Address,
                           normalized  => False);

         Attribute_3 := attribute.Forge.new_Attribute
           (name        => "aColor",
            gl_location => the_Program.Program.attribute_Location ("aColor"),
            size        => 4,
            data_kind   => attribute.GL_UNSIGNED_BYTE,
            stride      => lit_colored_textured.Vertex'Size / 8,
            offset      =>   sample_Vertex.Color.Primary.Red'Address
                           - sample_Vertex.Site (1)         'Address,
            normalized  => True);

         Attribute_4 := attribute.Forge.new_Attribute
                           (name        => "aCoords",
                            gl_location => the_Program.Program.attribute_Location ("aCoords"),
                            size        => 2,
                            data_kind   => attribute.GL_FLOAT,
                            stride      => lit_colored_textured.Vertex'Size / 8,
                            offset      =>   sample_Vertex.Coords.S'Address
                                           - sample_Vertex.Site (1)'Address,
                            normalized  => False);

         the_Program.Program.add (Attribute_1);
         the_Program.Program.add (Attribute_2);
         the_Program.Program.add (Attribute_3);
         the_Program.Program.add (Attribute_4);


         glBindAttribLocation (program =>  the_Program.Program.gl_Program,
                               index   =>  the_Program.Program.Attribute (named => "aSite").gl_Location,
                               name    => +Attribute_1_Name_ptr);

         glBindAttribLocation (program =>  the_Program.Program.gl_Program,
                               index   =>  the_Program.Program.Attribute (named => "aNormal").gl_Location,
                               name    => +Attribute_2_Name_ptr);

         glBindAttribLocation (program =>  the_Program.Program.gl_Program,
                               index   =>  the_Program.Program.Attribute (named => "aColor").gl_Location,
                               name    => +Attribute_3_Name_ptr);

         glBindAttribLocation (program =>  the_Program.Program.gl_Program,
                               index   =>  the_Program.Program.Attribute (named => "aCoords").gl_Location,
                               name    => +Attribute_4_Name_ptr);
      end define;

   begin
      --  Define the shaders and program, if required.
      --
      if         texture_is_Alpha
        and then the_Programs (alpha_Texture).Program = null
      then
         define (the_Programs (alpha_Texture)'Access,
                 use_fragment_shader => "assets/mmi/shader/lit_colored_textured-text.frag");

      elsif the_Programs (rgba_Texture).Program = null
      then
         define (the_Programs (rgba_Texture)'Access,
                 use_fragment_shader => "assets/mmi/shader/lit_colored_textured.frag");
      end if;


      if texture_is_Alpha then
         Self.Program_is (openGL.Program.view (the_Programs (alpha_Texture).Program));
      else
         Self.Program_is (openGL.Program.view (the_Programs (rgba_Texture).Program));
      end if;

      return Self;
   end new_Geometry;



   --  Attributes
   --

   overriding
   function is_Transparent (Self : in     Item) return Boolean
   is
   begin
      return Self.is_Transparent;
   end is_Transparent;



   function is_Transparent (Self : access Vertex_array) return Boolean
   is
      use type color_Value;
   begin
      for Each in Self'Range
      loop
         if Self (Each).Color.Opacity /= Opaque then
            return True;
         end if;
      end loop;

      return False;   -- tbd: also need to check if txture is transparent ?
   end is_Transparent;



   --- Operations
   --


   package opengl_Buffer_of_geometry_Vertices is new opengl.Buffer.general (base_object   => opengl.Buffer.array_Object,
                                                                            index         => Index_t,
                                                                            element       => Vertex,
                                                                            element_array => Vertex_array);

   procedure Vertices_are (Self : in out Item'Class;   Now : access Vertex_array)
   is
      use      opengl_Buffer_of_geometry_Vertices;
      use type Index_t,
               openGL.Buffer.view;
   begin
      if Self.Vertices = null then
         self.Vertices := new opengl_Buffer_of_geometry_Vertices.object'
                                (to_Buffer (Now,
                                            usage => opengl.buffer.static_Draw));
      else
         set (opengl_Buffer_of_geometry_Vertices.Object (Self.Vertices.all), to => Now);
      end if;

      Self.is_Transparent := is_Transparent (Now);
   end Vertices_are;



   overriding
   procedure Indices_are  (Self : in out Item;   Now       : in Indices;
                                                 for_Facia : in Positive)
   is
   begin
      raise Program_Error with "TBD";
   end Indices_are;



   overriding
   procedure enable_Texture (Self : in Item)
   is
      use GL, openGL.Texture;
   begin
      glActiveTexture (gl.GL_TEXTURE0);

      if Self.Texture = openGL.Texture.null_Object then
         enable (white_Texture);
      else
         enable (Self.Texture);
      end if;
   end enable_Texture;


end opengl.Geometry.lit_colored_textured;
