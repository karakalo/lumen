with
     openGL.Shader,
     openGL.Buffer.general,
     openGL.Attribute,
     openGL.Texture,
     openGL.Palette,

     GL.lean,
     GL.Pointers,

     System,
     interfaces.c.Strings,
     system.Storage_Elements;


package body opengl.Geometry.lit_textured_skinned
is
   use GL.lean,
       GL.Pointers,

       Interfaces,
       System;


   --  Globals
   --
   the_vertex_Shader   : aliased openGL.Shader.item;
   the_fragment_Shader : aliased openGL.Shader.item;
   the_Program         : aliased openGL.Program.lit_textured_skinned.view;

   white_Texture       :         openGL.Texture.Object;



   --  Forge
   --

   type Geometry_view is access all Geometry.lit_textured_skinned.item'class;


   function new_Geometry return access Geometry.lit_textured_skinned.item'class
   is
      Self : constant Geometry_view := new Geometry.lit_textured_skinned.item;
   begin
      Self.Program_is (opengl.Program.view (geometry.lit_textured_skinned.Program));
      return Self;
   end new_Geometry;



   --  Attributes
   --

   overriding
   function  is_Transparent (Self : in     Item) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end is_Transparent;



   function Program return openGL.Program.lit_textured_skinned.view
   is
      use type openGL.Program.lit_textured_skinned.view;
   begin
      if the_Program = null then   -- Define the shaders and program.
         declare
            use openGL.Palette,
                system.Storage_Elements;

            sample_Vertex        : Vertex;

            Attribute_1_Name     : aliased          C.char_array := "aSite";
            Attribute_1_Name_ptr : aliased constant C.strings.chars_ptr
              := C.strings.to_chars_ptr (Attribute_1_Name'Unchecked_Access);

            Attribute_2_Name     : aliased          C.char_array := "aNormal";
            Attribute_2_Name_ptr : aliased constant C.strings.chars_ptr
              := C.strings.to_chars_ptr (Attribute_2_Name'Unchecked_Access);

            Attribute_3_Name     : aliased          C.char_array := "aColor";
            Attribute_3_Name_ptr : aliased constant C.strings.chars_ptr
              := C.strings.to_chars_ptr (Attribute_3_Name'Unchecked_Access);

            Attribute_4_Name     : aliased          C.char_array := "aCoords";
            Attribute_4_Name_ptr : aliased constant C.strings.chars_ptr
              := C.strings.to_chars_ptr (Attribute_4_Name'Unchecked_Access);

            Attribute_5_Name     : aliased          C.char_array := "bone_Ids";
            Attribute_5_Name_ptr : aliased constant C.strings.chars_ptr
              := C.strings.to_chars_ptr (Attribute_5_Name'Unchecked_Access);

            Attribute_6_Name     : aliased          C.char_array := "bone_Weights";
            Attribute_6_Name_ptr : aliased constant C.strings.chars_ptr
              := C.strings.to_chars_ptr (Attribute_6_Name'Unchecked_Access);

            Attribute_1 : openGL.Attribute.view;
            Attribute_2 : openGL.Attribute.view;
            Attribute_3 : openGL.Attribute.view;
            Attribute_4 : openGL.Attribute.view;
            Attribute_5 : openGL.Attribute.view;
            Attribute_6 : openGL.Attribute.view;

            white_Image : constant openGL.Image := (1 .. 2 => (1 .. 2 => White));
         begin
            white_Texture := openGL.Texture.to_Texture (white_Image);

            the_vertex_Shader  .define (openGL.Shader.Vertex,   "assets/mmi/shader/lit_textured_skinned.vert");
            the_fragment_Shader.define (openGL.Shader.Fragment, "assets/mmi/shader/lit_textured_skinned.frag");

            the_Program := new openGL.Program.lit_textured_skinned.item;
            the_Program.define (the_vertex_Shader  'Access,
                                the_fragment_Shader'Access);
            the_Program.enable;

            Attribute_1 := attribute.Forge.new_Attribute
                             (name        => "aSite",
                              gl_location => the_Program.attribute_Location ("aSite"),
                              size        => 3,
                              data_kind   => attribute.GL_FLOAT,
                              stride      => lit_textured_skinned.Vertex'Size / 8,
                              offset      => 0,
                              normalized  => False);

            Attribute_2 := attribute.Forge.new_Attribute
                             (name        => "aNormal",
                              gl_location => the_Program.attribute_Location ("aNormal"),
                              size        => 3,
                              data_kind   => attribute.GL_FLOAT,
                              stride      => lit_textured_skinned.Vertex'Size / 8,
                              offset      =>   sample_Vertex.Normal (1)'Address
                                             - sample_Vertex.Site   (1)'Address,
                              normalized  => False);

            Attribute_3 := attribute.Forge.new_Attribute
                             (name        => "aColor",
                              gl_location => the_Program.attribute_Location ("aColor"),
                              size        => 4,
                              data_kind   => attribute.GL_UNSIGNED_BYTE,
                              stride      => lit_textured_skinned.Vertex'Size / 8,
                              offset      =>   sample_Vertex.Color.Primary.Red'Address
                                             - sample_Vertex.Site (1)         'Address,
                              normalized  => True);

            Attribute_4 := attribute.Forge.new_Attribute
                             (name        => "aCoords",
                              gl_location => the_Program.attribute_Location ("aCoords"),
                              size        => 2,
                              data_kind   => attribute.GL_FLOAT,
                              stride      => lit_textured_skinned.Vertex'Size / 8,
                              offset      =>   sample_Vertex.Coords.S'Address
                                             - sample_Vertex.Site (1)'Address,
                              normalized  => False);

            Attribute_5 := attribute.Forge.new_Attribute
                             (name        => "bone_Ids",
                              gl_location => the_Program.attribute_Location ("bone_Ids"),
                              size        => 4,
                              data_kind   => attribute.GL_FLOAT,
                              stride      => lit_textured_skinned.Vertex'Size / 8,
                              offset      =>   sample_Vertex.bone_Ids (1)'Address
                                             - sample_Vertex.Site (1)'Address,
                              normalized  => False);

            Attribute_6 := attribute.Forge.new_Attribute
                             (name        => "bone_Weights",
                              gl_location => the_Program.attribute_Location ("bone_Weights"),
                              size        => 4,
                              data_kind   => attribute.GL_FLOAT,
                              stride      => lit_textured_skinned.Vertex'Size / 8,
                              offset      =>   sample_Vertex.bone_Weights (1)'Address
                                             - sample_Vertex.Site (1)'Address,
                              normalized  => False);

            the_Program.add (Attribute_1);
            the_Program.add (Attribute_2);
            the_Program.add (Attribute_3);
            the_Program.add (Attribute_4);
            the_Program.add (Attribute_5);
            the_Program.add (Attribute_6);

            glBindAttribLocation (program => the_Program.gl_Program,
                                  index   => the_Program.Attribute (named => "aSite").gl_Location,
                                  name    => +Attribute_1_Name_ptr);

            glBindAttribLocation (program => the_Program.gl_Program,
                                  index   => the_Program.Attribute (named => "aNormal").gl_Location,
                                  name    => +Attribute_2_Name_ptr);

            glBindAttribLocation (program => the_Program.gl_Program,
                                  index   => the_Program.Attribute (named => "aColor").gl_Location,
                                  name    => +Attribute_3_Name_ptr);

            glBindAttribLocation (program => the_Program.gl_Program,
                                  index   => the_Program.Attribute (named => "aCoords").gl_Location,
                                  name    => +Attribute_4_Name_ptr);

            glBindAttribLocation (program => the_Program.gl_Program,
                                  index   => the_Program.Attribute (named => "bone_Ids").gl_Location,
                                  name    => +Attribute_5_Name_ptr);

            glBindAttribLocation (program => the_Program.gl_Program,
                                  index   => the_Program.Attribute (named => "bone_Weights").gl_Location,
                                  name    => +Attribute_6_Name_ptr);
         end;
      end if;

      return the_Program;
   end Program;



   overriding
   procedure Indices_are  (Self : in out Item;   Now       : in Indices;
                                                 for_Facia : in Positive)
   is
   begin
      raise Program_Error with "TBD";
   end Indices_are;



   --  Operations
   --

   package opengl_Buffer_of_geometry_Vertices is new opengl.Buffer.general (base_object   => opengl.Buffer.array_Object,
                                                                            index         => Index_t,
                                                                            element       => Vertex,
                                                                            element_array => Vertex_array);

   procedure Vertices_are (Self : in out Item'Class;   Now : access Vertex_array)
   is
      use      opengl_Buffer_of_geometry_Vertices;
      use type Index_t;
   begin
      self.Vertices := new opengl_Buffer_of_geometry_Vertices.object' (to_Buffer (Now,
                                                                                  usage => opengl.buffer.static_Draw));
--        Self.is_Transparent := is_Transparent (Now);
   end Vertices_are;



   overriding
   procedure enable_Texture (Self : in Item)
   is
      use GL,
          openGL.Texture;
   begin
      glActiveTexture (gl.GL_TEXTURE0);

      if Self.Texture = openGL.Texture.null_Object then
         enable (white_Texture);
      else
         enable (Self.Texture);
      end if;
   end enable_Texture;


end opengl.Geometry.lit_textured_skinned;
