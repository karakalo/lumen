with
     Ada.Text_IO,
     Ada.Integer_Text_IO,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded;


package body openGL.IO.wavefront
is
   use Ada.Text_IO;


   package real_Text_IO is new Ada.Text_IO.Float_IO (openGL.Real);

   --     type io_Faces_view is access all io.Faces;



   function to_Text (Self : in String) return Text
   is
   begin
      return Ada.Strings.Unbounded.To_Unbounded_String (Self);
   end to_Text;



   function to_Vector_3 (Self : in String) return Vector_3
   is
      use real_Text_IO;

      X, Y, Z : Real;
      Last    : Natural;
   begin
      Get (Self, X, Last);
      Get (Self (Last + 1 .. Self'Last), Y, Last);
      Get (Self (Last + 1 .. Self'Last), Z, Last);

      return (X, Y, Z);
   end to_Vector_3;



   function to_Coordinate (Self : in String) return Coordinate_2D
   is
      use real_Text_IO;

      U, V : Real;
      Last : Natural;
   begin
      Get (Self, U, Last);
      Get (Self (Last + 1 .. Self'Last), V, Last);

      return (U, V);
   end to_Coordinate;



   function to_Facet (Self : in String) return IO.Face
   is
      use      Ada.Integer_Text_IO;
      use type Index_t;

      site_Id,
      coord_Id,
      normal_Id    : Integer;

      the_Vertices : Vertices (1 .. 500);
      vertex_Count : Index_t := 0;

      Last         : Natural := Self'First - 1;

   begin
      loop
         Get (Self (Last + 1 .. Self'Last),
              site_Id,
              Last);

         if        Last            = Self'Last
           or else Self (Last + 1) = ' '
         then   --  both texture coord and normal are absent
            coord_Id  := Integer (null_Id);
            normal_Id := Integer (null_Id);

         elsif Self (Last + 1) = '/'
         then
            if Self (Last + 2) = '/'
            then   -- texture coord is absent
               coord_Id := Integer (null_Id);
               Get (Self (Last + 3 .. Self'Last),
                    normal_Id,
                    Last);
            else
               Get (Self (Last + 2 .. Self'Last),
                    coord_Id,
                    Last);

               if        Last            = Self'Last
                 or else Self (Last + 1) = ' '
               then   -- normal is absent
                  normal_Id := Integer (null_Id);

               elsif Self (Last + 1) = '/'
               then
                  Get (Self (Last + 2 .. Self'Last), normal_Id, Last);

               else
                  raise Constraint_Error with "Invalid indices: " & Self & ".";
               end if;
            end if;

         else
            raise Constraint_Error with "Invalid indices: " & Self & ".";
         end if;

         if          site_Id < 0
           or else  coord_Id < 0
           or else normal_Id < 0
         then
            raise Constraint_Error with "Negative indices not implemented: " & Self & ".";
         end if;

         vertex_Count                := vertex_Count + 1;
         the_Vertices (vertex_Count) := (Index_t (  site_Id),
                                         Index_t ( coord_Id),
                                         Index_t (normal_Id),
                                         null_Id);
         exit when Last + 1 >= Self'Last;
      end loop;


      case vertex_Count
      is
         when 3      =>   return (Triangle, the_Vertices (1 .. 3));
         when 4      =>   return (Quad,     the_Vertices (1 .. 4));
         when others =>   return (Polygon,  new Vertices' (the_Vertices (1 .. vertex_Count)));
      end case;
   end to_Facet;



   function to_Model (model_Path : in String) return IO.Model
   is
      use      Ada.Strings.Fixed;
      use type Index_t,
               long_Index_t;

      the_File     : File_Type;

      the_Sites    : Sites_view    := new Sites (Index_t);
      site_Count   : Index_t       := 0;

      the_Coords   : Coords_view   := new Coordinates_2D (Index_t);
      coord_Count  : Index_t       := 0;

      the_Normals  : Normals_view  := new Normals (Index_t);
      normal_Count : Index_t       := 0;

      the_Faces    : IO.Faces_view := new IO.Faces' (1 .. 50_000 => <>);
      face_Count   : long_Index_t  := 0;

      the_Model    : Model;

   begin
      Open (the_File, In_File, model_Path);

      while not End_Of_File (the_File)
      loop
         declare
            the_Line : constant String := Get_Line (the_File);
         begin
            if the_Line'Length = 0 or else the_Line (1) = '#'
            then
               null;

            elsif Head (the_Line, 6) = "mtllib"
            then
               null;   -- tbd

            elsif Head (the_Line, 2) = "f "
            then
               face_Count             := face_Count + 1;
               the_Faces (face_Count) :=
                  to_Facet (the_Line (3 .. the_Line'Last));

            elsif Head (the_Line, 2) = "v "
            then
               site_Count             := site_Count + 1;
               the_Sites (site_Count) :=
                  to_Vector_3 (the_Line (3 .. the_Line'Last));

            elsif Head (the_Line, 3) = "vt "
            then
               coord_Count              := coord_Count + 1;
               the_Coords (coord_Count) :=
                  to_Coordinate (the_Line (4 .. the_Line'Last));

            elsif Head (the_Line, 3) = "vn "
            then
               normal_Count               := normal_Count + 1;
               the_Normals (normal_Count) :=
                  to_Vector_3 (the_Line (4 .. the_Line'Last));

            elsif Head (the_Line, 2) = "o "
            then
               null; -- currently ignored
            --                 face_Count             := face_Count + 1;
            --                 the_Faces (face_Count) := (a_Group,
            --(object_Name,
            --                                                      object_Name
            --  => to_Text (the_Line (3 .. the_Line'Last))));

            elsif Head (the_Line, 2) = "g "
            then
               null; -- currently ignored
            --                 face_Count             := face_Count + 1;
            --                 the_Faces (face_Count) := (a_Group, (group_Name,
            --                                                      group_Name
            --=> to_Text (the_Line (3 .. the_Line'Last))));

            elsif Head (the_Line, 2) = "s "
            then
               null; -- currently ignored
            --                 declare
            --                    use ada.integer_Text_IO;
            --
            --                    the_Id : Natural;
            --                    Last   : Natural;
            --                 begin
            --                    if Head (the_Line, 5) = "s off" then
            --                       the_Id := 0;
            --                    else
            --                       get (the_Line (3 .. the_Line'Last),
            --  the_Id,  Last);
            --                    end if;
            --
            --                    face_Count             := face_Count + 1;
            --                    the_Faces (face_Count) := (a_Group,
            --(smoothing_Group, smooth_group_Id => the_Id));
            --                 end;

            else
               null; -- put_Line ("opengl.io.wavefront ~ Unhandled line in " &
                     --  model_Path & ": '" & the_Line & "'");
            end if;
         end;
      end loop;

      Close (the_File);


      declare
         used_Sites   : constant IO.Sites_view   := new Sites'         (the_Sites   (1 ..   site_Count));
         used_Coords  : constant IO.Coords_view  := new Coordinates_2D'(the_Coords  (1 ..  coord_Count));
         used_Normals : constant IO.Normals_view := new Normals'       (the_Normals (1 .. normal_Count));
         used_Faces   : constant IO.Faces_view   := new IO.Faces'      (the_Faces   (1 ..   face_Count));
      begin
         free (the_Sites);
         free (the_Coords);
         free (the_Normals);
         free (the_Faces);

         return (Sites   => used_Sites,
                 Coords  => used_Coords,
                 Normals => used_Normals,
                 Weights => null,
                 Faces   => used_Faces);
      end;

   end to_Model;




   --- Images
   --

   function Image (Self : in IO.Face) return String
   is
      use      Ada.Strings.Unbounded;
      use type openGL.Index_t;

      the_Vertices : Vertices         renames Vertices_of (Self);
      the_Image    : Unbounded_String      := To_Unbounded_String ("f ");

      function id_Image (Self : in openGL.Index_t) return String
      is
         use Ada.Strings.Fixed;
      begin
         return Trim (Index_t'Image (Self),
                      Ada.Strings.Left);
      end id_Image;

   begin
      for Each in the_Vertices'Range
      loop
         Append (the_Image,
                 id_Image (the_Vertices (Each).site_Id));

         if the_Vertices (Each).coord_Id = null_Id
         then
            if the_Vertices (Each).normal_Id /= null_Id
            then
               Append (the_Image, "/");
            end if;
         else
            Append (the_Image, "/" & id_Image (the_Vertices (Each).coord_Id));
         end if;

         if the_Vertices (Each).normal_Id /= null_Id
         then
            Append (the_Image,
                    "/" & id_Image (the_Vertices (Each).normal_Id));
         end if;

         Append (the_Image, " ");
      end loop;

      return To_String (the_Image);
   end Image;



   function Image (Self : in wavefront.Group) return String
   is
      use Ada.Strings.Unbounded;
   begin
      case Self.Kind
      is
         when object_Name     =>   return "o " &     To_String (Self.object_Name);
         when group_Name      =>   return "g " &     To_String (Self.group_Name);
         when smoothing_Group =>   return "s"  & Natural'Image (Self.smooth_group_Id);
         when merging_Group   =>   return "";
      end case;
   end Image;



   function Image (Self : in wavefront.Face) return String
   is
   begin
      case Self.Kind
      is
         when a_Group =>   return Image (Self.Group);
         when a_Facet =>   return Image (Self.Facet);
      end case;
   end Image;





   --- Deprecated
   --

   type wf_Faces_view is access all wavefront.Faces;


   function to_Model (model_Path : in String) return wavefront.Model
   is
      use Ada.Strings.Fixed;
      use type Index_t, long_Index_t;

      the_File     : File_Type;

      the_Sites    : Sites (Index_t);
      site_Count   : Index_t := 0;

      the_Coords   : Coordinates_2D (Index_t);
      coord_Count  : Index_t := 0;

      the_Normals  : Normals (Index_t);
      normal_Count : Index_t := 0;

      the_Faces    : wf_Faces_view := new Faces'(1 .. 50_000 => <>);
      face_Count   : long_Index_t    := 0;

      the_Model    : Model;
   begin
      Open (the_File, In_File, model_Path);

      while not End_Of_File (the_File)
      loop
         declare
            the_Line : constant String := Get_Line (the_File);
         begin
            if the_Line'Length = 0 or else the_Line (1) = '#' then
               null;

            elsif Head (the_Line, 6) = "mtllib" then
               null;   -- tbd

            elsif Head (the_Line, 2) = "f " then
               face_Count             := face_Count + 1;
               the_Faces (face_Count) := (a_Facet,
                                          to_Facet (the_Line (3 .. the_Line'Last)));

            elsif Head (the_Line, 2) = "v " then
               site_Count             := site_Count + 1;
               the_Sites (site_Count) := to_Vector_3 (the_Line (3 .. the_Line'Last));

            elsif Head (the_Line, 3) = "vt " then
               coord_Count              := coord_Count + 1;
               the_Coords (coord_Count) := to_Coordinate (the_Line (4 .. the_Line'Last));

            elsif Head (the_Line, 3) = "vn " then
               normal_Count               := normal_Count + 1;
               the_Normals (normal_Count) := to_Vector_3 (the_Line (4 .. the_Line'Last));

            elsif Head (the_Line, 2) = "o " then
               face_Count             := face_Count + 1;
               the_Faces (face_Count) := (a_Group,
                                          (object_Name,
                                           object_Name => to_Text (the_Line (3 .. the_Line'Last))));
            --                 put_line ("Found: '" & the_Line (3 ..
            --  the_Line'Last) & "'");

            elsif Head (the_Line, 2) = "g " then
               face_Count             := face_Count + 1;
               the_Faces (face_Count) := (a_Group,
                                          (group_Name,
                                           group_Name => to_Text (the_Line (3 .. the_Line'Last))));

            elsif Head (the_Line, 2) = "s " then
               declare
                  use Ada.Integer_Text_IO;

                  the_Id : Natural;
                  Last   : Natural;
               begin
                  if Head (the_Line, 5) = "s off" then
                     the_Id := 0;
                  else
                     Get (the_Line (3 .. the_Line'Last), the_Id, Last);
                  end if;

                  face_Count             := face_Count + 1;
                  the_Faces (face_Count) := (a_Group,
                                             (smoothing_Group,
                                              smooth_group_Id => the_Id));
               end;

            else
               Put_Line
                 ("opengl.io.wavefront ~ Unhandled line in " &
                  model_Path &
                  ": '" &
                  the_Line &
                  "'");
            end if;
         end;
      end loop;

      Close (the_File);


      declare
         procedure free is new Ada.Unchecked_Deallocation (Faces,  wf_Faces_view);

         used_Faces : constant wf_Faces_view := new wavefront.Faces'(the_Faces (1 .. face_Count));
      begin
         free (the_Faces);

         return
           (Sites   => new openGL.Sites'(the_Sites (1 .. site_Count)),
            Coords  => new Coordinates_2D'(the_Coords (1 .. coord_Count)),
            Normals => new openGL.Normals'(the_Normals (1 .. normal_Count)),
            Faces   => used_Faces);
      end;

   end to_Model;



   procedure write
     (the_Model    : in wavefront.Model;
      to_file_Path : in String)
   is
      the_File : Ada.Text_IO.File_Type;

      package opengl_Real_text_IO is new Ada.Text_IO.Float_IO (openGL.Real);
      use opengl_Real_text_IO;

   begin
      Create (the_File, Out_File, Name => to_file_Path);

      --  write sites
      --
      for Each in the_Model.Sites'Range
      loop
         Put (the_File, "v ");
         Put (the_File, the_Model.Sites (Each) (1), Aft => 19, Exp => 0);
         Put (the_File, " ");
         Put (the_File, the_Model.Sites (Each) (2), Aft => 19, Exp => 0);
         Put (the_File, " ");
         Put (the_File, the_Model.Sites (Each) (3), Aft => 19, Exp => 0);
         New_Line (the_File);
      end loop;

      New_Line (the_File);

      --  write texture coords
      --
      for Each in the_Model.Coords'Range
      loop
         Put (the_File, "vt ");
         Put (the_File, the_Model.Coords (Each).S, Aft => 19, Exp => 0);
         Put (the_File, " ");
         Put (the_File, the_Model.Coords (Each).T, Aft => 19, Exp => 0);
         New_Line (the_File);
      end loop;

      New_Line (the_File);

      --  write normals
      --
      for Each in the_Model.Normals'Range
      loop
         Put (the_File, "vn ");
         Put (the_File, the_Model.Normals (Each) (1), Aft => 19, Exp => 0);
         Put (the_File, " ");
         Put (the_File, the_Model.Normals (Each) (2), Aft => 19, Exp => 0);
         Put (the_File, " ");
         Put (the_File, the_Model.Normals (Each) (3), Aft => 19, Exp => 0);
         New_Line (the_File);
      end loop;

      New_Line (the_File);

      --  write faces
      --
      for Each in the_Model.Faces'Range
      loop
         Put_Line (the_File, Image (the_Model.Faces (Each)));
      end loop;

      Close (the_File);
   end write;


end opengl.IO.wavefront;
