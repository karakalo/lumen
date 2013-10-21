package openGL.Texture.Coordinates
--
--  Provides openGL texture co-ordinates.
--
is
   --     pragma remote_Types;

   --- 2d
   --

   type coordinate_Generator is abstract tagged null record;

   function to_Coordinates (Self : in coordinate_Generator;   the_Vertices : access Sites) return Coordinates_2D
                            is abstract;


   type xz_Generator is new coordinate_Generator with
      record
         Normalise : texture_Transform_2d;
         Tile      : texture_Transform_2d;
      end record;

   overriding
   function to_Coordinates (Self : in xz_Generator;   the_Vertices : access Sites) return Coordinates_2D;


   type xy_Generator is new coordinate_Generator with
      record
         Normalise : texture_Transform_2d;
         Tile      : texture_Transform_2d;
      end record;

   overriding
   function to_Coordinates (Self : in xy_Generator;   the_Vertices : access Sites) return Coordinates_2D;


   type zy_Generator is new coordinate_Generator with
      record
         Normalise : texture_Transform_2d;
         Tile      : texture_Transform_2d;
      end record;

   overriding
   function to_Coordinates (Self : in zy_Generator;   the_Vertices : access Sites) return Coordinates_2D;


   type mercator_Generator is new coordinate_Generator with
      record
         null;
      end record;

   overriding
   function to_Coordinates (Self : in mercator_Generator;   the_Vertices : access Sites) return Coordinates_2D;
   --
   --  tbd: the first and last coordinates generated for a latitude ring are
   --                currently 0.0 and 0.0
   --      when they should be 0.0 and 1.0



   --- 3d
   --

   type Coordinate_3D is
      record
         S, T, R : aliased Real;
      end record;

   type Coordinate_3D_array is array (Natural range <>) of Coordinate_3D;



   --- 4d
   --

   type Coordinate_4D is
      record
         S, T, R, Q : aliased Real;
      end record;

   type Coordinate_4D_array is array (Natural range <>) of Coordinate_4D;


end opengl.Texture.Coordinates;
