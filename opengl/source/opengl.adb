with
     System,
     Ada.Unchecked_Conversion;

package body openGL
is
   use type Real;


   --  almost_Zero
   --

   --  From the Ada 95 Quality and Style Guide, 7.2.7:
   --
   --  Tests for
   --
   --  (1) absolute "equality" to 0 in storage,
   --  (2) absolute "equality" to 0 in computation,
   --  (3) relative "equality" to 0 in storage, and
   --  (4) relative "equality" to 0 in computation:
   --
   --    abs X <= Float_Type'Model_Small                      -- (1)
   --    abs X <= Float_Type'Base'Model_Small                 -- (2)
   --    abs X <= abs X * Float_Type'Model_Epsilon            -- (3)
   --    abs X <= abs X * Float_Type'Base'Model_Epsilon       -- (4)
   --
   function almost_Zero (X : Real) return Boolean
   is
   begin
      return  abs X <= Real'Base'Model_Small;
   end Almost_zero;





   --  Colors
   --

   function to_color_Value (Self : in Real) return color_Value
   is
   begin
      if Self = 1.0 then
         return 255;
      else
         return color_Value (Self * 256.0);
      end if;
   end to_color_Value;



   function to_Real (Self : in color_Value) return Real
   is
   begin
      return Real (Self) / 255.0;
   end to_Real;



   function to_Color (R, G, B : in Real) return Color
   is
   begin
      pragma Assert (R >= 0.0 and R <= 1.0);
      pragma Assert (G >= 0.0 and G <= 1.0);
      pragma Assert (B >= 0.0 and B <= 1.0);

      return (to_color_Value (R),
              to_color_Value (G),
              to_color_Value (B));
   end to_Color;




   --  heightmap
   --

   function scaled (Self : in height_Map;   By : in Real) return height_Map
   is
   begin
      return the_height_Map : height_Map := Self
      do
         scale (the_height_Map, by => By);
      end return;
   end scaled;



   procedure scale (Self : in out height_Map;   By : in Real)
   is
   begin
      for Row in Self'Range (1) loop
         for Col in Self'Range (1) loop
            Self (Row, Col) := Self (Row, Col) * By;
         end loop;
      end loop;
   end scale;



   function height_Extent (Self : in height_Map) return Vector_2
   is
      Min : Real := Real'Last;
      Max : Real := Real'First;
   begin
      for Row in Self'Range (1) loop
         for Col in Self'Range (2) loop
            Min := Real'Min (Min,  Self (Row, Col));
            Max := Real'Max (Max,  Self (Row, Col));
         end loop;
      end loop;

      return (Min, Max);
   end height_Extent;



   function Region (Self : in height_Map;   Rows, Cols : in index_Pair) return height_Map
   is
      use type Index_t;

      Width      : constant Index_t := Index_t (Rows (2) - Rows (1));
      Height     : constant Index_t := Index_t (Cols (2) - Cols (1));

      the_Region : opengl.height_Map (1 .. Width  + 1,
                                      1 .. Height + 1);
   begin
      for Row in the_Region'Range (1)
      loop
         for Col in the_Region'Range (2)
         loop
            the_Region (Row, Col) := Self (Row + Rows (1) - 1,
                                           Col + Cols (1) - 1);
         end loop;
      end loop;

      return the_Region;
   end Region;



   ---------
   -- Images
   --

   function to_Image (From : in lucid_Image) return Image
   is
      the_Image : Image (From'Range (1),
                         From'Range (2));
   begin
      for Row in From'Range (1)
      loop
         for Col in From'Range (2)
         loop
            the_Image (Row, Col) := From (Row, Col).Primary;
         end loop;
      end loop;

      return the_Image;
   end to_Image;



   -----------
   --  Streams
   --

   use Ada.Streams;

   number_of_stream_Elements_for_a_Real : constant Ada.Streams.Stream_Element_Offset
     := Real'Stream_Size / Ada.Streams.Stream_Element'Size;


   procedure height_Map_write (Stream : not null access Ada.Streams.Root_Stream_Type'Class;   Item : in  height_Map)
   is
      Item_Size : constant Stream_Element_Offset :=   Item'Length (1)
                                                    * Item'Length (2)
                                                    * number_of_stream_Elements_for_a_Real;

      type SEA_Pointer is access all Stream_Element_Array (1 .. Item_Size);

      function As_SEA_Pointer is new Ada.Unchecked_Conversion (System.Address, SEA_Pointer);
   begin
      Ada.Streams.Write (Stream.all, As_SEA_Pointer (Item'Address).all);
   end height_Map_write;



   procedure height_Map_read (Stream : not null access Ada.Streams.Root_Stream_Type'Class;    Item : out height_Map)
   is
      Item_Size : constant Stream_Element_Offset :=   Item'Length (1)
                                                    * Item'Length (2)
                                                    * number_of_stream_Elements_for_a_Real;

      type SEA_Pointer is access all Stream_Element_Array (1 .. Item_Size);

      function As_SEA_Pointer is new Ada.Unchecked_Conversion (System.Address, SEA_Pointer);
      Last : Stream_Element_Offset;
   begin
      Ada.Streams.read (Stream.all, As_SEA_Pointer (Item'Address).all, Last);
   end height_Map_read;



   procedure height_Map_output (Stream : not null access Ada.Streams.Root_Stream_Type'Class;   Self : in  height_Map)
   is
   begin
      Integer'Output (Stream, Self'Length (1));
      Integer'Output (Stream, Self'Length (2));

      height_Map_write (Stream, Self);
   end height_Map_output;



   function height_Map_input (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return height_Map
   is
      Bounds_1 : constant Integer := Integer'Input (Stream);
      Bounds_2 : constant Integer := Integer'Input (Stream);

      Self : height_Map (1 .. Index_t (Bounds_1),
                         1 .. Index_t (Bounds_2));
   begin
      height_Map_read (Stream, Self);
      return Self;
   end height_Map_input;


end openGL;
