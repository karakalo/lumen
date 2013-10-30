with
     opengl.Buffer.general;

package opengl.Buffer.texture_coords is new opengl.Buffer.general (base_object   => opengl.Buffer.array_Object,
                                                                   index         => opengl.Index_t,
                                                                   element       => opengl.Coordinate_2D,
                                                                   element_array => opengl.Coordinates_2D);
