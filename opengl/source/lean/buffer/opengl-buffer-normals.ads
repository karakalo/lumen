with
     opengl.Buffer.general;

package opengl.Buffer.normals is new opengl.Buffer.general (base_object   => opengl.Buffer.array_Object,
                                                            index         => opengl.Index_t,
                                                            element       => opengl.Normal,
                                                            element_array => opengl.Normals);
