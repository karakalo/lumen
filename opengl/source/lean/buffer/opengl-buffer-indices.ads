with
     opengl.Buffer.general;

package opengl.Buffer.indices is new opengl.Buffer.general (base_object   => opengl.Buffer.element_array_Object,
                                                            index         => opengl.long_Index_t,
                                                            element       => opengl.Index_t,
                                                            element_array => opengl.Indices);
