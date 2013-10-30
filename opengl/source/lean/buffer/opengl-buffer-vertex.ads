with
     opengl.Buffer.general;

package opengl.Buffer.vertex is new opengl.Buffer.general (base_object   => opengl.Buffer.array_Object,
                                                           index         => opengl.Index_t,
                                                           element       => opengl.Site,
                                                           element_array => opengl.Sites);
