
package GLext.Pointers is

   -- GLchar_Pointer
   --
   type GLchar_Pointer is access all glext.GLchar;

   type GLchar_Pointer_array is
     array (C.size_t range <>)
            of aliased GLchar_Pointer;

   -- GLintptr_Pointer
   --
   type GLintptr_Pointer is access all glext.GLintptr;

   type GLintptr_Pointer_array is
     array (C.size_t range <>)
            of aliased GLintptr_Pointer;

   -- GLsizeiptr_Pointer
   --
   type GLsizeiptr_Pointer is access all glext.GLsizeiptr;

   type GLsizeiptr_Pointer_array is
     array (C.size_t range <>)
            of aliased GLsizeiptr_Pointer;


   -- GLint64_Pointer
   --
   type GLint64_Pointer is access all glext.GLint64;

   type GLint64_Pointer_array is
     array (C.size_t range <>)
            of aliased GLint64_Pointer;


   -- GLuint64_Pointer
   --
   type GLuint64_Pointer is access all glext.GLuint64;

   type GLuint64_Pointer_array is
     array (C.size_t range <>)
            of aliased GLuint64_Pointer;

end GLext.Pointers;
