with
     GL.lean,
     Interfaces.C.Strings,
     System;


package GL.Pointers
--
--  Provides pointer conversions.
--
is
   use Interfaces;

   function to_GLvoid_access (From : in     system.Address)      return access GLvoid;
   function to_GLvoid_access (From : access C.unsigned_char)     return access GLvoid;
   function to_GLchar_access (From : in     C.Strings.chars_ptr) return access lean.GLchar;

   function to_GLchar_Pointer_access
                             (From : access interfaces.c.Strings.chars_ptr_array)
                                                                 return access lean.GLchar_Pointer;

   function "+" (From : in     system.Address)      return access GLvoid         renames to_GLvoid_access;
   function "+" (From : access C.unsigned_char)     return access GLvoid         renames to_GLvoid_access;
   function "+" (From : in     C.Strings.chars_ptr) return access lean.GLchar renames to_GLchar_access;

end GL.Pointers;
