with
     Ada.Unchecked_Conversion,
     system.Address_To_Access_Conversions;


package body GL.Pointers
is

   type GLvoid_access         is access all GLvoid;
   type GLchar_access         is access all lean.GLchar;
   type GLchar_Pointer_access is access all lean.GLchar_Pointer;
   type chars_ptr_access      is access all C.strings.chars_ptr;


   package Conversions is new system.Address_To_Access_Conversions (GLvoid);



   function to_GLvoid_access (From : in system.Address) return access GLvoid
   is
   begin
      return conversions.To_Pointer (From);
   end;



   function to_GLvoid_access (From : access C.unsigned_char) return access GLvoid
   is
      type unsigned_char_access is access all C.unsigned_char;
      function Convert is new ada.unchecked_Conversion (unsigned_char_access, GLvoid_access);
   begin
      return Convert (unsigned_char_access (From));
   end;



   function to_GLchar_access (From : in C.Strings.chars_ptr) return access lean.GLchar
   is
      function Convert is new ada.unchecked_Conversion (C.Strings.chars_ptr, GLchar_access);
   begin
      return Convert (From);
   end;



   function to_GLchar_Pointer_access (From : access C.Strings.chars_ptr_array) return access lean.GLchar_Pointer
   is
      function Convert is new ada.unchecked_Conversion (chars_ptr_access, GLchar_Pointer_access);
   begin
      return Convert (From (From'First)'unchecked_Access);
   end;

end GL.Pointers;
