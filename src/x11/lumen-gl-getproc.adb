pragma Ada_2005;

with Ada.Finalization;
with Interfaces.C;
with Interfaces.C.Strings;

package body Lumen.GL.GetProc is

   FailedGetProcInitialization : Exception;

   type LibHandle_Type is null record;
   type LibHandle_Access is access LibHandle_Type;

   function dlopen
     (filename : Interfaces.C.Strings.chars_ptr;
      flag     : Interfaces.C.int)
      return LibHandle_Access;
   pragma Import(C,dlopen,"dlopen");

   function dlsym
     (handle : LibHandle_Access;
      Symbol : Interfaces.C.Strings.chars_ptr)
      return System.Address;
   pragma Import(C,dlsym,"dlsym");

   function dlclose
     (handle : LibHandle_Access)
      return Interfaces.C.int;
   pragma Import(C,dlclose,"dlclose");

   type GLLib_Type is new Ada.Finalization.Controlled with
      record
         LibHandle : LibHandle_Access;
      end record;

   overriding
   procedure Initialize
     (Object : in out GLLib_Type);

   overriding
   procedure Finalize
     (Object : in out GLLib_Type);
   ---------------------------------------------------------------------------

   procedure Initialize
     (Object : in out GLLib_Type) is
      CLibName : Interfaces.C.Strings.chars_ptr;
   begin
      CLibName:=Interfaces.C.Strings.New_String("libGL.so");
      Object.LibHandle:=dlopen(CLibName,1);
      Interfaces.C.Strings.Free(CLibName);
      if Object.LibHandle=null then
         raise FailedGetProcInitialization;
      end if;
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Object : in out GLLib_Type) is
      Error: Interfaces.C.int;
      pragma Unreferenced(Error);
   begin
      Error:=dlclose(Object.LibHandle);
   end Finalize;
   ---------------------------------------------------------------------------

   GLLib : GLLib_Type;

   function GetProcAddress
     (Name : String)
      return System.Address is

      Result : System.Address;
      CName  : Interfaces.C.Strings.chars_ptr;

   begin
      CName:=Interfaces.C.Strings.New_String(Name);
      Result:=dlsym(GLLib.LibHandle,CName);
      Interfaces.C.Strings.Free(CName);
      return Result;
   end GetProcAddress;
   ---------------------------------------------------------------------------

end Lumen.GL.GetProc;
