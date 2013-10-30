with GLX.Pointers;

package GLX.GLXContext
is

   subtype Item is GLX.Pointers.GLXcontextRec_Pointer;


   type Pointer         is access all Item;
   type Pointer_Pointer is access all Pointer;

   type Items    is array (C.size_t range <>) of aliased Item;
   type Pointers is array (C.size_t range <>) of aliased Pointer;

end GLX.GLXContext;
