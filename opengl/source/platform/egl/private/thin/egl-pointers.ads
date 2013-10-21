package eGL.Pointers
is

   type Display_Pointer          is access all eGL.Display;
   type NativeWindowType_Pointer is access all eGL.NativeWindowType;
   type NativePixmapType_Pointer is access all eGL.NativePixmapType;
   type EGLint_Pointer           is access all eGL.EGLint;
   type EGLBoolean_Pointer       is access all eGL.EGLBoolean;
   type EGLenum_Pointer          is access all eGL.EGLenum;
   type EGLConfig_Pointer        is access all eGL.EGLConfig;
   type EGLContext_Pointer       is access all eGL.EGLContext;
   type EGLDisplay_Pointer       is access all eGL.EGLDisplay;
   type EGLSurface_Pointer       is access all eGL.EGLSurface;
   type EGLClientBuffer_Pointer  is access all eGL.EGLClientBuffer;

   type Display_Pointer_array          is array (C.size_t range <>) of aliased Display_Pointer;
   type NativeWindowType_Pointer_array is array (C.size_t range <>) of aliased NativeWindowType_Pointer;
   type NativePixmapType_Pointer_array is array (C.size_t range <>) of aliased NativePixmapType_Pointer;
   type EGLint_Pointer_array           is array (C.size_t range <>) of aliased EGLint_Pointer;
   type EGLBoolean_Pointer_array       is array (C.size_t range <>) of aliased EGLBoolean_Pointer;
   type EGLenum_Pointer_array          is array (C.size_t range <>) of aliased EGLenum_Pointer;
   type EGLConfig_Pointer_array        is array (C.size_t range <>) of aliased EGLConfig_Pointer;
   type EGLContext_Pointer_array       is array (C.size_t range <>) of aliased EGLContext_Pointer;
   type EGLDisplay_Pointer_array       is array (C.size_t range <>) of aliased EGLDisplay_Pointer;
   type EGLSurface_Pointer_array       is array (C.size_t range <>) of aliased EGLSurface_Pointer;
   type EGLClientBuffer_Pointer_array  is array (C.size_t range <>) of aliased EGLClientBuffer_Pointer;

end eGL.Pointers;
