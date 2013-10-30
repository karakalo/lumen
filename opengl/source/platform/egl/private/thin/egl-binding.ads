with
     eGL.Pointers,
     eGL.NativeDisplayType,

     Interfaces.C.Strings,
     System;


package eGL.Binding
is

   function eglGetError                             return eGL.EGLint;

   function eglGetDisplay
     (display_id : in eGL.NativeDisplayType.Item)   return eGL.EGLDisplay;

   function eglInitialize
     (dpy   : in eGL.EGLDisplay;
      major : in eGL.Pointers.EGLint_Pointer;
      minor : in eGL.Pointers.EGLint_Pointer)       return eGL.EGLBoolean;

   function eglTerminate (dpy : in eGL.EGLDisplay)  return eGL.EGLBoolean;

   function eglQueryString
     (dpy  : in eGL.EGLDisplay;
      name : in eGL.EGLint)                         return Interfaces.C.Strings.chars_ptr;

   function eglGetConfigs
     (dpy         : in eGL.EGLDisplay;
      configs     : in eGL.Pointers.EGLConfig_Pointer;
      config_size : in eGL.EGLint;
      num_config  : in eGL.Pointers.EGLint_Pointer) return eGL.EGLBoolean;

   function eglChooseConfig
     (dpy         : in eGL.EGLDisplay;
      attrib_list : in eGL.Pointers.EGLint_Pointer;
      configs     : in eGL.Pointers.EGLConfig_Pointer;
      config_size : in eGL.EGLint;
      num_config  : in eGL.Pointers.EGLint_Pointer) return eGL.EGLBoolean;

   function eglGetConfigAttrib
     (dpy       : in eGL.EGLDisplay;
      config    : in eGL.EGLConfig;
      attribute : in eGL.EGLint;
      value     : in eGL.Pointers.EGLint_Pointer)   return eGL.EGLBoolean;

   function eglCreateWindowSurface
     (dpy         : in eGL.EGLDisplay;
      config      : in eGL.EGLConfig;
      win         : in eGL.NativeWindowType;
      attrib_list : in eGL.Pointers.EGLint_Pointer) return eGL.EGLSurface;

   function eglCreatePbufferSurface
     (dpy         : in eGL.EGLDisplay;
      config      : in eGL.EGLConfig;
      attrib_list : in eGL.Pointers.EGLint_Pointer) return eGL.EGLSurface;

   function eglCreatePixmapSurface
     (dpy         : in eGL.EGLDisplay;
      config      : in eGL.EGLConfig;
      pixmap      : in eGL.NativePixmapType;
      attrib_list : in eGL.Pointers.EGLint_Pointer) return eGL.EGLSurface;

   function eglDestroySurface
     (dpy     : in eGL.EGLDisplay;
      surface : in eGL.EGLSurface)                  return eGL.EGLBoolean;

   function eglQuerySurface
     (dpy       : in eGL.EGLDisplay;
      surface   : in eGL.EGLSurface;
      attribute : in eGL.EGLint;
      value     : in eGL.Pointers.EGLint_Pointer)   return eGL.EGLBoolean;

   function eglBindAPI (api : in eGL.EGLenum)       return eGL.EGLBoolean;

   function eglQueryAPI                             return eGL.EGLenum;

   function eglWaitClient                           return eGL.EGLBoolean;

   function eglReleaseThread                        return eGL.EGLBoolean;

   function eglCreatePbufferFromClientBuffer
     (dpy         : in eGL.EGLDisplay;
      buftype     : in eGL.EGLenum;
      buffer      : in eGL.EGLClientBuffer;
      config      : in eGL.EGLConfig;
      attrib_list : in eGL.Pointers.EGLint_Pointer) return eGL.EGLSurface;

   function eglSurfaceAttrib
     (dpy       : in eGL.EGLDisplay;
      surface   : in eGL.EGLSurface;
      attribute : in eGL.EGLint;
      value     : in eGL.EGLint)                    return eGL.EGLBoolean;

   function eglBindTexImage
     (dpy     : in eGL.EGLDisplay;
      surface : in eGL.EGLSurface;
      buffer  : in eGL.EGLint)                      return eGL.EGLBoolean;

   function eglReleaseTexImage
     (dpy     : in eGL.EGLDisplay;
      surface : in eGL.EGLSurface;
      buffer  : in eGL.EGLint)                      return eGL.EGLBoolean;

   function eglSwapInterval
     (dpy      : in eGL.EGLDisplay;
      interval : in eGL.EGLint)
      return     eGL.EGLBoolean;

   function eglCreateContext
     (dpy           : in eGL.EGLDisplay;
      config        : in eGL.EGLConfig;
      share_context : in eGL.EGLContext;
      attrib_list   : in eGL.Pointers.EGLint_Pointer) return eGL.EGLContext;

   function eglDestroyContext
     (dpy  : in eGL.EGLDisplay;
      ctx  : in eGL.EGLContext)                       return eGL.EGLBoolean;

   function eglMakeCurrent
     (dpy  : in eGL.EGLDisplay;
      draw : in eGL.EGLSurface;
      read : in eGL.EGLSurface;
      ctx  : in eGL.EGLContext)                       return eGL.EGLBoolean;

   function eglGetCurrentContext                      return eGL.EGLContext;

   function eglGetCurrentSurface
     (readdraw : in eGL.EGLint)                       return eGL.EGLSurface;

   function eglGetCurrentDisplay                      return eGL.EGLDisplay;

   function eglQueryContext
     (dpy       : in eGL.EGLDisplay;
      ctx       : in eGL.EGLContext;
      attribute : in eGL.EGLint;
      value     : in eGL.Pointers.EGLint_Pointer)     return eGL.EGLBoolean;

   function eglWaitGL                                 return eGL.EGLBoolean;

   function eglWaitNative (engine : in eGL.EGLint)    return eGL.EGLBoolean;

   function eglSwapBuffers
     (dpy     : in eGL.EGLDisplay;
      surface : in eGL.EGLSurface)                    return eGL.EGLBoolean;

   function eglCopyBuffers
     (dpy     : in eGL.EGLDisplay;
      surface : in eGL.EGLSurface;
      target  : in eGL.NativePixmapType)              return eGL.EGLBoolean;

   function eglGetProcAddress
     (procname : in Interfaces.C.Strings.chars_ptr)   return void_ptr;


   --  Out-of-band handle values.
   --
   egl_DEFAULT_DISPLAY : constant access eGL.Display;
   egl_NO_CONTEXT      : constant        eGL.EGLContext;
   egl_NO_DISPLAY      : constant        eGL.EGLDisplay;
   egl_NO_SURFACE      : constant        eGL.EGLSurface;

   --  Out-of-band attribute value.
   --
   egl_DONT_CARE       : constant        eGL.EGLint;




private
   use System;

   egl_DEFAULT_DISPLAY : constant access eGL.Display := null;
   egl_NO_CONTEXT      : constant eGL.EGLContext     := null_Address;
   egl_NO_DISPLAY      : constant  eGL.EGLDisplay    := null_Address;
   egl_NO_SURFACE      : constant  eGL.EGLSurface    := null_Address;

   egl_DONT_CARE       : constant  eGL.EGLint := -1;


   pragma Import (C, eglGetError,             "eglGetError");
   pragma Import (C, eglGetDisplay,           "eglGetDisplay");
   pragma Import (C, eglInitialize,           "eglInitialize");
   pragma Import (C, eglTerminate,            "eglTerminate");
   pragma Import (C, eglQueryString,          "eglQueryString");
   pragma Import (C, eglGetConfigs,           "eglGetConfigs");
   pragma Import (C, eglChooseConfig,         "eglChooseConfig");
   pragma Import (C, eglGetConfigAttrib,      "eglGetConfigAttrib");
   pragma Import (C, eglCreateWindowSurface,  "eglCreateWindowSurface");
   pragma Import (C, eglCreatePbufferSurface, "eglCreatePbufferSurface");
   pragma Import (C, eglCreatePixmapSurface,  "eglCreatePixmapSurface");
   pragma Import (C, eglDestroySurface,       "eglDestroySurface");
   pragma Import (C, eglQuerySurface,         "eglQuerySurface");
   pragma Import (C, eglBindAPI,              "eglBindAPI");
   pragma Import (C, eglQueryAPI,             "eglQueryAPI");
   pragma Import (C, eglWaitClient,           "eglWaitClient");
   pragma Import (C, eglReleaseThread,        "eglReleaseThread");
   pragma Import (C, eglCreatePbufferFromClientBuffer,
                                              "eglCreatePbufferFromClientBuffer");
   pragma Import (C, eglSurfaceAttrib,        "eglSurfaceAttrib");
   pragma Import (C, eglBindTexImage,         "eglBindTexImage");
   pragma Import (C, eglReleaseTexImage,      "eglReleaseTexImage");
   pragma Import (C, eglSwapInterval,         "eglSwapInterval");
   pragma Import (C, eglCreateContext,        "eglCreateContext");
   pragma Import (C, eglDestroyContext,       "eglDestroyContext");
   pragma Import (C, eglMakeCurrent,          "eglMakeCurrent");
   pragma Import (C, eglGetCurrentContext,    "eglGetCurrentContext");
   pragma Import (C, eglGetCurrentSurface,    "eglGetCurrentSurface");
   pragma Import (C, eglGetCurrentDisplay,    "eglGetCurrentDisplay");
   pragma Import (C, eglQueryContext,         "eglQueryContext");
   pragma Import (C, eglWaitGL,               "eglWaitGL");
   pragma Import (C, eglWaitNative,           "eglWaitNative");
   pragma Import (C, eglSwapBuffers,          "eglSwapBuffers");
   pragma Import (C, eglCopyBuffers,          "eglCopyBuffers");
   pragma Import (C, eglGetProcAddress,       "eglGetProcAddress");

end eGL.Binding;
