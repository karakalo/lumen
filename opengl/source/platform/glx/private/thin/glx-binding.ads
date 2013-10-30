with
     GLX.Pointers,
     Interfaces.C;

package GLX.Binding
is

--     function glXChooseVisual
--       (dpy        : in xcb.Pointers.Display_Pointer;
--        screen     : in Interfaces.C.int;
--        attribList : in Swig.Pointers.int_Pointer)
--        return       access GLX.XVisualInfo;
--
--     function glXCreateContext
--       (dpy       : in xcb.Pointers.Display_Pointer;
--        vis       : in GLX.Pointers.XVisualInfo_Pointer;
--        shareList : in GLX.GLXContext.Item;
--        direct    : in GLX.Bool)
--        return      access GLX.a_a_GLXcontextRec;
--
--     procedure glXDestroyContext
--       (dpy : in xcb.Pointers.Display_Pointer;
--        ctx : in GLX.GLXContext.Item);
--
--     function glXMakeCurrent
--       (dpy      : in xcb.Pointers.Display_Pointer;
--        drawable : in GLX.GLXDrawable;
--        ctx      : in GLX.GLXContext.Item)
--        return     GLX.Bool;
--
--     procedure glXCopyContext
--       (dpy  : in xcb.Pointers.Display_Pointer;
--        src  : in GLX.GLXContext.Item;
--        dst  : in GLX.GLXContext.Item;
--        mask : in Interfaces.C.unsigned_long);
--
--     procedure glXSwapBuffers
--       (dpy      : in xcb.Pointers.Display_Pointer;
--        drawable : in GLX.GLXDrawable);
--
--     function glXCreateGLXPixmap
--       (dpy    : in xcb.Pointers.Display_Pointer;
--        visual : in GLX.Pointers.XVisualInfo_Pointer;
--        pixmap : in GLX.Pixmap)
--        return   GLX.GLXPixmap;
--
--     procedure glXDestroyGLXPixmap
--       (dpy    : in xcb.Pointers.Display_Pointer;
--        pixmap : in GLX.GLXPixmap);
--
--     function glXQueryExtension
--       (dpy    : in xcb.Pointers.Display_Pointer;
--        errorb : in Swig.Pointers.int_Pointer;
--        event  : in Swig.Pointers.int_Pointer)
--        return   GLX.Bool;
--
--     function glXQueryVersion
--       (dpy  : in xcb.Pointers.Display_Pointer;
--        maj  : in Swig.Pointers.int_Pointer;
--        min  : in Swig.Pointers.int_Pointer)
--        return GLX.Bool;
--
--     function glXIsDirect
--       (dpy  : in xcb.Pointers.Display_Pointer;
--        ctx  : in GLX.GLXContext.Item)
--        return GLX.Bool;
--
--     function glXGetConfig
--       (dpy    : in xcb.Pointers.Display_Pointer;
--        visual : in GLX.Pointers.XVisualInfo_Pointer;
--        attrib : in Interfaces.C.int;
--        value  : in Swig.Pointers.int_Pointer)
--        return   Interfaces.C.int;

   function  glXGetCurrentContext  return access GLX.GLXcontextRec;
   function  glXGetCurrentDrawable return  GLX.GLXDrawable;

   procedure glXWaitGL;
   procedure glXWaitX;

   procedure glXUseXFont (font  : in GLX.Font;
                          first : in Interfaces.C.int;
                          count : in Interfaces.C.int;
                          list  : in Interfaces.C.int);

--     function glXQueryExtensionsString
--       (dpy    : in xcb.Pointers.Display_Pointer;
--        screen : in Interfaces.C.int)
--        return   Interfaces.C.Strings.chars_ptr;
--
--     function glXQueryServerString
--       (dpy    : in xcb.Pointers.Display_Pointer;
--        screen : in Interfaces.C.int;
--        name   : in Interfaces.C.int)
--        return   Interfaces.C.Strings.chars_ptr;
--
--     function glXGetClientString
--       (dpy  : in xcb.Pointers.Display_Pointer;
--        name : in Interfaces.C.int)
--        return Interfaces.C.Strings.chars_ptr;
--
--     function glXGetCurrentDisplay return access xcb.Display;
--
--     function glXChooseFBConfig
--       (dpy        : in xcb.Pointers.Display_Pointer;
--        screen     : in Interfaces.C.int;
--        attribList : in Swig.Pointers.int_Pointer;
--        nitems     : in Swig.Pointers.int_Pointer)
--        return       access GLX.GLXFBConfig;
--
--     function glXGetFBConfigAttrib
--       (dpy       : in xcb.Pointers.Display_Pointer;
--        config    : in GLX.GLXFBConfig;
--        attribute : in Interfaces.C.int;
--        value     : in Swig.Pointers.int_Pointer)
--        return      Interfaces.C.int;
--
--     function glXGetFBConfigs
--       (dpy       : in xcb.Pointers.Display_Pointer;
--        screen    : in Interfaces.C.int;
--        nelements : in Swig.Pointers.int_Pointer)
--        return      GLX.GLXFBConfig;
--
--     function glXGetVisualFromFBConfig
--       (dpy    : in xcb.Pointers.Display_Pointer;
--        config : in GLX.GLXFBConfig)
--        return   access GLX.XVisualInfo;
--
--     function glXCreateWindow
--       (dpy        : in xcb.Pointers.Display_Pointer;
--        config     : in GLX.GLXFBConfig;
--        win        : in GLX.Window;
--        attribList : in Swig.Pointers.int_Pointer)
--        return       GLX.GLXWindow;
--
--     procedure glXDestroyWindow
--       (dpy    : in xcb.Pointers.Display_Pointer;
--        window : in GLX.GLXWindow);
--
--     function glXCreatePixmap
--       (dpy        : in xcb.Pointers.Display_Pointer;
--        config     : in GLX.GLXFBConfig;
--        pixmap     : in GLX.Pixmap;
--        attribList : in Swig.Pointers.int_Pointer)
--        return       GLX.GLXPixmap;
--
--     procedure glXDestroyPixmap
--       (dpy    : in xcb.Pointers.Display_Pointer;
--        pixmap : in GLX.GLXPixmap);
--
--     function glXCreatePbuffer
--       (dpy        : in xcb.Pointers.Display_Pointer;
--        config     : in GLX.GLXFBConfig;
--        attribList : in Swig.Pointers.int_Pointer)
--        return       GLX.GLXPbuffer;
--
--     procedure glXDestroyPbuffer
--       (dpy  : in xcb.Pointers.Display_Pointer;
--        pbuf : in GLX.GLXPbuffer);
--
--     procedure glXQueryDrawable
--       (dpy       : in xcb.Pointers.Display_Pointer;
--        draw      : in GLX.GLXDrawable;
--        attribute : in Interfaces.C.int;
--        value     : in Swig.Pointers.unsigned_Pointer);
--
--     function glXCreateNewContext
--       (dpy        : in xcb.Pointers.Display_Pointer;
--        config     : in GLX.GLXFBConfig;
--        renderType : in Interfaces.C.int;
--        shareList  : in GLX.GLXContext.Item;
--        direct     : in GLX.Bool)
--        return       access GLX.a_a_GLXcontextRec;
--
--     function glXMakeContextCurrent
--       (dpy  : in xcb.Pointers.Display_Pointer;
--        draw : in GLX.GLXDrawable;
--        read : in GLX.GLXDrawable;
--        ctx  : in GLX.GLXContext.Item)
--        return GLX.Bool;

   function glXGetCurrentReadDrawable return  GLX.GLXDrawable;

--     function glXQueryContext
--       (dpy       : in xcb.Pointers.Display_Pointer;
--        ctx       : in GLX.GLXContext.Item;
--        attribute : in Interfaces.C.int;
--        value     : in Swig.Pointers.int_Pointer)
--        return      Interfaces.C.int;
--
--     procedure glXSelectEvent
--       (dpy      : in xcb.Pointers.Display_Pointer;
--        drawable : in GLX.GLXDrawable;
--        mask     : in Interfaces.C.unsigned_long);
--
--     function glXBindTexImageARB
--       (dpy     : in xcb.Pointers.Display_Pointer;
--        pbuffer : in GLX.GLXPbuffer;
--        buffer  : in Interfaces.C.int)
--        return    GLX.Bool;
--
--     function glXReleaseTexImageARB
--       (dpy     : in xcb.Pointers.Display_Pointer;
--        pbuffer : in GLX.GLXPbuffer;
--        buffer  : in Interfaces.C.int)
--        return    GLX.Bool;
--
--     function glXDrawableAttribARB
--       (dpy        : in xcb.Pointers.Display_Pointer;
--        draw       : in GLX.GLXDrawable;
--        attribList : in Swig.Pointers.int_Pointer)
--        return       GLX.Bool;
--
--     procedure glXBindTexImageEXT
--       (dpy         : in xcb.Pointers.Display_Pointer;
--        drawable    : in GLX.GLXDrawable;
--        buffer      : in Interfaces.C.int;
--        attrib_list : in Swig.Pointers.int_Pointer);
--
--     procedure glXReleaseTexImageEXT
--       (dpy      : in xcb.Pointers.Display_Pointer;
--        drawable : in GLX.GLXDrawable;
--        buffer   : in Interfaces.C.int);

   function get_visualid (Self : in GLX.Pointers.XVisualInfo_Pointer) return GLX.VisualID;



private

--     pragma Import (C, glXChooseVisual, "Ada_glXChooseVisual");
--     pragma Import (C, glXCreateContext, "Ada_glXCreateContext");
--     pragma Import (C, glXDestroyContext, "Ada_glXDestroyContext");
--     pragma Import (C, glXMakeCurrent, "Ada_glXMakeCurrent");
--     pragma Import (C, glXCopyContext, "Ada_glXCopyContext");
--     pragma Import (C, glXSwapBuffers, "Ada_glXSwapBuffers");
--     pragma Import (C, glXCreateGLXPixmap, "Ada_glXCreateGLXPixmap");
--     pragma Import (C, glXDestroyGLXPixmap, "Ada_glXDestroyGLXPixmap");
--     pragma Import (C, glXQueryExtension, "Ada_glXQueryExtension");
--     pragma Import (C, glXQueryVersion, "Ada_glXQueryVersion");
--     pragma Import (C, glXIsDirect, "Ada_glXIsDirect");
--     pragma Import (C, glXGetConfig, "Ada_glXGetConfig");

   pragma Import (C, glXGetCurrentContext,  "glXGetCurrentContext");
   pragma Import (C, glXGetCurrentDrawable, "glXGetCurrentDrawable");
   pragma Import (C, glXWaitGL,             "glXWaitGL");
   pragma Import (C, glXWaitX,              "glXWaitX");
   pragma Import (C, glXUseXFont,           "glXUseXFont");

--     pragma Import (C, glXQueryExtensionsString, "Ada_glXQueryExtensionsString");
--     pragma Import (C, glXQueryServerString, "Ada_glXQueryServerString");
--     pragma Import (C, glXGetClientString, "Ada_glXGetClientString");
--     pragma Import (C, glXGetCurrentDisplay, "Ada_glXGetCurrentDisplay");
--     pragma Import (C, glXChooseFBConfig, "Ada_glXChooseFBConfig");
--     pragma Import (C, glXGetFBConfigAttrib, "Ada_glXGetFBConfigAttrib");
--     pragma Import (C, glXGetFBConfigs, "Ada_glXGetFBConfigs");
--     pragma Import (C, glXGetVisualFromFBConfig, "Ada_glXGetVisualFromFBConfig");
--     pragma Import (C, glXCreateWindow, "Ada_glXCreateWindow");
--     pragma Import (C, glXDestroyWindow, "Ada_glXDestroyWindow");
--     pragma Import (C, glXCreatePixmap, "Ada_glXCreatePixmap");
--     pragma Import (C, glXDestroyPixmap, "Ada_glXDestroyPixmap");
--     pragma Import (C, glXCreatePbuffer, "Ada_glXCreatePbuffer");
--     pragma Import (C, glXDestroyPbuffer, "Ada_glXDestroyPbuffer");
--     pragma Import (C, glXQueryDrawable, "Ada_glXQueryDrawable");
--     pragma Import (C, glXCreateNewContext, "Ada_glXCreateNewContext");
--     pragma Import (C, glXMakeContextCurrent, "Ada_glXMakeContextCurrent");

   pragma Import (C, glXGetCurrentReadDrawable, "glXGetCurrentReadDrawable");

--     pragma Import (C, glXQueryContext, "Ada_glXQueryContext");
--     pragma Import (C, glXSelectEvent, "Ada_glXSelectEvent");
--     pragma Import (C, glXBindTexImageARB, "Ada_glXBindTexImageARB");
--     pragma Import (C, glXReleaseTexImageARB, "Ada_glXReleaseTexImageARB");
--     pragma Import (C, glXDrawableAttribARB, "Ada_glXDrawableAttribARB");
--     pragma Import (C, glXBindTexImageEXT, "Ada_glXBindTexImageEXT");
--     pragma Import (C, glXReleaseTexImageEXT, "Ada_glXReleaseTexImageEXT");
   pragma Import (C, get_visualid, "Ada_get_visualid");

end GLX.Binding;
