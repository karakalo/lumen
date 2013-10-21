package GLX.Pointers
is

   --  VisualID_Pointer
   --
   type VisualID_Pointer  is access all GLX.VisualID;
   type VisualID_Pointers is array (C.size_t range <>) of aliased VisualID_Pointer;

   --  XVisualInfo_Pointer
   --
   type XVisualInfo_Pointer is access all GLX.XVisualInfo;
   type XVisualInfo_Pointers is array (C.size_t range <>) of aliased XVisualInfo_Pointer;

   --  Pixmap_Pointer
   --
   type Pixmap_Pointer  is access all GLX.Pixmap;
   type Pixmap_Pointers is array (C.size_t range <>) of aliased Pixmap_Pointer;

   --  Font_Pointer
   --
   type Font_Pointer  is access all GLX.Font;
   type Font_Pointers is array (C.size_t range <>) of aliased Font_Pointer;

   --  Window_Pointer
   --
   type Window_Pointer  is access all GLX.Window;
   type Window_Pointers is array (C.size_t range <>) of aliased Window_Pointer;

   --  Bool_Pointer
   --
   type Bool_Pointer  is access all GLX.Bool;
   type Bool_Pointers is array (Interfaces.C.size_t range <>) of aliased Bool_Pointer;

   --  a_a_GLXcontextRec_Pointer
   --
   type GLXcontextRec_Pointer  is access all GLX.GLXcontextRec;
   type a_a_GLXcontextRec_Pointers is array (C.size_t range <>) of aliased GLXcontextRec_Pointer;

   --  XID_Pointer
   --
   type XID_Pointer  is access all GLX.XID;
   type XID_Pointers is array (C.size_t range <>) of aliased XID_Pointer;

   --  GLXPixmap_Pointer
   --
   type GLXPixmap_Pointer  is access all GLX.GLXPixmap;
   type GLXPixmap_Pointers is array (C.size_t range <>) of aliased GLXPixmap_Pointer;

   --  GLXDrawable_Pointer
   --
   type GLXDrawable_Pointer  is access all GLX.GLXDrawable;
   type GLXDrawable_Pointers is array (C.size_t range <>) of aliased GLXDrawable_Pointer;

   --  p._a_GLXFBConfigRec_Pointer
   --
   type p_a_GLXFBConfigRec_Pointer  is access all GLX.p_a_GLXFBConfigRec;
   type p_a_GLXFBConfigRec_Pointers is array (C.size_t range <>) of aliased p_a_GLXFBConfigRec_Pointer;

   --  GLXFBConfig_Pointer
   --
   type GLXFBConfig_Pointer  is access all GLX.GLXFBConfig;
   type GLXFBConfig_Pointers is array (C.size_t range <>) of aliased GLXFBConfig_Pointer;

   --  GLXFBConfigID_Pointer
   --
   type GLXFBConfigID_Pointer  is access all GLX.GLXFBConfigID;
   type GLXFBConfigID_Pointers is array (C.size_t range <>) of aliased GLXFBConfigID_Pointer;

   --  GLXContextID_Pointer
   --
   type GLXContextID_Pointer  is access all GLX.GLXContextID;
   type GLXContextID_Pointers is array (C.size_t range <>) of aliased GLXContextID_Pointer;

   --  GLXWindow_Pointer
   --
   type GLXWindow_Pointer  is access all GLX.GLXWindow;
   type GLXWindow_Pointers is array (C.size_t range <>) of aliased GLXWindow_Pointer;

   --  GLXPbuffer_Pointer
   --
   type GLXPbuffer_Pointer  is access all GLX.GLXPbuffer;
   type GLXPbuffer_Pointers is array (C.size_t range <>) of aliased GLXPbuffer_Pointer;

end GLX.Pointers;
