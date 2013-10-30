with
     glx.Pointers;

package GLX.pointer_Pointers
is
   use glx.Pointers;

   type VisualID_Pointer_Pointer           is access all VisualID_Pointer;
   type XVisualInfo_Pointer_Pointer        is access all XVisualInfo_Pointer;
   type Pixmap_Pointer_Pointer             is access all Pixmap_Pointer;
   type Font_Pointer_Pointer               is access all Font_Pointer;
   type Window_Pointer_Pointer             is access all Window_Pointer;
   type Bool_Pointer_Pointer               is access all Bool_Pointer;
   type a_a_GLXcontextRec_Pointer_Pointer  is access all GLXcontextRec_Pointer;
   type XID_Pointer_Pointer                is access all XID_Pointer;
   type GLXPixmap_Pointer_Pointer          is access all GLXPixmap_Pointer;
   type GLXDrawable_Pointer_Pointer        is access all GLXDrawable_Pointer;
   type p_a_GLXFBConfigRec_Pointer_Pointer is access all p_a_GLXFBConfigRec_Pointer;
   type GLXFBConfig_Pointer_Pointer        is access all GLXFBConfig_Pointer;
   type GLXFBConfigID_Pointer_Pointer      is access all GLXFBConfigID_Pointer;
   type GLXContextID_Pointer_Pointer       is access all GLXContextID_Pointer;
   type GLXWindow_Pointer_Pointer          is access all GLXWindow_Pointer;
   type GLXPbuffer_Pointer_Pointer         is access all GLXPbuffer_Pointer;

end GLX.pointer_Pointers;
