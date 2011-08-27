with GL.Pointers;



package GL.Binding is

   use Pointers;


   procedure glClearIndex (c : in GLfloat);

   procedure glClearColor
     (red   : in GLclampf;
      green : in GLclampf;
      blue  : in GLclampf;
      alpha : in GLclampf);

   procedure glClear (mask : in GLbitfield);

   procedure glIndexMask (mask : in GLuint);

   procedure glColorMask
     (red   : in GLboolean;
      green : in GLboolean;
      blue  : in GLboolean;
      alpha : in GLboolean);

   procedure glAlphaFunc (func : in GLenum; ref : in GLclampf);

   procedure glBlendFunc (sfactor : in GLenum; dfactor : in GLenum);

   procedure glLogicOp (opcode : in GLenum);

   procedure glCullFace (mode : in GLenum);

   procedure glFrontFace (mode : in GLenum);

   procedure glPointSize (size : in GLfloat);

   procedure glLineWidth (width : in GLfloat);

   procedure glLineStipple (factor : in GLint; pattern : in GLushort);

   procedure glPolygonMode (face : in GLenum; mode : in GLenum);

   procedure glPolygonOffset
     (factor : in GLfloat;
      units  : in GLfloat);

   procedure glPolygonStipple (mask : in GLubyte_Pointer);

   procedure glGetPolygonStipple (mask : in GLubyte_Pointer);

   procedure glEdgeFlag (flag : in GLboolean);

   procedure glEdgeFlagv (flag : in GLboolean_Pointer);

   procedure glScissor
     (x      : in GLint;
      y      : in GLint;
      width  : in GLsizei;
      height : in GLsizei);

   procedure glClipPlane
     (plane    : in GLenum;
      equation : in GLdouble_Pointer);

   procedure glGetClipPlane
     (plane    : in GLenum;
      equation : in GLdouble_Pointer);

   procedure glDrawBuffer (mode : in GLenum);

   procedure glReadBuffer (mode : in GLenum);

   procedure glEnable (cap : in GLenum);

   procedure glDisable (cap : in GLenum);

   function glIsEnabled (cap : in GLenum) return GLboolean;

   procedure glEnableClientState (cap : in GLenum);

   procedure glDisableClientState (cap : in GLenum);

   procedure glGetBooleanv
     (pname  : in GLenum;
      params : in GLboolean_Pointer);

   procedure glGetDoublev
     (pname  : in GLenum;
      params : in GLdouble_Pointer);

   procedure glGetFloatv
     (pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glGetIntegerv
     (pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glPushAttrib (mask : in GLbitfield);

   procedure glPopAttrib;

   procedure glPushClientAttrib (mask : in GLbitfield);

   procedure glPopClientAttrib;

   function glRenderMode (mode : in GLenum) return GLint;

   function glGetError return  GLenum;

   function glGetString
     (name : in GLenum)
      return GLubyte_Pointer;

   procedure glFinish;

   procedure glFlush;

   procedure glHint (target : in GLenum; mode : in GLenum);

   procedure glClearDepth (depth : in GLclampd);

   procedure glDepthFunc (func : in GLenum);

   procedure glDepthMask (flag : in GLboolean);

   procedure glDepthRange
     (near_val : in GLclampd;
      far_val  : in GLclampd);

   procedure glClearAccum
     (red   : in GLfloat;
      green : in GLfloat;
      blue  : in GLfloat;
      alpha : in GLfloat);

   procedure glAccum (op : in GLenum; value : in GLfloat);

   procedure glMatrixMode (mode : in GLenum);

   procedure glOrtho
     (left     : in GLdouble;
      right    : in GLdouble;
      bottom   : in GLdouble;
      top      : in GLdouble;
      near_val : in GLdouble;
      far_val  : in GLdouble);

   procedure glFrustum
     (left     : in GLdouble;
      right    : in GLdouble;
      bottom   : in GLdouble;
      top      : in GLdouble;
      near_val : in GLdouble;
      far_val  : in GLdouble);

   procedure glViewport
     (x      : in GLint;
      y      : in GLint;
      width  : in GLsizei;
      height : in GLsizei);

   procedure glPushMatrix;

   procedure glPopMatrix;

   procedure glLoadIdentity;

   procedure glLoadMatrixd (m : in GLdouble_Pointer);

   procedure glLoadMatrixf (m : in GLfloat_Pointer);

   procedure glMultMatrixd (m : in GLdouble_Pointer);

   procedure glMultMatrixf (m : in GLfloat_Pointer);

   procedure glRotated
     (angle : in GLdouble;
      x     : in GLdouble;
      y     : in GLdouble;
      z     : in GLdouble);

   procedure glRotatef
     (angle : in GLfloat;
      x     : in GLfloat;
      y     : in GLfloat;
      z     : in GLfloat);

   procedure glScaled
     (x : in GLdouble;
      y : in GLdouble;
      z : in GLdouble);

   procedure glScalef
     (x : in GLfloat;
      y : in GLfloat;
      z : in GLfloat);

   procedure glTranslated
     (x : in GLdouble;
      y : in GLdouble;
      z : in GLdouble);

   procedure glTranslatef
     (x : in GLfloat;
      y : in GLfloat;
      z : in GLfloat);

   function glIsList (list : in GLuint) return GLboolean;

   procedure glDeleteLists (list : in GLuint; the_range : in GLsizei);

   function glGenLists (the_range : in GLsizei) return GLuint;

   procedure glNewList (list : in GLuint; mode : in GLenum);

   procedure glEndList;

   procedure glCallList (list : in GLuint);

   procedure glCallLists
     (n        : in GLsizei;
      the_type : in GLenum;
      lists    : in GLvoid_Pointer);

   procedure glListBase (base : in GLuint);

   procedure glBegin (mode : in GLenum);

   procedure glEnd;

   procedure glVertex2d (x : in GLdouble; y : in GLdouble);

   procedure glVertex2f (x : in GLfloat; y : in GLfloat);

   procedure glVertex2i (x : in GLint; y : in GLint);

   procedure glVertex2s (x : in GLshort; y : in GLshort);

   procedure glVertex3d
     (x : in GLdouble;
      y : in GLdouble;
      z : in GLdouble);

   procedure glVertex3f
     (x : in GLfloat;
      y : in GLfloat;
      z : in GLfloat);

   procedure glVertex3i (x : in GLint; y : in GLint; z : in GLint);

   procedure glVertex3s
     (x : in GLshort;
      y : in GLshort;
      z : in GLshort);

   procedure glVertex4d
     (x : in GLdouble;
      y : in GLdouble;
      z : in GLdouble;
      w : in GLdouble);

   procedure glVertex4f
     (x : in GLfloat;
      y : in GLfloat;
      z : in GLfloat;
      w : in GLfloat);

   procedure glVertex4i
     (x : in GLint;
      y : in GLint;
      z : in GLint;
      w : in GLint);

   procedure glVertex4s
     (x : in GLshort;
      y : in GLshort;
      z : in GLshort;
      w : in GLshort);

   procedure glVertex2dv (v : in GLdouble_Pointer);

   procedure glVertex2fv (v : in GLfloat_Pointer);

   procedure glVertex2iv (v : in GLint_Pointer);

   procedure glVertex2sv (v : in GLshort_Pointer);

   procedure glVertex3dv (v : in GLdouble_Pointer);

   procedure glVertex3fv (v : in GLfloat_Pointer);

   procedure glVertex3iv (v : in GLint_Pointer);

   procedure glVertex3sv (v : in GLshort_Pointer);

   procedure glVertex4dv (v : in GLdouble_Pointer);

   procedure glVertex4fv (v : in GLfloat_Pointer);

   procedure glVertex4iv (v : in GLint_Pointer);

   procedure glVertex4sv (v : in GLshort_Pointer);

   procedure glNormal3b
     (nx : in GLbyte;
      ny : in GLbyte;
      nz : in GLbyte);

   procedure glNormal3d
     (nx : in GLdouble;
      ny : in GLdouble;
      nz : in GLdouble);

   procedure glNormal3f
     (nx : in GLfloat;
      ny : in GLfloat;
      nz : in GLfloat);

   procedure glNormal3i
     (nx : in GLint;
      ny : in GLint;
      nz : in GLint);

   procedure glNormal3s
     (nx : in GLshort;
      ny : in GLshort;
      nz : in GLshort);

   procedure glNormal3bv (v : in GLbyte_Pointer);

   procedure glNormal3dv (v : in GLdouble_Pointer);

   procedure glNormal3fv (v : in GLfloat_Pointer);

   procedure glNormal3iv (v : in GLint_Pointer);

   procedure glNormal3sv (v : in GLshort_Pointer);

   procedure glIndexd (c : in GLdouble);

   procedure glIndexf (c : in GLfloat);

   procedure glIndexi (c : in GLint);

   procedure glIndexs (c : in GLshort);

   procedure glIndexub (c : in GLubyte);

   procedure glIndexdv (c : in GLdouble_Pointer);

   procedure glIndexfv (c : in GLfloat_Pointer);

   procedure glIndexiv (c : in GLint_Pointer);

   procedure glIndexsv (c : in GLshort_Pointer);

   procedure glIndexubv (c : in GLubyte_Pointer);

   procedure glColor3b
     (red   : in GLbyte;
      green : in GLbyte;
      blue  : in GLbyte);

   procedure glColor3d
     (red   : in GLdouble;
      green : in GLdouble;
      blue  : in GLdouble);

   procedure glColor3f
     (red   : in GLfloat;
      green : in GLfloat;
      blue  : in GLfloat);

   procedure glColor3i
     (red   : in GLint;
      green : in GLint;
      blue  : in GLint);

   procedure glColor3s
     (red   : in GLshort;
      green : in GLshort;
      blue  : in GLshort);

   procedure glColor3ub
     (red   : in GLubyte;
      green : in GLubyte;
      blue  : in GLubyte);

   procedure glColor3ui
     (red   : in GLuint;
      green : in GLuint;
      blue  : in GLuint);

   procedure glColor3us
     (red   : in GLushort;
      green : in GLushort;
      blue  : in GLushort);

   procedure glColor4b
     (red   : in GLbyte;
      green : in GLbyte;
      blue  : in GLbyte;
      alpha : in GLbyte);

   procedure glColor4d
     (red   : in GLdouble;
      green : in GLdouble;
      blue  : in GLdouble;
      alpha : in GLdouble);

   procedure glColor4f
     (red   : in GLfloat;
      green : in GLfloat;
      blue  : in GLfloat;
      alpha : in GLfloat);

   procedure glColor4i
     (red   : in GLint;
      green : in GLint;
      blue  : in GLint;
      alpha : in GLint);

   procedure glColor4s
     (red   : in GLshort;
      green : in GLshort;
      blue  : in GLshort;
      alpha : in GLshort);

   procedure glColor4ub
     (red   : in GLubyte;
      green : in GLubyte;
      blue  : in GLubyte;
      alpha : in GLubyte);

   procedure glColor4ui
     (red   : in GLuint;
      green : in GLuint;
      blue  : in GLuint;
      alpha : in GLuint);

   procedure glColor4us
     (red   : in GLushort;
      green : in GLushort;
      blue  : in GLushort;
      alpha : in GLushort);

   procedure glColor3bv (v : in GLbyte_Pointer);

   procedure glColor3dv (v : in GLdouble_Pointer);

   procedure glColor3fv (v : in GLfloat_Pointer);

   procedure glColor3iv (v : in GLint_Pointer);

   procedure glColor3sv (v : in GLshort_Pointer);

   procedure glColor3ubv (v : in GLubyte_Pointer);

   procedure glColor3uiv (v : in GLuint_Pointer);

   procedure glColor3usv (v : in GLushort_Pointer);

   procedure glColor4bv (v : in GLbyte_Pointer);

   procedure glColor4dv (v : in GLdouble_Pointer);

   procedure glColor4fv (v : in GLfloat_Pointer);

   procedure glColor4iv (v : in GLint_Pointer);

   procedure glColor4sv (v : in GLshort_Pointer);

   procedure glColor4ubv (v : in GLubyte_Pointer);

   procedure glColor4uiv (v : in GLuint_Pointer);

   procedure glColor4usv (v : in GLushort_Pointer);

   procedure glTexCoord1d (s : in GLdouble);

   procedure glTexCoord1f (s : in GLfloat);

   procedure glTexCoord1i (s : in GLint);

   procedure glTexCoord1s (s : in GLshort);

   procedure glTexCoord2d (s : in GLdouble; t : in GLdouble);

   procedure glTexCoord2f (s : in GLfloat; t : in GLfloat);

   procedure glTexCoord2i (s : in GLint; t : in GLint);

   procedure glTexCoord2s (s : in GLshort; t : in GLshort);

   procedure glTexCoord3d
     (s : in GLdouble;
      t : in GLdouble;
      r : in GLdouble);

   procedure glTexCoord3f
     (s : in GLfloat;
      t : in GLfloat;
      r : in GLfloat);

   procedure glTexCoord3i
     (s : in GLint;
      t : in GLint;
      r : in GLint);

   procedure glTexCoord3s
     (s : in GLshort;
      t : in GLshort;
      r : in GLshort);

   procedure glTexCoord4d
     (s : in GLdouble;
      t : in GLdouble;
      r : in GLdouble;
      q : in GLdouble);

   procedure glTexCoord4f
     (s : in GLfloat;
      t : in GLfloat;
      r : in GLfloat;
      q : in GLfloat);

   procedure glTexCoord4i
     (s : in GLint;
      t : in GLint;
      r : in GLint;
      q : in GLint);

   procedure glTexCoord4s
     (s : in GLshort;
      t : in GLshort;
      r : in GLshort;
      q : in GLshort);

   procedure glTexCoord1dv (v : in GLdouble_Pointer);

   procedure glTexCoord1fv (v : in GLfloat_Pointer);

   procedure glTexCoord1iv (v : in GLint_Pointer);

   procedure glTexCoord1sv (v : in GLshort_Pointer);

   procedure glTexCoord2dv (v : in GLdouble_Pointer);

   procedure glTexCoord2fv (v : in GLfloat_Pointer);

   procedure glTexCoord2iv (v : in GLint_Pointer);

   procedure glTexCoord2sv (v : in GLshort_Pointer);

   procedure glTexCoord3dv (v : in GLdouble_Pointer);

   procedure glTexCoord3fv (v : in GLfloat_Pointer);

   procedure glTexCoord3iv (v : in GLint_Pointer);

   procedure glTexCoord3sv (v : in GLshort_Pointer);

   procedure glTexCoord4dv (v : in GLdouble_Pointer);

   procedure glTexCoord4fv (v : in GLfloat_Pointer);

   procedure glTexCoord4iv (v : in GLint_Pointer);

   procedure glTexCoord4sv (v : in GLshort_Pointer);

   procedure glRasterPos2d (x : in GLdouble; y : in GLdouble);

   procedure glRasterPos2f (x : in GLfloat; y : in GLfloat);

   procedure glRasterPos2i (x : in GLint; y : in GLint);

   procedure glRasterPos2s (x : in GLshort; y : in GLshort);

   procedure glRasterPos3d
     (x : in GLdouble;
      y : in GLdouble;
      z : in GLdouble);

   procedure glRasterPos3f
     (x : in GLfloat;
      y : in GLfloat;
      z : in GLfloat);

   procedure glRasterPos3i
     (x : in GLint;
      y : in GLint;
      z : in GLint);

   procedure glRasterPos3s
     (x : in GLshort;
      y : in GLshort;
      z : in GLshort);

   procedure glRasterPos4d
     (x : in GLdouble;
      y : in GLdouble;
      z : in GLdouble;
      w : in GLdouble);

   procedure glRasterPos4f
     (x : in GLfloat;
      y : in GLfloat;
      z : in GLfloat;
      w : in GLfloat);

   procedure glRasterPos4i
     (x : in GLint;
      y : in GLint;
      z : in GLint;
      w : in GLint);

   procedure glRasterPos4s
     (x : in GLshort;
      y : in GLshort;
      z : in GLshort;
      w : in GLshort);

   procedure glRasterPos2dv (v : in GLdouble_Pointer);

   procedure glRasterPos2fv (v : in GLfloat_Pointer);

   procedure glRasterPos2iv (v : in GLint_Pointer);

   procedure glRasterPos2sv (v : in GLshort_Pointer);

   procedure glRasterPos3dv (v : in GLdouble_Pointer);

   procedure glRasterPos3fv (v : in GLfloat_Pointer);

   procedure glRasterPos3iv (v : in GLint_Pointer);

   procedure glRasterPos3sv (v : in GLshort_Pointer);

   procedure glRasterPos4dv (v : in GLdouble_Pointer);

   procedure glRasterPos4fv (v : in GLfloat_Pointer);

   procedure glRasterPos4iv (v : in GLint_Pointer);

   procedure glRasterPos4sv (v : in GLshort_Pointer);

   procedure glRectd
     (x1 : in GLdouble;
      y1 : in GLdouble;
      x2 : in GLdouble;
      y2 : in GLdouble);

   procedure glRectf
     (x1 : in GLfloat;
      y1 : in GLfloat;
      x2 : in GLfloat;
      y2 : in GLfloat);

   procedure glRecti
     (x1 : in GLint;
      y1 : in GLint;
      x2 : in GLint;
      y2 : in GLint);

   procedure glRects
     (x1 : in GLshort;
      y1 : in GLshort;
      x2 : in GLshort;
      y2 : in GLshort);

   procedure glRectdv
     (v1 : in GLdouble_Pointer;
      v2 : in GLdouble_Pointer);

   procedure glRectfv
     (v1 : in GLfloat_Pointer;
      v2 : in GLfloat_Pointer);

   procedure glRectiv
     (v1 : in GLint_Pointer;
      v2 : in GLint_Pointer);

   procedure glRectsv
     (v1 : in GLshort_Pointer;
      v2 : in GLshort_Pointer);

   procedure glVertexPointer
     (size     : in GLint;
      the_type : in GLenum;
      stride   : in GLsizei;
      ptr      : in GLvoid_Pointer);

   procedure glNormalPointer
     (the_type : in GLenum;
      stride   : in GLsizei;
      ptr      : in GLvoid_Pointer);

   procedure glColorPointer
     (size     : in GLint;
      the_type : in GLenum;
      stride   : in GLsizei;
      ptr      : in GLvoid_Pointer);

   procedure glIndexPointer
     (the_type : in GLenum;
      stride   : in GLsizei;
      ptr      : in GLvoid_Pointer);

   procedure glTexCoordPointer
     (size     : in GLint;
      the_type : in GLenum;
      stride   : in GLsizei;
      ptr      : in GLvoid_Pointer);

   procedure glEdgeFlagPointer
     (stride : in GLsizei;
      ptr    : in GLvoid_Pointer);

   procedure glGetPointerv
     (pname  : in GLenum;
      params : in GLvoid_Pointer_Pointer);

   procedure glArrayElement (i : in GLint);

   procedure glDrawArrays
     (mode  : in GLenum;
      first : in GLint;
      count : in GLsizei);

   procedure glDrawElements
     (mode     : in GLenum;
      count    : in GLsizei;
      the_type : in GLenum;
      indices  : in GLvoid_Pointer);

   procedure glInterleavedArrays
     (format  : in GLenum;
      stride  : in GLsizei;
      pointer : in GLvoid_Pointer);

   procedure glShadeModel (mode : in GLenum);

   procedure glLightf
     (light : in GLenum;
      pname : in GLenum;
      param : in GLfloat);

   procedure glLighti
     (light : in GLenum;
      pname : in GLenum;
      param : in GLint);

   procedure glLightfv
     (light  : in GLenum;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glLightiv
     (light  : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glGetLightfv
     (light  : in GLenum;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glGetLightiv
     (light  : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glLightModelf (pname : in GLenum; param : in GLfloat);

   procedure glLightModeli (pname : in GLenum; param : in GLint);

   procedure glLightModelfv
     (pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glLightModeliv
     (pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glMaterialf
     (face  : in GLenum;
      pname : in GLenum;
      param : in GLfloat);

   procedure glMateriali
     (face  : in GLenum;
      pname : in GLenum;
      param : in GLint);

   procedure glMaterialfv
     (face   : in GLenum;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glMaterialiv
     (face   : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glGetMaterialfv
     (face   : in GLenum;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glGetMaterialiv
     (face   : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glColorMaterial (face : in GLenum; mode : in GLenum);

   procedure glPixelZoom (xfactor : in GLfloat; yfactor : in GLfloat);

   procedure glPixelStoref (pname : in GLenum; param : in GLfloat);

   procedure glPixelStorei (pname : in GLenum; param : in GLint);

   procedure glPixelTransferf (pname : in GLenum; param : in GLfloat);

   procedure glPixelTransferi (pname : in GLenum; param : in GLint);

   procedure glPixelMapfv
     (map     : in GLenum;
      mapsize : in GLsizei;
      values  : in GLfloat_Pointer);

   procedure glPixelMapuiv
     (map     : in GLenum;
      mapsize : in GLsizei;
      values  : in GLuint_Pointer);

   procedure glPixelMapusv
     (map     : in GLenum;
      mapsize : in GLsizei;
      values  : in GLushort_Pointer);

   procedure glGetPixelMapfv
     (map    : in GLenum;
      values : in GLfloat_Pointer);

   procedure glGetPixelMapuiv
     (map    : in GLenum;
      values : in GLuint_Pointer);

   procedure glGetPixelMapusv
     (map    : in GLenum;
      values : in GLushort_Pointer);

   procedure glBitmap
     (width  : in GLsizei;
      height : in GLsizei;
      xorig  : in GLfloat;
      yorig  : in GLfloat;
      xmove  : in GLfloat;
      ymove  : in GLfloat;
      bitmap : in GLubyte_Pointer);

   procedure glReadPixels
     (x        : in GLint;
      y        : in GLint;
      width    : in GLsizei;
      height   : in GLsizei;
      format   : in GLenum;
      the_type : in GLenum;
      pixels   : in GLvoid_Pointer);

   procedure glDrawPixels
     (width    : in GLsizei;
      height   : in GLsizei;
      format   : in GLenum;
      the_type : in GLenum;
      pixels   : in GLvoid_Pointer);

   procedure glCopyPixels
     (x        : in GLint;
      y        : in GLint;
      width    : in GLsizei;
      height   : in GLsizei;
      the_type : in GLenum);

   procedure glStencilFunc
     (func : in GLenum;
      ref  : in GLint;
      mask : in GLuint);

   procedure glStencilMask (mask : in GLuint);

   procedure glStencilOp
     (fail  : in GLenum;
      zfail : in GLenum;
      zpass : in GLenum);

   procedure glClearStencil (s : in GLint);

   procedure glTexGend
     (coord : in GLenum;
      pname : in GLenum;
      param : in GLdouble);

   procedure glTexGenf
     (coord : in GLenum;
      pname : in GLenum;
      param : in GLfloat);

   procedure glTexGeni
     (coord : in GLenum;
      pname : in GLenum;
      param : in GLint);

   procedure glTexGendv
     (coord  : in GLenum;
      pname  : in GLenum;
      params : in GLdouble_Pointer);

   procedure glTexGenfv
     (coord  : in GLenum;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glTexGeniv
     (coord  : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glGetTexGendv
     (coord  : in GLenum;
      pname  : in GLenum;
      params : in GLdouble_Pointer);

   procedure glGetTexGenfv
     (coord  : in GLenum;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glGetTexGeniv
     (coord  : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glTexEnvf
     (target : in GLenum;
      pname  : in GLenum;
      param  : in GLfloat);

   procedure glTexEnvi
     (target : in GLenum;
      pname  : in GLenum;
      param  : in GLint);

   procedure glTexEnvfv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glTexEnviv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glGetTexEnvfv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glGetTexEnviv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glTexParameterf
     (target : in GLenum;
      pname  : in GLenum;
      param  : in GLfloat);

   procedure glTexParameteri
     (target : in GLenum;
      pname  : in GLenum;
      param  : in GLint);

   procedure glTexParameterfv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glTexParameteriv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glGetTexParameterfv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glGetTexParameteriv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glGetTexLevelParameterfv
     (target : in GLenum;
      level  : in GLint;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glGetTexLevelParameteriv
     (target : in GLenum;
      level  : in GLint;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glTexImage1D
     (target         : in GLenum;
      level          : in GLint;
      internalFormat : in GLint;
      width          : in GLsizei;
      border         : in GLint;
      format         : in GLenum;
      the_type       : in GLenum;
      pixels         : in GLvoid_Pointer);

   procedure glTexImage2D
     (target         : in GLenum;
      level          : in GLint;
      internalFormat : in GLint;
      width          : in GLsizei;
      height         : in GLsizei;
      border         : in GLint;
      format         : in GLenum;
      the_type       : in GLenum;
      pixels         : in GLvoid_Pointer);

   procedure glGetTexImage
     (target   : in GLenum;
      level    : in GLint;
      format   : in GLenum;
      the_type : in GLenum;
      pixels   : in GLvoid_Pointer);

   procedure glGenTextures
     (n        : in GLsizei;
      textures : in GLuint_Pointer);

   procedure glDeleteTextures
     (n        : in GLsizei;
      textures : in GLuint_Pointer);

   procedure glBindTexture (target : in GLenum; texture : in GLuint);

   procedure glPrioritizeTextures
     (n          : in GLsizei;
      textures   : in GLuint_Pointer;
      priorities : in GLclampf_Pointer);

   function glAreTexturesResident
     (n          : in GLsizei;
      textures   : in GLuint_Pointer;
      residences : in GLboolean_Pointer)
      return       GLboolean;

   function glIsTexture (texture : in GLuint) return GLboolean;

   procedure glTexSubImage1D
     (target   : in GLenum;
      level    : in GLint;
      xoffset  : in GLint;
      width    : in GLsizei;
      format   : in GLenum;
      the_type : in GLenum;
      pixels   : in GLvoid_Pointer);

   procedure glTexSubImage2D
     (target   : in GLenum;
      level    : in GLint;
      xoffset  : in GLint;
      yoffset  : in GLint;
      width    : in GLsizei;
      height   : in GLsizei;
      format   : in GLenum;
      the_type : in GLenum;
      pixels   : in GLvoid_Pointer);

   procedure glCopyTexImage1D
     (target         : in GLenum;
      level          : in GLint;
      internalformat : in GLenum;
      x              : in GLint;
      y              : in GLint;
      width          : in GLsizei;
      border         : in GLint);

   procedure glCopyTexImage2D
     (target         : in GLenum;
      level          : in GLint;
      internalformat : in GLenum;
      x              : in GLint;
      y              : in GLint;
      width          : in GLsizei;
      height         : in GLsizei;
      border         : in GLint);

   procedure glCopyTexSubImage1D
     (target  : in GLenum;
      level   : in GLint;
      xoffset : in GLint;
      x       : in GLint;
      y       : in GLint;
      width   : in GLsizei);

   procedure glCopyTexSubImage2D
     (target  : in GLenum;
      level   : in GLint;
      xoffset : in GLint;
      yoffset : in GLint;
      x       : in GLint;
      y       : in GLint;
      width   : in GLsizei;
      height  : in GLsizei);

   procedure glMap1d
     (target : in GLenum;
      u1     : in GLdouble;
      u2     : in GLdouble;
      stride : in GLint;
      order  : in GLint;
      points : in GLdouble_Pointer);

   procedure glMap1f
     (target : in GLenum;
      u1     : in GLfloat;
      u2     : in GLfloat;
      stride : in GLint;
      order  : in GLint;
      points : in GLfloat_Pointer);

   procedure glMap2d
     (target  : in GLenum;
      u1      : in GLdouble;
      u2      : in GLdouble;
      ustride : in GLint;
      uorder  : in GLint;
      v1      : in GLdouble;
      v2      : in GLdouble;
      vstride : in GLint;
      vorder  : in GLint;
      points  : in GLdouble_Pointer);

   procedure glMap2f
     (target  : in GLenum;
      u1      : in GLfloat;
      u2      : in GLfloat;
      ustride : in GLint;
      uorder  : in GLint;
      v1      : in GLfloat;
      v2      : in GLfloat;
      vstride : in GLint;
      vorder  : in GLint;
      points  : in GLfloat_Pointer);

   procedure glGetMapdv
     (target : in GLenum;
      query  : in GLenum;
      v      : in GLdouble_Pointer);

   procedure glGetMapfv
     (target : in GLenum;
      query  : in GLenum;
      v      : in GLfloat_Pointer);

   procedure glGetMapiv
     (target : in GLenum;
      query  : in GLenum;
      v      : in GLint_Pointer);

   procedure glEvalCoord1d (u : in GLdouble);

   procedure glEvalCoord1f (u : in GLfloat);

   procedure glEvalCoord1dv (u : in GLdouble_Pointer);

   procedure glEvalCoord1fv (u : in GLfloat_Pointer);

   procedure glEvalCoord2d (u : in GLdouble; v : in GLdouble);

   procedure glEvalCoord2f (u : in GLfloat; v : in GLfloat);

   procedure glEvalCoord2dv (u : in GLdouble_Pointer);

   procedure glEvalCoord2fv (u : in GLfloat_Pointer);

   procedure glMapGrid1d
     (un : in GLint;
      u1 : in GLdouble;
      u2 : in GLdouble);

   procedure glMapGrid1f
     (un : in GLint;
      u1 : in GLfloat;
      u2 : in GLfloat);

   procedure glMapGrid2d
     (un : in GLint;
      u1 : in GLdouble;
      u2 : in GLdouble;
      vn : in GLint;
      v1 : in GLdouble;
      v2 : in GLdouble);

   procedure glMapGrid2f
     (un : in GLint;
      u1 : in GLfloat;
      u2 : in GLfloat;
      vn : in GLint;
      v1 : in GLfloat;
      v2 : in GLfloat);

   procedure glEvalPoint1 (i : in GLint);

   procedure glEvalPoint2 (i : in GLint; j : in GLint);

   procedure glEvalMesh1
     (mode : in GLenum;
      i1   : in GLint;
      i2   : in GLint);

   procedure glEvalMesh2
     (mode : in GLenum;
      i1   : in GLint;
      i2   : in GLint;
      j1   : in GLint;
      j2   : in GLint);

   procedure glFogf (pname : in GLenum; param : in GLfloat);

   procedure glFogi (pname : in GLenum; param : in GLint);

   procedure glFogfv
     (pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glFogiv
     (pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glFeedbackBuffer
     (size     : in GLsizei;
      the_type : in GLenum;
      buffer   : in GLfloat_Pointer);

   procedure glPassThrough (token : in GLfloat);

   procedure glSelectBuffer
     (size   : in GLsizei;
      buffer : in GLuint_Pointer);

   procedure glInitNames;

   procedure glLoadName (name : in GLuint);

   procedure glPushName (name : in GLuint);

   procedure glPopName;

   procedure glDrawRangeElements
     (mode     : in GLenum;
      start    : in GLuint;
      the_end  : in GLuint;
      count    : in GLsizei;
      the_type : in GLenum;
      indices  : in GLvoid_Pointer);

   procedure glTexImage3D
     (target         : in GLenum;
      level          : in GLint;
      internalFormat : in GLint;
      width          : in GLsizei;
      height         : in GLsizei;
      depth          : in GLsizei;
      border         : in GLint;
      format         : in GLenum;
      the_type       : in GLenum;
      pixels         : in GLvoid_Pointer);

   procedure glTexSubImage3D
     (target   : in GLenum;
      level    : in GLint;
      xoffset  : in GLint;
      yoffset  : in GLint;
      zoffset  : in GLint;
      width    : in GLsizei;
      height   : in GLsizei;
      depth    : in GLsizei;
      format   : in GLenum;
      the_type : in GLenum;
      pixels   : in GLvoid_Pointer);

   procedure glCopyTexSubImage3D
     (target  : in GLenum;
      level   : in GLint;
      xoffset : in GLint;
      yoffset : in GLint;
      zoffset : in GLint;
      x       : in GLint;
      y       : in GLint;
      width   : in GLsizei;
      height  : in GLsizei);

   procedure glColorTable
     (target         : in GLenum;
      internalformat : in GLenum;
      width          : in GLsizei;
      format         : in GLenum;
      the_type       : in GLenum;
      table          : in GLvoid_Pointer);

   procedure glColorSubTable
     (target   : in GLenum;
      start    : in GLsizei;
      count    : in GLsizei;
      format   : in GLenum;
      the_type : in GLenum;
      data     : in GLvoid_Pointer);

   procedure glColorTableParameteriv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glColorTableParameterfv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glCopyColorSubTable
     (target : in GLenum;
      start  : in GLsizei;
      x      : in GLint;
      y      : in GLint;
      width  : in GLsizei);

   procedure glCopyColorTable
     (target         : in GLenum;
      internalformat : in GLenum;
      x              : in GLint;
      y              : in GLint;
      width          : in GLsizei);

   procedure glGetColorTable
     (target   : in GLenum;
      format   : in GLenum;
      the_type : in GLenum;
      table    : in GLvoid_Pointer);

   procedure glGetColorTableParameterfv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glGetColorTableParameteriv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glBlendEquation (mode : in GLenum);

   procedure glBlendColor
     (red   : in GLclampf;
      green : in GLclampf;
      blue  : in GLclampf;
      alpha : in GLclampf);

   procedure glHistogram
     (target         : in GLenum;
      width          : in GLsizei;
      internalformat : in GLenum;
      sink           : in GLboolean);

   procedure glResetHistogram (target : in GLenum);

   procedure glGetHistogram
     (target   : in GLenum;
      reset    : in GLboolean;
      format   : in GLenum;
      the_type : in GLenum;
      values   : in GLvoid_Pointer);

   procedure glGetHistogramParameterfv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glGetHistogramParameteriv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glMinmax
     (target         : in GLenum;
      internalformat : in GLenum;
      sink           : in GLboolean);

   procedure glResetMinmax (target : in GLenum);

   procedure glGetMinmax
     (target : in GLenum;
      reset  : in GLboolean;
      format : in GLenum;
      types  : in GLenum;
      values : in GLvoid_Pointer);

   procedure glGetMinmaxParameterfv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glGetMinmaxParameteriv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glConvolutionFilter1D
     (target         : in GLenum;
      internalformat : in GLenum;
      width          : in GLsizei;
      format         : in GLenum;
      the_type       : in GLenum;
      image          : in GLvoid_Pointer);

   procedure glConvolutionFilter2D
     (target         : in GLenum;
      internalformat : in GLenum;
      width          : in GLsizei;
      height         : in GLsizei;
      format         : in GLenum;
      the_type       : in GLenum;
      image          : in GLvoid_Pointer);

   procedure glConvolutionParameterf
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLfloat);

   procedure glConvolutionParameterfv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glConvolutionParameteri
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLint);

   procedure glConvolutionParameteriv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glCopyConvolutionFilter1D
     (target         : in GLenum;
      internalformat : in GLenum;
      x              : in GLint;
      y              : in GLint;
      width          : in GLsizei);

   procedure glCopyConvolutionFilter2D
     (target         : in GLenum;
      internalformat : in GLenum;
      x              : in GLint;
      y              : in GLint;
      width          : in GLsizei;
      height         : in GLsizei);

   procedure glGetConvolutionFilter
     (target   : in GLenum;
      format   : in GLenum;
      the_type : in GLenum;
      image    : in GLvoid_Pointer);

   procedure glGetConvolutionParameterfv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glGetConvolutionParameteriv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glSeparableFilter2D
     (target         : in GLenum;
      internalformat : in GLenum;
      width          : in GLsizei;
      height         : in GLsizei;
      format         : in GLenum;
      the_type       : in GLenum;
      row            : in GLvoid_Pointer;
      column         : in GLvoid_Pointer);

   procedure glGetSeparableFilter
     (target   : in GLenum;
      format   : in GLenum;
      the_type : in GLenum;
      row      : in GLvoid_Pointer;
      column   : in GLvoid_Pointer;
      span     : in GLvoid_Pointer);

   procedure glActiveTexture (texture : in GLenum);

   procedure glClientActiveTexture (texture : in GLenum);

   procedure glCompressedTexImage1D
     (target         : in GLenum;
      level          : in GLint;
      internalformat : in GLenum;
      width          : in GLsizei;
      border         : in GLint;
      imageSize      : in GLsizei;
      data           : in GLvoid_Pointer);

   procedure glCompressedTexImage2D
     (target         : in GLenum;
      level          : in GLint;
      internalformat : in GLenum;
      width          : in GLsizei;
      height         : in GLsizei;
      border         : in GLint;
      imageSize      : in GLsizei;
      data           : in GLvoid_Pointer);

   procedure glCompressedTexImage3D
     (target         : in GLenum;
      level          : in GLint;
      internalformat : in GLenum;
      width          : in GLsizei;
      height         : in GLsizei;
      depth          : in GLsizei;
      border         : in GLint;
      imageSize      : in GLsizei;
      data           : in GLvoid_Pointer);

   procedure glCompressedTexSubImage1D
     (target    : in GLenum;
      level     : in GLint;
      xoffset   : in GLint;
      width     : in GLsizei;
      format    : in GLenum;
      imageSize : in GLsizei;
      data      : in GLvoid_Pointer);

   procedure glCompressedTexSubImage2D
     (target    : in GLenum;
      level     : in GLint;
      xoffset   : in GLint;
      yoffset   : in GLint;
      width     : in GLsizei;
      height    : in GLsizei;
      format    : in GLenum;
      imageSize : in GLsizei;
      data      : in GLvoid_Pointer);

   procedure glCompressedTexSubImage3D
     (target    : in GLenum;
      level     : in GLint;
      xoffset   : in GLint;
      yoffset   : in GLint;
      zoffset   : in GLint;
      width     : in GLsizei;
      height    : in GLsizei;
      depth     : in GLsizei;
      format    : in GLenum;
      imageSize : in GLsizei;
      data      : in GLvoid_Pointer);

   procedure glGetCompressedTexImage
     (target : in GLenum;
      lod    : in GLint;
      img    : in GLvoid_Pointer);

   procedure glMultiTexCoord1d (target : in GLenum; s : in GLdouble);

   procedure glMultiTexCoord1dv
     (target : in GLenum;
      v      : in GLdouble_Pointer);

   procedure glMultiTexCoord1f (target : in GLenum; s : in GLfloat);

   procedure glMultiTexCoord1fv
     (target : in GLenum;
      v      : in GLfloat_Pointer);

   procedure glMultiTexCoord1i (target : in GLenum; s : in GLint);

   procedure glMultiTexCoord1iv
     (target : in GLenum;
      v      : in GLint_Pointer);

   procedure glMultiTexCoord1s (target : in GLenum; s : in GLshort);

   procedure glMultiTexCoord1sv
     (target : in GLenum;
      v      : in GLshort_Pointer);

   procedure glMultiTexCoord2d
     (target : in GLenum;
      s      : in GLdouble;
      t      : in GLdouble);

   procedure glMultiTexCoord2dv
     (target : in GLenum;
      v      : in GLdouble_Pointer);

   procedure glMultiTexCoord2f
     (target : in GLenum;
      s      : in GLfloat;
      t      : in GLfloat);

   procedure glMultiTexCoord2fv
     (target : in GLenum;
      v      : in GLfloat_Pointer);

   procedure glMultiTexCoord2i
     (target : in GLenum;
      s      : in GLint;
      t      : in GLint);

   procedure glMultiTexCoord2iv
     (target : in GLenum;
      v      : in GLint_Pointer);

   procedure glMultiTexCoord2s
     (target : in GLenum;
      s      : in GLshort;
      t      : in GLshort);

   procedure glMultiTexCoord2sv
     (target : in GLenum;
      v      : in GLshort_Pointer);

   procedure glMultiTexCoord3d
     (target : in GLenum;
      s      : in GLdouble;
      t      : in GLdouble;
      r      : in GLdouble);

   procedure glMultiTexCoord3dv
     (target : in GLenum;
      v      : in GLdouble_Pointer);

   procedure glMultiTexCoord3f
     (target : in GLenum;
      s      : in GLfloat;
      t      : in GLfloat;
      r      : in GLfloat);

   procedure glMultiTexCoord3fv
     (target : in GLenum;
      v      : in GLfloat_Pointer);

   procedure glMultiTexCoord3i
     (target : in GLenum;
      s      : in GLint;
      t      : in GLint;
      r      : in GLint);

   procedure glMultiTexCoord3iv
     (target : in GLenum;
      v      : in GLint_Pointer);

   procedure glMultiTexCoord3s
     (target : in GLenum;
      s      : in GLshort;
      t      : in GLshort;
      r      : in GLshort);

   procedure glMultiTexCoord3sv
     (target : in GLenum;
      v      : in GLshort_Pointer);

   procedure glMultiTexCoord4d
     (target : in GLenum;
      s      : in GLdouble;
      t      : in GLdouble;
      r      : in GLdouble;
      q      : in GLdouble);

   procedure glMultiTexCoord4dv
     (target : in GLenum;
      v      : in GLdouble_Pointer);

   procedure glMultiTexCoord4f
     (target : in GLenum;
      s      : in GLfloat;
      t      : in GLfloat;
      r      : in GLfloat;
      q      : in GLfloat);

   procedure glMultiTexCoord4fv
     (target : in GLenum;
      v      : in GLfloat_Pointer);

   procedure glMultiTexCoord4i
     (target : in GLenum;
      s      : in GLint;
      t      : in GLint;
      r      : in GLint;
      q      : in GLint);

   procedure glMultiTexCoord4iv
     (target : in GLenum;
      v      : in GLint_Pointer);

   procedure glMultiTexCoord4s
     (target : in GLenum;
      s      : in GLshort;
      t      : in GLshort;
      r      : in GLshort;
      q      : in GLshort);

   procedure glMultiTexCoord4sv
     (target : in GLenum;
      v      : in GLshort_Pointer);

   procedure glLoadTransposeMatrixd (m : in GLdouble_Pointer);

   procedure glLoadTransposeMatrixf (m : in GLfloat_Pointer);

   procedure glMultTransposeMatrixd (m : in GLdouble_Pointer);

   procedure glMultTransposeMatrixf (m : in GLfloat_Pointer);

   procedure glSampleCoverage
     (value  : in GLclampf;
      invert : in GLboolean);

   procedure glActiveTextureARB (texture : in GLenum);

   procedure glClientActiveTextureARB (texture : in GLenum);

   procedure glMultiTexCoord1dARB
     (target : in GLenum;
      s      : in GLdouble);

   procedure glMultiTexCoord1dvARB
     (target : in GLenum;
      v      : in GLdouble_Pointer);

   procedure glMultiTexCoord1fARB
     (target : in GLenum;
      s      : in GLfloat);

   procedure glMultiTexCoord1fvARB
     (target : in GLenum;
      v      : in GLfloat_Pointer);

   procedure glMultiTexCoord1iARB (target : in GLenum; s : in GLint);

   procedure glMultiTexCoord1ivARB
     (target : in GLenum;
      v      : in GLint_Pointer);

   procedure glMultiTexCoord1sARB
     (target : in GLenum;
      s      : in GLshort);

   procedure glMultiTexCoord1svARB
     (target : in GLenum;
      v      : in GLshort_Pointer);

   procedure glMultiTexCoord2dARB
     (target : in GLenum;
      s      : in GLdouble;
      t      : in GLdouble);

   procedure glMultiTexCoord2dvARB
     (target : in GLenum;
      v      : in GLdouble_Pointer);

   procedure glMultiTexCoord2fARB
     (target : in GLenum;
      s      : in GLfloat;
      t      : in GLfloat);

   procedure glMultiTexCoord2fvARB
     (target : in GLenum;
      v      : in GLfloat_Pointer);

   procedure glMultiTexCoord2iARB
     (target : in GLenum;
      s      : in GLint;
      t      : in GLint);

   procedure glMultiTexCoord2ivARB
     (target : in GLenum;
      v      : in GLint_Pointer);

   procedure glMultiTexCoord2sARB
     (target : in GLenum;
      s      : in GLshort;
      t      : in GLshort);

   procedure glMultiTexCoord2svARB
     (target : in GLenum;
      v      : in GLshort_Pointer);

   procedure glMultiTexCoord3dARB
     (target : in GLenum;
      s      : in GLdouble;
      t      : in GLdouble;
      r      : in GLdouble);

   procedure glMultiTexCoord3dvARB
     (target : in GLenum;
      v      : in GLdouble_Pointer);

   procedure glMultiTexCoord3fARB
     (target : in GLenum;
      s      : in GLfloat;
      t      : in GLfloat;
      r      : in GLfloat);

   procedure glMultiTexCoord3fvARB
     (target : in GLenum;
      v      : in GLfloat_Pointer);

   procedure glMultiTexCoord3iARB
     (target : in GLenum;
      s      : in GLint;
      t      : in GLint;
      r      : in GLint);

   procedure glMultiTexCoord3ivARB
     (target : in GLenum;
      v      : in GLint_Pointer);

   procedure glMultiTexCoord3sARB
     (target : in GLenum;
      s      : in GLshort;
      t      : in GLshort;
      r      : in GLshort);

   procedure glMultiTexCoord3svARB
     (target : in GLenum;
      v      : in GLshort_Pointer);

   procedure glMultiTexCoord4dARB
     (target : in GLenum;
      s      : in GLdouble;
      t      : in GLdouble;
      r      : in GLdouble;
      q      : in GLdouble);

   procedure glMultiTexCoord4dvARB
     (target : in GLenum;
      v      : in GLdouble_Pointer);

   procedure glMultiTexCoord4fARB
     (target : in GLenum;
      s      : in GLfloat;
      t      : in GLfloat;
      r      : in GLfloat;
      q      : in GLfloat);

   procedure glMultiTexCoord4fvARB
     (target : in GLenum;
      v      : in GLfloat_Pointer);

   procedure glMultiTexCoord4iARB
     (target : in GLenum;
      s      : in GLint;
      t      : in GLint;
      r      : in GLint;
      q      : in GLint);

   procedure glMultiTexCoord4ivARB
     (target : in GLenum;
      v      : in GLint_Pointer);

   procedure glMultiTexCoord4sARB
     (target : in GLenum;
      s      : in GLshort;
      t      : in GLshort;
      r      : in GLshort;
      q      : in GLshort);

   procedure glMultiTexCoord4svARB
     (target : in GLenum;
      v      : in GLshort_Pointer);


   procedure glGetProgramRegisterfvMESA
     (target : in GLenum;
      len    : in GLsizei;
      name   : in GLubyte_Pointer;
      v      : in GLfloat_Pointer);

   procedure glBlendEquationSeparateATI
     (modeRGB : in GLenum;
      modeA   : in GLenum);

private

   pragma Import (StdCall, glClearIndex, "glClearIndex");
   pragma Import (StdCall, glClearColor, "glClearColor");
   pragma Import (StdCall, glClear, "glClear");
   pragma Import (StdCall, glIndexMask, "glIndexMask");
   pragma Import (StdCall, glColorMask, "glColorMask");
   pragma Import (StdCall, glAlphaFunc, "glAlphaFunc");
   pragma Import (StdCall, glBlendFunc, "glBlendFunc");
   pragma Import (StdCall, glLogicOp, "glLogicOp");
   pragma Import (StdCall, glCullFace, "glCullFace");
   pragma Import (StdCall, glFrontFace, "glFrontFace");
   pragma Import (StdCall, glPointSize, "glPointSize");
   pragma Import (StdCall, glLineWidth, "glLineWidth");
   pragma Import (StdCall, glLineStipple, "glLineStipple");
   pragma Import (StdCall, glPolygonMode, "glPolygonMode");
   pragma Import (StdCall, glPolygonOffset, "glPolygonOffset");
   pragma Import (StdCall, glPolygonStipple, "glPolygonStipple");
   pragma Import (StdCall, glGetPolygonStipple, "glGetPolygonStipple");
   pragma Import (StdCall, glEdgeFlag, "glEdgeFlag");
   pragma Import (StdCall, glEdgeFlagv, "glEdgeFlagv");
   pragma Import (StdCall, glScissor, "glScissor");
   pragma Import (StdCall, glClipPlane, "glClipPlane");
   pragma Import (StdCall, glGetClipPlane, "glGetClipPlane");
   pragma Import (StdCall, glDrawBuffer, "glDrawBuffer");
   pragma Import (StdCall, glReadBuffer, "glReadBuffer");
   pragma Import (StdCall, glEnable, "glEnable");
   pragma Import (StdCall, glDisable, "glDisable");
   pragma Import (StdCall, glIsEnabled, "glIsEnabled");
   pragma Import (StdCall, glEnableClientState, "glEnableClientState");
   pragma Import (StdCall, glDisableClientState, "glDisableClientState");
   pragma Import (StdCall, glGetBooleanv, "glGetBooleanv");
   pragma Import (StdCall, glGetDoublev, "glGetDoublev");
   pragma Import (StdCall, glGetFloatv, "glGetFloatv");
   pragma Import (StdCall, glGetIntegerv, "glGetIntegerv");
   pragma Import (StdCall, glPushAttrib, "glPushAttrib");
   pragma Import (StdCall, glPopAttrib, "glPopAttrib");
   pragma Import (StdCall, glPushClientAttrib, "glPushClientAttrib");
   pragma Import (StdCall, glPopClientAttrib, "glPopClientAttrib");
   pragma Import (StdCall, glRenderMode, "glRenderMode");
   pragma Import (StdCall, glGetError, "glGetError");
   pragma Import (StdCall, glGetString, "glGetString");
   pragma Import (StdCall, glFinish, "glFinish");
   pragma Import (StdCall, glFlush, "glFlush");
   pragma Import (StdCall, glHint, "glHint");
   pragma Import (StdCall, glClearDepth, "glClearDepth");
   pragma Import (StdCall, glDepthFunc, "glDepthFunc");
   pragma Import (StdCall, glDepthMask, "glDepthMask");
   pragma Import (StdCall, glDepthRange, "glDepthRange");
   pragma Import (StdCall, glClearAccum, "glClearAccum");
   pragma Import (StdCall, glAccum, "glAccum");
   pragma Import (StdCall, glMatrixMode, "glMatrixMode");
   pragma Import (StdCall, glOrtho, "glOrtho");
   pragma Import (StdCall, glFrustum, "glFrustum");
   pragma Import (StdCall, glViewport, "glViewport");
   pragma Import (StdCall, glPushMatrix, "glPushMatrix");
   pragma Import (StdCall, glPopMatrix, "glPopMatrix");
   pragma Import (StdCall, glLoadIdentity, "glLoadIdentity");
   pragma Import (StdCall, glLoadMatrixd, "glLoadMatrixd");
   pragma Import (StdCall, glLoadMatrixf, "glLoadMatrixf");
   pragma Import (StdCall, glMultMatrixd, "glMultMatrixd");
   pragma Import (StdCall, glMultMatrixf, "glMultMatrixf");
   pragma Import (StdCall, glRotated, "glRotated");
   pragma Import (StdCall, glRotatef, "glRotatef");
   pragma Import (StdCall, glScaled, "glScaled");
   pragma Import (StdCall, glScalef, "glScalef");
   pragma Import (StdCall, glTranslated, "glTranslated");
   pragma Import (StdCall, glTranslatef, "glTranslatef");
   pragma Import (StdCall, glIsList, "glIsList");
   pragma Import (StdCall, glDeleteLists, "glDeleteLists");
   pragma Import (StdCall, glGenLists, "glGenLists");
   pragma Import (StdCall, glNewList, "glNewList");
   pragma Import (StdCall, glEndList, "glEndList");
   pragma Import (StdCall, glCallList, "glCallList");
   pragma Import (StdCall, glCallLists, "glCallLists");
   pragma Import (StdCall, glListBase, "glListBase");
   pragma Import (StdCall, glBegin, "glBegin");
   pragma Import (StdCall, glEnd, "glEnd");
   pragma Import (StdCall, glVertex2d, "glVertex2d");
   pragma Import (StdCall, glVertex2f, "glVertex2f");
   pragma Import (StdCall, glVertex2i, "glVertex2i");
   pragma Import (StdCall, glVertex2s, "glVertex2s");
   pragma Import (StdCall, glVertex3d, "glVertex3d");
   pragma Import (StdCall, glVertex3f, "glVertex3f");
   pragma Import (StdCall, glVertex3i, "glVertex3i");
   pragma Import (StdCall, glVertex3s, "glVertex3s");
   pragma Import (StdCall, glVertex4d, "glVertex4d");
   pragma Import (StdCall, glVertex4f, "glVertex4f");
   pragma Import (StdCall, glVertex4i, "glVertex4i");
   pragma Import (StdCall, glVertex4s, "glVertex4s");
   pragma Import (StdCall, glVertex2dv, "glVertex2dv");
   pragma Import (StdCall, glVertex2fv, "glVertex2fv");
   pragma Import (StdCall, glVertex2iv, "glVertex2iv");
   pragma Import (StdCall, glVertex2sv, "glVertex2sv");
   pragma Import (StdCall, glVertex3dv, "glVertex3dv");
   pragma Import (StdCall, glVertex3fv, "glVertex3fv");
   pragma Import (StdCall, glVertex3iv, "glVertex3iv");
   pragma Import (StdCall, glVertex3sv, "glVertex3sv");
   pragma Import (StdCall, glVertex4dv, "glVertex4dv");
   pragma Import (StdCall, glVertex4fv, "glVertex4fv");
   pragma Import (StdCall, glVertex4iv, "glVertex4iv");
   pragma Import (StdCall, glVertex4sv, "glVertex4sv");
   pragma Import (StdCall, glNormal3b, "glNormal3b");
   pragma Import (StdCall, glNormal3d, "glNormal3d");
   pragma Import (StdCall, glNormal3f, "glNormal3f");
   pragma Import (StdCall, glNormal3i, "glNormal3i");
   pragma Import (StdCall, glNormal3s, "glNormal3s");
   pragma Import (StdCall, glNormal3bv, "glNormal3bv");
   pragma Import (StdCall, glNormal3dv, "glNormal3dv");
   pragma Import (StdCall, glNormal3fv, "glNormal3fv");
   pragma Import (StdCall, glNormal3iv, "glNormal3iv");
   pragma Import (StdCall, glNormal3sv, "glNormal3sv");
   pragma Import (StdCall, glIndexd, "glIndexd");
   pragma Import (StdCall, glIndexf, "glIndexf");
   pragma Import (StdCall, glIndexi, "glIndexi");
   pragma Import (StdCall, glIndexs, "glIndexs");
   pragma Import (StdCall, glIndexub, "glIndexub");
   pragma Import (StdCall, glIndexdv, "glIndexdv");
   pragma Import (StdCall, glIndexfv, "glIndexfv");
   pragma Import (StdCall, glIndexiv, "glIndexiv");
   pragma Import (StdCall, glIndexsv, "glIndexsv");
   pragma Import (StdCall, glIndexubv, "glIndexubv");
   pragma Import (StdCall, glColor3b, "glColor3b");
   pragma Import (StdCall, glColor3d, "glColor3d");
   pragma Import (StdCall, glColor3f, "glColor3f");
   pragma Import (StdCall, glColor3i, "glColor3i");
   pragma Import (StdCall, glColor3s, "glColor3s");
   pragma Import (StdCall, glColor3ub, "glColor3ub");
   pragma Import (StdCall, glColor3ui, "glColor3ui");
   pragma Import (StdCall, glColor3us, "glColor3us");
   pragma Import (StdCall, glColor4b, "glColor4b");
   pragma Import (StdCall, glColor4d, "glColor4d");
   pragma Import (StdCall, glColor4f, "glColor4f");
   pragma Import (StdCall, glColor4i, "glColor4i");
   pragma Import (StdCall, glColor4s, "glColor4s");
   pragma Import (StdCall, glColor4ub, "glColor4ub");
   pragma Import (StdCall, glColor4ui, "glColor4ui");
   pragma Import (StdCall, glColor4us, "glColor4us");
   pragma Import (StdCall, glColor3bv, "glColor3bv");
   pragma Import (StdCall, glColor3dv, "glColor3dv");
   pragma Import (StdCall, glColor3fv, "glColor3fv");
   pragma Import (StdCall, glColor3iv, "glColor3iv");
   pragma Import (StdCall, glColor3sv, "glColor3sv");
   pragma Import (StdCall, glColor3ubv, "glColor3ubv");
   pragma Import (StdCall, glColor3uiv, "glColor3uiv");
   pragma Import (StdCall, glColor3usv, "glColor3usv");
   pragma Import (StdCall, glColor4bv, "glColor4bv");
   pragma Import (StdCall, glColor4dv, "glColor4dv");
   pragma Import (StdCall, glColor4fv, "glColor4fv");
   pragma Import (StdCall, glColor4iv, "glColor4iv");
   pragma Import (StdCall, glColor4sv, "glColor4sv");
   pragma Import (StdCall, glColor4ubv, "glColor4ubv");
   pragma Import (StdCall, glColor4uiv, "glColor4uiv");
   pragma Import (StdCall, glColor4usv, "glColor4usv");
   pragma Import (StdCall, glTexCoord1d, "glTexCoord1d");
   pragma Import (StdCall, glTexCoord1f, "glTexCoord1f");
   pragma Import (StdCall, glTexCoord1i, "glTexCoord1i");
   pragma Import (StdCall, glTexCoord1s, "glTexCoord1s");
   pragma Import (StdCall, glTexCoord2d, "glTexCoord2d");
   pragma Import (StdCall, glTexCoord2f, "glTexCoord2f");
   pragma Import (StdCall, glTexCoord2i, "glTexCoord2i");
   pragma Import (StdCall, glTexCoord2s, "glTexCoord2s");
   pragma Import (StdCall, glTexCoord3d, "glTexCoord3d");
   pragma Import (StdCall, glTexCoord3f, "glTexCoord3f");
   pragma Import (StdCall, glTexCoord3i, "glTexCoord3i");
   pragma Import (StdCall, glTexCoord3s, "glTexCoord3s");
   pragma Import (StdCall, glTexCoord4d, "glTexCoord4d");
   pragma Import (StdCall, glTexCoord4f, "glTexCoord4f");
   pragma Import (StdCall, glTexCoord4i, "glTexCoord4i");
   pragma Import (StdCall, glTexCoord4s, "glTexCoord4s");
   pragma Import (StdCall, glTexCoord1dv, "glTexCoord1dv");
   pragma Import (StdCall, glTexCoord1fv, "glTexCoord1fv");
   pragma Import (StdCall, glTexCoord1iv, "glTexCoord1iv");
   pragma Import (StdCall, glTexCoord1sv, "glTexCoord1sv");
   pragma Import (StdCall, glTexCoord2dv, "glTexCoord2dv");
   pragma Import (StdCall, glTexCoord2fv, "glTexCoord2fv");
   pragma Import (StdCall, glTexCoord2iv, "glTexCoord2iv");
   pragma Import (StdCall, glTexCoord2sv, "glTexCoord2sv");
   pragma Import (StdCall, glTexCoord3dv, "glTexCoord3dv");
   pragma Import (StdCall, glTexCoord3fv, "glTexCoord3fv");
   pragma Import (StdCall, glTexCoord3iv, "glTexCoord3iv");
   pragma Import (StdCall, glTexCoord3sv, "glTexCoord3sv");
   pragma Import (StdCall, glTexCoord4dv, "glTexCoord4dv");
   pragma Import (StdCall, glTexCoord4fv, "glTexCoord4fv");
   pragma Import (StdCall, glTexCoord4iv, "glTexCoord4iv");
   pragma Import (StdCall, glTexCoord4sv, "glTexCoord4sv");
   pragma Import (StdCall, glRasterPos2d, "glRasterPos2d");
   pragma Import (StdCall, glRasterPos2f, "glRasterPos2f");
   pragma Import (StdCall, glRasterPos2i, "glRasterPos2i");
   pragma Import (StdCall, glRasterPos2s, "glRasterPos2s");
   pragma Import (StdCall, glRasterPos3d, "glRasterPos3d");
   pragma Import (StdCall, glRasterPos3f, "glRasterPos3f");
   pragma Import (StdCall, glRasterPos3i, "glRasterPos3i");
   pragma Import (StdCall, glRasterPos3s, "glRasterPos3s");
   pragma Import (StdCall, glRasterPos4d, "glRasterPos4d");
   pragma Import (StdCall, glRasterPos4f, "glRasterPos4f");
   pragma Import (StdCall, glRasterPos4i, "glRasterPos4i");
   pragma Import (StdCall, glRasterPos4s, "glRasterPos4s");
   pragma Import (StdCall, glRasterPos2dv, "glRasterPos2dv");
   pragma Import (StdCall, glRasterPos2fv, "glRasterPos2fv");
   pragma Import (StdCall, glRasterPos2iv, "glRasterPos2iv");
   pragma Import (StdCall, glRasterPos2sv, "glRasterPos2sv");
   pragma Import (StdCall, glRasterPos3dv, "glRasterPos3dv");
   pragma Import (StdCall, glRasterPos3fv, "glRasterPos3fv");
   pragma Import (StdCall, glRasterPos3iv, "glRasterPos3iv");
   pragma Import (StdCall, glRasterPos3sv, "glRasterPos3sv");
   pragma Import (StdCall, glRasterPos4dv, "glRasterPos4dv");
   pragma Import (StdCall, glRasterPos4fv, "glRasterPos4fv");
   pragma Import (StdCall, glRasterPos4iv, "glRasterPos4iv");
   pragma Import (StdCall, glRasterPos4sv, "glRasterPos4sv");
   pragma Import (StdCall, glRectd, "glRectd");
   pragma Import (StdCall, glRectf, "glRectf");
   pragma Import (StdCall, glRecti, "glRecti");
   pragma Import (StdCall, glRects, "glRects");
   pragma Import (StdCall, glRectdv, "glRectdv");
   pragma Import (StdCall, glRectfv, "glRectfv");
   pragma Import (StdCall, glRectiv, "glRectiv");
   pragma Import (StdCall, glRectsv, "glRectsv");
   pragma Import (StdCall, glVertexPointer, "glVertexPointer");
   pragma Import (StdCall, glNormalPointer, "glNormalPointer");
   pragma Import (StdCall, glColorPointer, "glColorPointer");
   pragma Import (StdCall, glIndexPointer, "glIndexPointer");
   pragma Import (StdCall, glTexCoordPointer, "glTexCoordPointer");
   pragma Import (StdCall, glEdgeFlagPointer, "glEdgeFlagPointer");
   pragma Import (StdCall, glGetPointerv, "glGetPointerv");
   pragma Import (StdCall, glArrayElement, "glArrayElement");
   pragma Import (StdCall, glDrawArrays, "glDrawArrays");
   pragma Import (StdCall, glDrawElements, "glDrawElements");
   pragma Import (StdCall, glInterleavedArrays, "glInterleavedArrays");
   pragma Import (StdCall, glShadeModel, "glShadeModel");
   pragma Import (StdCall, glLightf, "glLightf");
   pragma Import (StdCall, glLighti, "glLighti");
   pragma Import (StdCall, glLightfv, "glLightfv");
   pragma Import (StdCall, glLightiv, "glLightiv");
   pragma Import (StdCall, glGetLightfv, "glGetLightfv");
   pragma Import (StdCall, glGetLightiv, "glGetLightiv");
   pragma Import (StdCall, glLightModelf, "glLightModelf");
   pragma Import (StdCall, glLightModeli, "glLightModeli");
   pragma Import (StdCall, glLightModelfv, "glLightModelfv");
   pragma Import (StdCall, glLightModeliv, "glLightModeliv");
   pragma Import (StdCall, glMaterialf, "glMaterialf");
   pragma Import (StdCall, glMateriali, "glMateriali");
   pragma Import (StdCall, glMaterialfv, "glMaterialfv");
   pragma Import (StdCall, glMaterialiv, "glMaterialiv");
   pragma Import (StdCall, glGetMaterialfv, "glGetMaterialfv");
   pragma Import (StdCall, glGetMaterialiv, "glGetMaterialiv");
   pragma Import (StdCall, glColorMaterial, "glColorMaterial");
   pragma Import (StdCall, glPixelZoom, "glPixelZoom");
   pragma Import (StdCall, glPixelStoref, "glPixelStoref");
   pragma Import (StdCall, glPixelStorei, "glPixelStorei");
   pragma Import (StdCall, glPixelTransferf, "glPixelTransferf");
   pragma Import (StdCall, glPixelTransferi, "glPixelTransferi");
   pragma Import (StdCall, glPixelMapfv, "glPixelMapfv");
   pragma Import (StdCall, glPixelMapuiv, "glPixelMapuiv");
   pragma Import (StdCall, glPixelMapusv, "glPixelMapusv");
   pragma Import (StdCall, glGetPixelMapfv, "glGetPixelMapfv");
   pragma Import (StdCall, glGetPixelMapuiv, "glGetPixelMapuiv");
   pragma Import (StdCall, glGetPixelMapusv, "glGetPixelMapusv");
   pragma Import (StdCall, glBitmap, "glBitmap");
   pragma Import (StdCall, glReadPixels, "glReadPixels");
   pragma Import (StdCall, glDrawPixels, "glDrawPixels");
   pragma Import (StdCall, glCopyPixels, "glCopyPixels");
   pragma Import (StdCall, glStencilFunc, "glStencilFunc");
   pragma Import (StdCall, glStencilMask, "glStencilMask");
   pragma Import (StdCall, glStencilOp, "glStencilOp");
   pragma Import (StdCall, glClearStencil, "glClearStencil");
   pragma Import (StdCall, glTexGend, "glTexGend");
   pragma Import (StdCall, glTexGenf, "glTexGenf");
   pragma Import (StdCall, glTexGeni, "glTexGeni");
   pragma Import (StdCall, glTexGendv, "glTexGendv");
   pragma Import (StdCall, glTexGenfv, "glTexGenfv");
   pragma Import (StdCall, glTexGeniv, "glTexGeniv");
   pragma Import (StdCall, glGetTexGendv, "glGetTexGendv");
   pragma Import (StdCall, glGetTexGenfv, "glGetTexGenfv");
   pragma Import (StdCall, glGetTexGeniv, "glGetTexGeniv");
   pragma Import (StdCall, glTexEnvf, "glTexEnvf");
   pragma Import (StdCall, glTexEnvi, "glTexEnvi");
   pragma Import (StdCall, glTexEnvfv, "glTexEnvfv");
   pragma Import (StdCall, glTexEnviv, "glTexEnviv");
   pragma Import (StdCall, glGetTexEnvfv, "glGetTexEnvfv");
   pragma Import (StdCall, glGetTexEnviv, "glGetTexEnviv");
   pragma Import (StdCall, glTexParameterf, "glTexParameterf");
   pragma Import (StdCall, glTexParameteri, "glTexParameteri");
   pragma Import (StdCall, glTexParameterfv, "glTexParameterfv");
   pragma Import (StdCall, glTexParameteriv, "glTexParameteriv");
   pragma Import (StdCall, glGetTexParameterfv, "glGetTexParameterfv");
   pragma Import (StdCall, glGetTexParameteriv, "glGetTexParameteriv");
   pragma Import
     (StdCall,
      glGetTexLevelParameterfv,
      "glGetTexLevelParameterfv");
   pragma Import
     (StdCall,
      glGetTexLevelParameteriv,
      "glGetTexLevelParameteriv");
   pragma Import (StdCall, glTexImage1D, "glTexImage1D");
   pragma Import (StdCall, glTexImage2D, "glTexImage2D");
   pragma Import (StdCall, glGetTexImage, "glGetTexImage");
   pragma Import (StdCall, glGenTextures, "glGenTextures");
   pragma Import (StdCall, glDeleteTextures, "glDeleteTextures");
   pragma Import (StdCall, glBindTexture, "glBindTexture");
   pragma Import (StdCall, glPrioritizeTextures, "glPrioritizeTextures");
   pragma Import (StdCall, glAreTexturesResident, "glAreTexturesResident");
   pragma Import (StdCall, glIsTexture, "glIsTexture");
   pragma Import (StdCall, glTexSubImage1D, "glTexSubImage1D");
   pragma Import (StdCall, glTexSubImage2D, "glTexSubImage2D");
   pragma Import (StdCall, glCopyTexImage1D, "glCopyTexImage1D");
   pragma Import (StdCall, glCopyTexImage2D, "glCopyTexImage2D");
   pragma Import (StdCall, glCopyTexSubImage1D, "glCopyTexSubImage1D");
   pragma Import (StdCall, glCopyTexSubImage2D, "glCopyTexSubImage2D");
   pragma Import (StdCall, glMap1d, "glMap1d");
   pragma Import (StdCall, glMap1f, "glMap1f");
   pragma Import (StdCall, glMap2d, "glMap2d");
   pragma Import (StdCall, glMap2f, "glMap2f");
   pragma Import (StdCall, glGetMapdv, "glGetMapdv");
   pragma Import (StdCall, glGetMapfv, "glGetMapfv");
   pragma Import (StdCall, glGetMapiv, "glGetMapiv");
   pragma Import (StdCall, glEvalCoord1d, "glEvalCoord1d");
   pragma Import (StdCall, glEvalCoord1f, "glEvalCoord1f");
   pragma Import (StdCall, glEvalCoord1dv, "glEvalCoord1dv");
   pragma Import (StdCall, glEvalCoord1fv, "glEvalCoord1fv");
   pragma Import (StdCall, glEvalCoord2d, "glEvalCoord2d");
   pragma Import (StdCall, glEvalCoord2f, "glEvalCoord2f");
   pragma Import (StdCall, glEvalCoord2dv, "glEvalCoord2dv");
   pragma Import (StdCall, glEvalCoord2fv, "glEvalCoord2fv");
   pragma Import (StdCall, glMapGrid1d, "glMapGrid1d");
   pragma Import (StdCall, glMapGrid1f, "glMapGrid1f");
   pragma Import (StdCall, glMapGrid2d, "glMapGrid2d");
   pragma Import (StdCall, glMapGrid2f, "glMapGrid2f");
   pragma Import (StdCall, glEvalPoint1, "glEvalPoint1");
   pragma Import (StdCall, glEvalPoint2, "glEvalPoint2");
   pragma Import (StdCall, glEvalMesh1, "glEvalMesh1");
   pragma Import (StdCall, glEvalMesh2, "glEvalMesh2");
   pragma Import (StdCall, glFogf, "glFogf");
   pragma Import (StdCall, glFogi, "glFogi");
   pragma Import (StdCall, glFogfv, "glFogfv");
   pragma Import (StdCall, glFogiv, "glFogiv");
   pragma Import (StdCall, glFeedbackBuffer, "glFeedbackBuffer");
   pragma Import (StdCall, glPassThrough, "glPassThrough");
   pragma Import (StdCall, glSelectBuffer, "glSelectBuffer");
   pragma Import (StdCall, glInitNames, "glInitNames");
   pragma Import (StdCall, glLoadName, "glLoadName");
   pragma Import (StdCall, glPushName, "glPushName");
   pragma Import (StdCall, glPopName, "glPopName");
   pragma Import (StdCall, glDrawRangeElements, "glDrawRangeElements");
   pragma Import (StdCall, glTexImage3D, "glTexImage3D");
   pragma Import (StdCall, glTexSubImage3D, "glTexSubImage3D");
   pragma Import (StdCall, glCopyTexSubImage3D, "glCopyTexSubImage3D");
   pragma Import (StdCall, glColorTable, "glColorTable");
   pragma Import (StdCall, glColorSubTable, "glColorSubTable");
   pragma Import (StdCall, glColorTableParameteriv, "glColorTableParameteriv");
   pragma Import (StdCall, glColorTableParameterfv, "glColorTableParameterfv");
   pragma Import (StdCall, glCopyColorSubTable, "glCopyColorSubTable");
   pragma Import (StdCall, glCopyColorTable, "glCopyColorTable");
   pragma Import (StdCall, glGetColorTable, "glGetColorTable");
   pragma Import
     (StdCall,
      glGetColorTableParameterfv,
      "glGetColorTableParameterfv");
   pragma Import
     (StdCall,
      glGetColorTableParameteriv,
      "glGetColorTableParameteriv");
   pragma Import (StdCall, glBlendEquation, "glBlendEquation");
   pragma Import (StdCall, glBlendColor, "glBlendColor");
   pragma Import (StdCall, glHistogram, "glHistogram");
   pragma Import (StdCall, glResetHistogram, "glResetHistogram");
   pragma Import (StdCall, glGetHistogram, "glGetHistogram");
   pragma Import
     (StdCall,
      glGetHistogramParameterfv,
      "glGetHistogramParameterfv");
   pragma Import
     (StdCall,
      glGetHistogramParameteriv,
      "glGetHistogramParameteriv");
   pragma Import (StdCall, glMinmax, "glMinmax");
   pragma Import (StdCall, glResetMinmax, "glResetMinmax");
   pragma Import (StdCall, glGetMinmax, "glGetMinmax");
   pragma Import (StdCall, glGetMinmaxParameterfv, "glGetMinmaxParameterfv");
   pragma Import (StdCall, glGetMinmaxParameteriv, "glGetMinmaxParameteriv");
   pragma Import (StdCall, glConvolutionFilter1D, "glConvolutionFilter1D");
   pragma Import (StdCall, glConvolutionFilter2D, "glConvolutionFilter2D");
   pragma Import (StdCall, glConvolutionParameterf, "glConvolutionParameterf");
   pragma Import
     (StdCall,
      glConvolutionParameterfv,
      "glConvolutionParameterfv");
   pragma Import (StdCall, glConvolutionParameteri, "glConvolutionParameteri");
   pragma Import
     (StdCall,
      glConvolutionParameteriv,
      "glConvolutionParameteriv");
   pragma Import
     (StdCall,
      glCopyConvolutionFilter1D,
      "glCopyConvolutionFilter1D");
   pragma Import
     (StdCall,
      glCopyConvolutionFilter2D,
      "glCopyConvolutionFilter2D");
   pragma Import (StdCall, glGetConvolutionFilter, "glGetConvolutionFilter");
   pragma Import
     (StdCall,
      glGetConvolutionParameterfv,
      "glGetConvolutionParameterfv");
   pragma Import
     (StdCall,
      glGetConvolutionParameteriv,
      "glGetConvolutionParameteriv");
   pragma Import (StdCall, glSeparableFilter2D, "glSeparableFilter2D");
   pragma Import (StdCall, glGetSeparableFilter, "glGetSeparableFilter");
   pragma Import (StdCall, glActiveTexture, "glActiveTexture");
   pragma Import (StdCall, glClientActiveTexture, "glClientActiveTexture");
   pragma Import (StdCall, glCompressedTexImage1D, "glCompressedTexImage1D");
   pragma Import (StdCall, glCompressedTexImage2D, "glCompressedTexImage2D");
   pragma Import (StdCall, glCompressedTexImage3D, "glCompressedTexImage3D");
   pragma Import
     (StdCall,
      glCompressedTexSubImage1D,
      "glCompressedTexSubImage1D");
   pragma Import
     (StdCall,
      glCompressedTexSubImage2D,
      "glCompressedTexSubImage2D");
   pragma Import
     (StdCall,
      glCompressedTexSubImage3D,
      "glCompressedTexSubImage3D");
   pragma Import (StdCall, glGetCompressedTexImage, "glGetCompressedTexImage");
   pragma Import (StdCall, glMultiTexCoord1d, "glMultiTexCoord1d");
   pragma Import (StdCall, glMultiTexCoord1dv, "glMultiTexCoord1dv");
   pragma Import (StdCall, glMultiTexCoord1f, "glMultiTexCoord1f");
   pragma Import (StdCall, glMultiTexCoord1fv, "glMultiTexCoord1fv");
   pragma Import (StdCall, glMultiTexCoord1i, "glMultiTexCoord1i");
   pragma Import (StdCall, glMultiTexCoord1iv, "glMultiTexCoord1iv");
   pragma Import (StdCall, glMultiTexCoord1s, "glMultiTexCoord1s");
   pragma Import (StdCall, glMultiTexCoord1sv, "glMultiTexCoord1sv");
   pragma Import (StdCall, glMultiTexCoord2d, "glMultiTexCoord2d");
   pragma Import (StdCall, glMultiTexCoord2dv, "glMultiTexCoord2dv");
   pragma Import (StdCall, glMultiTexCoord2f, "glMultiTexCoord2f");
   pragma Import (StdCall, glMultiTexCoord2fv, "glMultiTexCoord2fv");
   pragma Import (StdCall, glMultiTexCoord2i, "glMultiTexCoord2i");
   pragma Import (StdCall, glMultiTexCoord2iv, "glMultiTexCoord2iv");
   pragma Import (StdCall, glMultiTexCoord2s, "glMultiTexCoord2s");
   pragma Import (StdCall, glMultiTexCoord2sv, "glMultiTexCoord2sv");
   pragma Import (StdCall, glMultiTexCoord3d, "glMultiTexCoord3d");
   pragma Import (StdCall, glMultiTexCoord3dv, "glMultiTexCoord3dv");
   pragma Import (StdCall, glMultiTexCoord3f, "glMultiTexCoord3f");
   pragma Import (StdCall, glMultiTexCoord3fv, "glMultiTexCoord3fv");
   pragma Import (StdCall, glMultiTexCoord3i, "glMultiTexCoord3i");
   pragma Import (StdCall, glMultiTexCoord3iv, "glMultiTexCoord3iv");
   pragma Import (StdCall, glMultiTexCoord3s, "glMultiTexCoord3s");
   pragma Import (StdCall, glMultiTexCoord3sv, "glMultiTexCoord3sv");
   pragma Import (StdCall, glMultiTexCoord4d, "glMultiTexCoord4d");
   pragma Import (StdCall, glMultiTexCoord4dv, "glMultiTexCoord4dv");
   pragma Import (StdCall, glMultiTexCoord4f, "glMultiTexCoord4f");
   pragma Import (StdCall, glMultiTexCoord4fv, "glMultiTexCoord4fv");
   pragma Import (StdCall, glMultiTexCoord4i, "glMultiTexCoord4i");
   pragma Import (StdCall, glMultiTexCoord4iv, "glMultiTexCoord4iv");
   pragma Import (StdCall, glMultiTexCoord4s, "glMultiTexCoord4s");
   pragma Import (StdCall, glMultiTexCoord4sv, "glMultiTexCoord4sv");
   pragma Import (StdCall, glLoadTransposeMatrixd, "glLoadTransposeMatrixd");
   pragma Import (StdCall, glLoadTransposeMatrixf, "glLoadTransposeMatrixf");
   pragma Import (StdCall, glMultTransposeMatrixd, "glMultTransposeMatrixd");
   pragma Import (StdCall, glMultTransposeMatrixf, "glMultTransposeMatrixf");
   pragma Import (StdCall, glSampleCoverage, "glSampleCoverage");
   pragma Import (StdCall, glActiveTextureARB, "glActiveTextureARB");
   pragma Import
     (StdCall,
      glClientActiveTextureARB,
      "glClientActiveTextureARB");
   pragma Import (StdCall, glMultiTexCoord1dARB, "glMultiTexCoord1dARB");
   pragma Import (StdCall, glMultiTexCoord1dvARB, "glMultiTexCoord1dvARB");
   pragma Import (StdCall, glMultiTexCoord1fARB, "glMultiTexCoord1fARB");
   pragma Import (StdCall, glMultiTexCoord1fvARB, "glMultiTexCoord1fvARB");
   pragma Import (StdCall, glMultiTexCoord1iARB, "glMultiTexCoord1iARB");
   pragma Import (StdCall, glMultiTexCoord1ivARB, "glMultiTexCoord1ivARB");
   pragma Import (StdCall, glMultiTexCoord1sARB, "glMultiTexCoord1sARB");
   pragma Import (StdCall, glMultiTexCoord1svARB, "glMultiTexCoord1svARB");
   pragma Import (StdCall, glMultiTexCoord2dARB, "glMultiTexCoord2dARB");
   pragma Import (StdCall, glMultiTexCoord2dvARB, "glMultiTexCoord2dvARB");
   pragma Import (StdCall, glMultiTexCoord2fARB, "glMultiTexCoord2fARB");
   pragma Import (StdCall, glMultiTexCoord2fvARB, "glMultiTexCoord2fvARB");
   pragma Import (StdCall, glMultiTexCoord2iARB, "glMultiTexCoord2iARB");
   pragma Import (StdCall, glMultiTexCoord2ivARB, "glMultiTexCoord2ivARB");
   pragma Import (StdCall, glMultiTexCoord2sARB, "glMultiTexCoord2sARB");
   pragma Import (StdCall, glMultiTexCoord2svARB, "glMultiTexCoord2svARB");
   pragma Import (StdCall, glMultiTexCoord3dARB, "glMultiTexCoord3dARB");
   pragma Import (StdCall, glMultiTexCoord3dvARB, "glMultiTexCoord3dvARB");
   pragma Import (StdCall, glMultiTexCoord3fARB, "glMultiTexCoord3fARB");
   pragma Import (StdCall, glMultiTexCoord3fvARB, "glMultiTexCoord3fvARB");
   pragma Import (StdCall, glMultiTexCoord3iARB, "glMultiTexCoord3iARB");
   pragma Import (StdCall, glMultiTexCoord3ivARB, "glMultiTexCoord3ivARB");
   pragma Import (StdCall, glMultiTexCoord3sARB, "glMultiTexCoord3sARB");
   pragma Import (StdCall, glMultiTexCoord3svARB, "glMultiTexCoord3svARB");
   pragma Import (StdCall, glMultiTexCoord4dARB, "glMultiTexCoord4dARB");
   pragma Import (StdCall, glMultiTexCoord4dvARB, "glMultiTexCoord4dvARB");
   pragma Import (StdCall, glMultiTexCoord4fARB, "glMultiTexCoord4fARB");
   pragma Import (StdCall, glMultiTexCoord4fvARB, "glMultiTexCoord4fvARB");
   pragma Import (StdCall, glMultiTexCoord4iARB, "glMultiTexCoord4iARB");
   pragma Import (StdCall, glMultiTexCoord4ivARB, "glMultiTexCoord4ivARB");
   pragma Import (StdCall, glMultiTexCoord4sARB, "glMultiTexCoord4sARB");
   pragma Import (StdCall, glMultiTexCoord4svARB, "glMultiTexCoord4svARB");
   pragma Import
     (StdCall,
      glGetProgramRegisterfvMESA,
      "glGetProgramRegisterfvMESA");
   pragma Import
     (StdCall,
      glBlendEquationSeparateATI,
      "glBlendEquationSeparateATI");

end GL.Binding;
