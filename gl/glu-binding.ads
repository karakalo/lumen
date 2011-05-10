with gl.Pointers;
with glu.Pointers;



package glu.Binding is

   use GL,
       gl.Pointers,
       glu.Pointers;



   procedure gluBeginCurve (nurb : in GLUnurbs_Pointer);

   procedure gluBeginPolygon (tess : in GLUtesselator_Pointer);

   procedure gluBeginSurface (nurb : in GLUnurbs_Pointer);

   procedure gluBeginTrim (nurb : in GLUnurbs_Pointer);

   function gluBuild1DMipmapLevels
     (target         : in GLenum;
      internalFormat : in GLint;
      width          : in GLsizei;
      format         : in GLenum;
      the_type       : in GLenum;
      level          : in GLint;
      base           : in GLint;
      max            : in GLint;
      data           : in GLvoid_Pointer)
      return           GLint;

   function gluBuild1DMipmaps
     (target         : in GLenum;
      internalFormat : in GLint;
      width          : in GLsizei;
      format         : in GLenum;
      the_type       : in GLenum;
      data           : in GLvoid_Pointer)
      return           GLint;

   function gluBuild2DMipmapLevels
     (target         : in GLenum;
      internalFormat : in GLint;
      width          : in GLsizei;
      height         : in GLsizei;
      format         : in GLenum;
      the_type       : in GLenum;
      level          : in GLint;
      base           : in GLint;
      max            : in GLint;
      data           : in GLvoid_Pointer)
      return           GLint;

   function gluBuild2DMipmaps
     (target         : in GLenum;
      internalFormat : in GLint;
      width          : in GLsizei;
      height         : in GLsizei;
      format         : in GLenum;
      the_type       : in GLenum;
      data           : in GLvoid_Pointer)
      return           GLint;

   function gluBuild3DMipmapLevels
     (target         : in GLenum;
      internalFormat : in GLint;
      width          : in GLsizei;
      height         : in GLsizei;
      depth          : in GLsizei;
      format         : in GLenum;
      the_type       : in GLenum;
      level          : in GLint;
      base           : in GLint;
      max            : in GLint;
      data           : in GLvoid_Pointer)
      return           GLint;

   function gluBuild3DMipmaps
     (target         : in GLenum;
      internalFormat : in GLint;
      width          : in GLsizei;
      height         : in GLsizei;
      depth          : in GLsizei;
      format         : in GLenum;
      the_type       : in GLenum;
      data           : in GLvoid_Pointer)
      return           GLint;

   function gluCheckExtension
     (extName   : in GLubyte_Pointer;
      extString : in GLubyte_Pointer)
      return      GLboolean;

   procedure gluCylinder
     (quad   : in GLUquadric_Pointer;
      base   : in GLdouble;
      top    : in GLdouble;
      height : in GLdouble;
      slices : in GLint;
      stacks : in GLint);

   procedure gluDeleteNurbsRenderer
     (nurb : in GLUnurbs_Pointer);

   procedure gluDeleteQuadric (quad : in GLUquadric_Pointer);

   procedure gluDeleteTess (tess : in GLUtesselator_Pointer);

   procedure gluDisk
     (quad   : in GLUquadric_Pointer;
      inner  : in GLdouble;
      outer  : in GLdouble;
      slices : in GLint;
      loops  : in GLint);

   procedure gluEndCurve (nurb : in GLUnurbs_Pointer);

   procedure gluEndPolygon (tess : in GLUtesselator_Pointer);

   procedure gluEndSurface (nurb : in GLUnurbs_Pointer);

   procedure gluEndTrim (nurb : in GLUnurbs_Pointer);

   function gluErrorString
     (error : in GLenum)
      return  GLubyte_Pointer;

   procedure gluGetNurbsProperty
     (nurb     : in GLUnurbs_Pointer;
      property : in GLenum;
      data     : in GLfloat_Pointer);

   function gluGetString
     (name : in GLenum)
      return GLubyte_Pointer;

   procedure gluGetTessProperty
     (tess  : in GLUtesselator_Pointer;
      which : in GLenum;
      data  : in GLdouble_Pointer);

   procedure gluLoadSamplingMatrices
     (nurb        : in GLUnurbs_Pointer;
      model       : in GLfloat_Pointer;
      perspective : in GLfloat_Pointer;
      view        : in GLint_Pointer);

   procedure gluLookAt
     (eyeX    : in GLdouble;
      eyeY    : in GLdouble;
      eyeZ    : in GLdouble;
      centerX : in GLdouble;
      centerY : in GLdouble;
      centerZ : in GLdouble;
      upX     : in GLdouble;
      upY     : in GLdouble;
      upZ     : in GLdouble);

   function gluNewNurbsRenderer return  GLUnurbs_Pointer;

   function gluNewQuadric return  GLUquadric_Pointer;

   function gluNewTess return  GLUtesselator_Pointer;

   procedure gluNextContour
     (tess     : in GLUtesselator_Pointer;
      the_type : in GLenum);

   procedure gluNurbsCallback
     (nurb         : in GLUnurbs_Pointer;
      which        : in GLenum;
      CallBackFunc : in glu.a_GLUfuncptr);

   procedure gluNurbsCallbackData
     (nurb     : in GLUnurbs_Pointer;
      userData : in GLvoid_Pointer);

   procedure gluNurbsCallbackDataEXT
     (nurb     : in GLUnurbs_Pointer;
      userData : in GLvoid_Pointer);

   procedure gluNurbsCurve
     (nurb      : in GLUnurbs_Pointer;
      knotCount : in GLint;
      knots     : in GLfloat_Pointer;
      stride    : in GLint;
      control   : in GLfloat_Pointer;
      order     : in GLint;
      the_type  : in GLenum);

   procedure gluNurbsProperty
     (nurb     : in GLUnurbs_Pointer;
      property : in GLenum;
      value    : in GLfloat);

   procedure gluNurbsSurface
     (nurb       : in GLUnurbs_Pointer;
      sKnotCount : in GLint;
      sKnots     : in GLfloat_Pointer;
      tKnotCount : in GLint;
      tKnots     : in GLfloat_Pointer;
      sStride    : in GLint;
      tStride    : in GLint;
      control    : in GLfloat_Pointer;
      sOrder     : in GLint;
      tOrder     : in GLint;
      the_type   : in GLenum);

   procedure gluOrtho2D
     (left   : in GLdouble;
      right  : in GLdouble;
      bottom : in GLdouble;
      top    : in GLdouble);

   procedure gluPartialDisk
     (quad   : in GLUquadric_Pointer;
      inner  : in GLdouble;
      outer  : in GLdouble;
      slices : in GLint;
      loops  : in GLint;
      start  : in GLdouble;
      sweep  : in GLdouble);

   procedure gluPerspective
     (fovy   : in GLdouble;
      aspect : in GLdouble;
      zNear  : in GLdouble;
      zFar   : in GLdouble);

   procedure gluPickMatrix
     (x        : in GLdouble;
      y        : in GLdouble;
      delX     : in GLdouble;
      delY     : in GLdouble;
      viewport : in GLint_Pointer);

   function gluProject
     (objX  : in GLdouble;
      objY  : in GLdouble;
      objZ  : in GLdouble;
      model : in GLdouble_Pointer;
      proj  : in GLdouble_Pointer;
      view  : in GLint_Pointer;
      winX  : in GLdouble_Pointer;
      winY  : in GLdouble_Pointer;
      winZ  : in GLdouble_Pointer)
      return  GLint;

   procedure gluPwlCurve
     (nurb     : in GLUnurbs_Pointer;
      count    : in GLint;
      data     : in GLfloat_Pointer;
      stride   : in GLint;
      the_type : in GLenum);

   procedure gluQuadricCallback
     (quad         : in GLUquadric_Pointer;
      which        : in GLenum;
      CallBackFunc : in glu.a_GLUfuncptr);

   procedure gluQuadricDrawStyle
     (quad : in GLUquadric_Pointer;
      draw : in GLenum);

   procedure gluQuadricNormals
     (quad   : in GLUquadric_Pointer;
      normal : in GLenum);

   procedure gluQuadricOrientation
     (quad        : in GLUquadric_Pointer;
      orientation : in GLenum);

   procedure gluQuadricTexture
     (quad    : in GLUquadric_Pointer;
      texture : in GLboolean);

   function gluScaleImage
     (format  : in GLenum;
      wIn     : in GLsizei;
      hIn     : in GLsizei;
      typeIn  : in GLenum;
      dataIn  : in GLvoid_Pointer;
      wOut    : in GLsizei;
      hOut    : in GLsizei;
      typeOut : in GLenum;
      dataOut : in GLvoid_Pointer)
      return    GLint;

   procedure gluSphere
     (quad   : in GLUquadric_Pointer;
      radius : in GLdouble;
      slices : in GLint;
      stacks : in GLint);

   procedure gluTessBeginContour
     (tess : in GLUtesselator_Pointer);

   procedure gluTessBeginPolygon
     (tess : in GLUtesselator_Pointer;
      data : in GLvoid_Pointer);

   procedure gluTessCallback
     (tess         : in GLUtesselator_Pointer;
      which        : in GLenum;
      CallBackFunc : in glu.a_GLUfuncptr);

   procedure gluTessEndContour
     (tess : in GLUtesselator_Pointer);

   procedure gluTessEndPolygon
     (tess : in GLUtesselator_Pointer);

   procedure gluTessNormal
     (tess   : in GLUtesselator_Pointer;
      valueX : in GLdouble;
      valueY : in GLdouble;
      valueZ : in GLdouble);

   procedure gluTessProperty
     (tess  : in GLUtesselator_Pointer;
      which : in GLenum;
      data  : in GLdouble);

   procedure gluTessVertex
     (tess     : in GLUtesselator_Pointer;
      location : in GLdouble_Pointer;
      data     : in GLvoid_Pointer);

   function gluUnProject
     (winX  : in GLdouble;
      winY  : in GLdouble;
      winZ  : in GLdouble;
      model : in GLdouble_Pointer;
      proj  : in GLdouble_Pointer;
      view  : in GLint_Pointer;
      objX  : in GLdouble_Pointer;
      objY  : in GLdouble_Pointer;
      objZ  : in GLdouble_Pointer)
      return  GLint;

   function gluUnProject4
     (winX    : in GLdouble;
      winY    : in GLdouble;
      winZ    : in GLdouble;
      clipW   : in GLdouble;
      model   : in GLdouble_Pointer;
      proj    : in GLdouble_Pointer;
      view    : in GLint_Pointer;
      nearVal : in GLdouble;
      farVal  : in GLdouble;
      objX    : in GLdouble_Pointer;
      objY    : in GLdouble_Pointer;
      objZ    : in GLdouble_Pointer;
      objW    : in GLdouble_Pointer)
      return    GLint;

private

   pragma Import (C, gluBeginCurve, "gluBeginCurve");
   pragma Import (C, gluBeginPolygon, "gluBeginPolygon");
   pragma Import (C, gluBeginSurface, "gluBeginSurface");
   pragma Import (C, gluBeginTrim, "gluBeginTrim");
   pragma Import (C, gluBuild1DMipmapLevels, "gluBuild1DMipmapLevels");
   pragma Import (C, gluBuild1DMipmaps, "gluBuild1DMipmaps");
   pragma Import (C, gluBuild2DMipmapLevels, "gluBuild2DMipmapLevels");
   pragma Import (C, gluBuild2DMipmaps, "gluBuild2DMipmaps");
   pragma Import (C, gluBuild3DMipmapLevels, "gluBuild3DMipmapLevels");
   pragma Import (C, gluBuild3DMipmaps, "gluBuild3DMipmaps");
   pragma Import (C, gluCheckExtension, "gluCheckExtension");
   pragma Import (C, gluCylinder, "gluCylinder");
   pragma Import (C, gluDeleteNurbsRenderer, "gluDeleteNurbsRenderer");
   pragma Import (C, gluDeleteQuadric, "gluDeleteQuadric");
   pragma Import (C, gluDeleteTess, "gluDeleteTess");
   pragma Import (C, gluDisk, "gluDisk");
   pragma Import (C, gluEndCurve, "gluEndCurve");
   pragma Import (C, gluEndPolygon, "gluEndPolygon");
   pragma Import (C, gluEndSurface, "gluEndSurface");
   pragma Import (C, gluEndTrim, "gluEndTrim");
   pragma Import (C, gluErrorString, "gluErrorString");
   pragma Import (C, gluGetNurbsProperty, "gluGetNurbsProperty");
   pragma Import (C, gluGetString, "gluGetString");
   pragma Import (C, gluGetTessProperty, "gluGetTessProperty");
   pragma Import (C, gluLoadSamplingMatrices, "gluLoadSamplingMatrices");
   pragma Import (C, gluLookAt, "gluLookAt");
   pragma Import (C, gluNewNurbsRenderer, "gluNewNurbsRenderer");
   pragma Import (C, gluNewQuadric, "gluNewQuadric");
   pragma Import (C, gluNewTess, "gluNewTess");
   pragma Import (C, gluNextContour, "gluNextContour");
   pragma Import (C, gluNurbsCallback, "gluNurbsCallback");
   pragma Import (C, gluNurbsCallbackData, "gluNurbsCallbackData");
   pragma Import (C, gluNurbsCallbackDataEXT, "gluNurbsCallbackDataEXT");
   pragma Import (C, gluNurbsCurve, "gluNurbsCurve");
   pragma Import (C, gluNurbsProperty, "gluNurbsProperty");
   pragma Import (C, gluNurbsSurface, "gluNurbsSurface");
   pragma Import (C, gluOrtho2D, "gluOrtho2D");
   pragma Import (C, gluPartialDisk, "gluPartialDisk");
   pragma Import (C, gluPerspective, "gluPerspective");
   pragma Import (C, gluPickMatrix, "gluPickMatrix");
   pragma Import (C, gluProject, "gluProject");
   pragma Import (C, gluPwlCurve, "gluPwlCurve");
   pragma Import (C, gluQuadricCallback, "gluQuadricCallback");
   pragma Import (C, gluQuadricDrawStyle, "gluQuadricDrawStyle");
   pragma Import (C, gluQuadricNormals, "gluQuadricNormals");
   pragma Import (C, gluQuadricOrientation, "gluQuadricOrientation");
   pragma Import (C, gluQuadricTexture, "gluQuadricTexture");
   pragma Import (C, gluScaleImage, "gluScaleImage");
   pragma Import (C, gluSphere, "gluSphere");
   pragma Import (C, gluTessBeginContour, "gluTessBeginContour");
   pragma Import (C, gluTessBeginPolygon, "gluTessBeginPolygon");
   pragma Import (C, gluTessCallback, "gluTessCallback");
   pragma Import (C, gluTessEndContour, "gluTessEndContour");
   pragma Import (C, gluTessEndPolygon, "gluTessEndPolygon");
   pragma Import (C, gluTessNormal, "gluTessNormal");
   pragma Import (C, gluTessProperty, "gluTessProperty");
   pragma Import (C, gluTessVertex, "gluTessVertex");
   pragma Import (C, gluUnProject, "gluUnProject");
   pragma Import (C, gluUnProject4, "gluUnProject4");

end glu.Binding;
