with GLext.Pointers,
     GL   .Pointers;



package GLext.Binding is

   use GL,
       GL   .Pointers,
       GLext.Pointers;


   procedure glBlendFuncSeparate
     (sfactorRGB   : in GLenum;
      dfactorRGB   : in GLenum;
      sfactorAlpha : in GLenum;
      dfactorAlpha : in GLenum);

   procedure glMultiDrawArrays
     (mode      : in GLenum;
      first     : in GLint_Pointer;
      count     : in GLsizei_Pointer;
      primcount : in GLsizei);

   procedure glMultiDrawElements
     (mode      : in GLenum;
      count     : in GLsizei_Pointer;
      the_type  : in GLenum;
      indices   : in GLvoid_Pointer;
      primcount : in GLsizei);

   procedure glPointParameterf
     (pname : in GLenum;
      param : in GLfloat);

   procedure glPointParameterfv
     (pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glPointParameteri (pname : in GLenum; param : in GLint);

   procedure glPointParameteriv
     (pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glGenQueries
     (n   : in GLsizei;
      ids : in GLuint_Pointer);

   procedure glDeleteQueries
     (n   : in GLsizei;
      ids : in GLuint_Pointer);

   function glIsQuery (id : in GLuint) return GLboolean;

   procedure glBeginQuery (target : in GLenum; id : in GLuint);

   procedure glEndQuery (target : in GLenum);

   procedure glGetQueryiv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glGetQueryObjectiv
     (id     : in GLuint;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glGetQueryObjectuiv
     (id     : in GLuint;
      pname  : in GLenum;
      params : in GLuint_Pointer);

   procedure glBindBuffer (target : in GLenum; buffer : in GLuint);

   procedure glDeleteBuffers
     (n       : in GLsizei;
      buffers : in GLuint_Pointer);

   procedure glGenBuffers
     (n       : in GLsizei;
      buffers : in GLuint_Pointer);

   function glIsBuffer (buffer : in GLuint) return GLboolean;

   procedure glBufferData
     (target : in GLenum;
      size   : in glext.GLsizeiptr;
      data   : in GLvoid_Pointer;
      usage  : in GLenum);

   procedure glBufferSubData
     (target : in GLenum;
      offset : in glext.GLintptr;
      size   : in glext.GLsizeiptr;
      data   : in GLvoid_Pointer);

   procedure glGetBufferSubData
     (target : in GLenum;
      offset : in glext.GLintptr;
      size   : in glext.GLsizeiptr;
      data   : in GLvoid_Pointer);

   function glMapBuffer
     (target     : in GLenum;
      the_access : in GLenum)
      return       GLvoid_Pointer;

   function glUnmapBuffer (target : in GLenum) return GLboolean;

   procedure glGetBufferParameteriv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glGetBufferPointerv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLvoid_Pointer);

   procedure glBlendEquationSeparate
     (modeRGB   : in GLenum;
      modeAlpha : in GLenum);

   procedure glDrawBuffers
     (n    : in GLsizei;
      bufs : in GLenum_Pointer);

   procedure glStencilOpSeparate
     (face   : in GLenum;
      sfail  : in GLenum;
      dpfail : in GLenum;
      dppass : in GLenum);

   procedure glStencilFuncSeparate
     (frontfunc : in GLenum;
      backfunc  : in GLenum;
      ref       : in GLint;
      mask      : in GLuint);

   procedure glStencilMaskSeparate
     (face : in GLenum;
      mask : in GLuint);

   procedure glAttachShader (program : in GLuint; shader : in GLuint);

   procedure glBindAttribLocation
     (program : in GLuint;
      index   : in GLuint;
      name    : in GLchar_Pointer);

   procedure glCompileShader (shader : in GLuint);

   function glCreateProgram return  GLuint;

   function glCreateShader (the_type : in GLenum) return GLuint;

   procedure glDeleteProgram (program : in GLuint);

   procedure glDeleteShader (shader : in GLuint);

   procedure glDetachShader (program : in GLuint; shader : in GLuint);

   procedure glDisableVertexAttribArray (index : in GLuint);

   procedure glEnableVertexAttribArray (index : in GLuint);

   procedure glGetActiveAttrib
     (program  : in GLuint;
      index    : in GLuint;
      bufSize  : in GLsizei;
      length   : in GLsizei_Pointer;
      size     : in GLint_Pointer;
      the_type : in GLenum_Pointer;
      name     : in GLchar_Pointer);

   procedure glGetActiveUniform
     (program  : in GLuint;
      index    : in GLuint;
      bufSize  : in GLsizei;
      length   : in GLsizei_Pointer;
      size     : in GLint_Pointer;
      the_type : in GLenum_Pointer;
      name     : in GLchar_Pointer);

   procedure glGetAttachedShaders
     (program  : in GLuint;
      maxCount : in GLsizei;
      count    : in GLsizei_Pointer;
      obj      : in GLuint_Pointer);

   function glGetAttribLocation
     (program : in GLuint;
      name    : in GLchar_Pointer)
      return    GLint;

   procedure glGetProgramiv
     (program : in GLuint;
      pname   : in GLenum;
      params  : in GLint_Pointer);

   procedure glGetProgramInfoLog
     (program : in GLuint;
      bufSize : in GLsizei;
      length  : in GLsizei_Pointer;
      infoLog : in GLchar_Pointer);

   procedure glGetShaderiv
     (shader : in GLuint;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glGetShaderInfoLog
     (shader  : in GLuint;
      bufSize : in GLsizei;
      length  : in GLsizei_Pointer;
      infoLog : in GLchar_Pointer);

   procedure glGetShaderSource
     (shader  : in GLuint;
      bufSize : in GLsizei;
      length  : in GLsizei_Pointer;
      source  : in GLchar_Pointer);

   function glGetUniformLocation
     (program : in GLuint;
      name    : in GLchar_Pointer)
      return    GLint;

   procedure glGetUniformfv
     (program  : in GLuint;
      location : in GLint;
      params   : in GLfloat_Pointer);

   procedure glGetUniformiv
     (program  : in GLuint;
      location : in GLint;
      params   : in GLint_Pointer);

   procedure glGetVertexAttribdv
     (index  : in GLuint;
      pname  : in GLenum;
      params : in GLdouble_Pointer);

   procedure glGetVertexAttribfv
     (index  : in GLuint;
      pname  : in GLenum;
      params : in GLfloat_Pointer);

   procedure glGetVertexAttribiv
     (index  : in GLuint;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glGetVertexAttribPointerv
     (index   : in GLuint;
      pname   : in GLenum;
      pointer : in GLvoid_Pointer);

   function glIsProgram (program : in GLuint) return GLboolean;

   function glIsShader (shader : in GLuint) return GLboolean;

   procedure glLinkProgram (program : in GLuint);

   procedure glShaderSource
     (shader : in GLuint;
      count  : in GLsizei;
      string : access GLchar_Pointer;
      length : in GLint_Pointer);

   procedure glUseProgram (program : in GLuint);

   procedure glUniform1f (location : in GLint; v0 : in GLfloat);

   procedure glUniform2f
     (location : in GLint;
      v0       : in GLfloat;
      v1       : in GLfloat);

   procedure glUniform3f
     (location : in GLint;
      v0       : in GLfloat;
      v1       : in GLfloat;
      v2       : in GLfloat);

   procedure glUniform4f
     (location : in GLint;
      v0       : in GLfloat;
      v1       : in GLfloat;
      v2       : in GLfloat;
      v3       : in GLfloat);

   procedure glUniform1i (location : in GLint; v0 : in GLint);

   procedure glUniform2i
     (location : in GLint;
      v0       : in GLint;
      v1       : in GLint);

   procedure glUniform3i
     (location : in GLint;
      v0       : in GLint;
      v1       : in GLint;
      v2       : in GLint);

   procedure glUniform4i
     (location : in GLint;
      v0       : in GLint;
      v1       : in GLint;
      v2       : in GLint;
      v3       : in GLint);

   procedure glUniform1fv
     (location : in GLint;
      count    : in GLsizei;
      value    : in GLfloat_Pointer);

   procedure glUniform2fv
     (location : in GLint;
      count    : in GLsizei;
      value    : in GLfloat_Pointer);

   procedure glUniform3fv
     (location : in GLint;
      count    : in GLsizei;
      value    : in GLfloat_Pointer);

   procedure glUniform4fv
     (location : in GLint;
      count    : in GLsizei;
      value    : in GLfloat_Pointer);

   procedure glUniform1iv
     (location : in GLint;
      count    : in GLsizei;
      value    : in GLint_Pointer);

   procedure glUniform2iv
     (location : in GLint;
      count    : in GLsizei;
      value    : in GLint_Pointer);

   procedure glUniform3iv
     (location : in GLint;
      count    : in GLsizei;
      value    : in GLint_Pointer);

   procedure glUniform4iv
     (location : in GLint;
      count    : in GLsizei;
      value    : in GLint_Pointer);

   procedure glUniformMatrix2fv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLfloat_Pointer);

   procedure glUniformMatrix3fv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLfloat_Pointer);

   procedure glUniformMatrix4fv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLfloat_Pointer);

   procedure glValidateProgram (program : in GLuint);

   procedure glVertexAttrib1d (index : in GLuint; x : in GLdouble);

   procedure glVertexAttrib1dv
     (index : in GLuint;
      v     : in GLdouble_Pointer);

   procedure glVertexAttrib1f (index : in GLuint; x : in GLfloat);

   procedure glVertexAttrib1fv
     (index : in GLuint;
      v     : in GLfloat_Pointer);

   procedure glVertexAttrib1s (index : in GLuint; x : in GLshort);

   procedure glVertexAttrib1sv
     (index : in GLuint;
      v     : in GLshort_Pointer);

   procedure glVertexAttrib2d
     (index : in GLuint;
      x     : in GLdouble;
      y     : in GLdouble);

   procedure glVertexAttrib2dv
     (index : in GLuint;
      v     : in GLdouble_Pointer);

   procedure glVertexAttrib2f
     (index : in GLuint;
      x     : in GLfloat;
      y     : in GLfloat);

   procedure glVertexAttrib2fv
     (index : in GLuint;
      v     : in GLfloat_Pointer);

   procedure glVertexAttrib2s
     (index : in GLuint;
      x     : in GLshort;
      y     : in GLshort);

   procedure glVertexAttrib2sv
     (index : in GLuint;
      v     : in GLshort_Pointer);

   procedure glVertexAttrib3d
     (index : in GLuint;
      x     : in GLdouble;
      y     : in GLdouble;
      z     : in GLdouble);

   procedure glVertexAttrib3dv
     (index : in GLuint;
      v     : in GLdouble_Pointer);

   procedure glVertexAttrib3f
     (index : in GLuint;
      x     : in GLfloat;
      y     : in GLfloat;
      z     : in GLfloat);

   procedure glVertexAttrib3fv
     (index : in GLuint;
      v     : in GLfloat_Pointer);

   procedure glVertexAttrib3s
     (index : in GLuint;
      x     : in GLshort;
      y     : in GLshort;
      z     : in GLshort);

   procedure glVertexAttrib3sv
     (index : in GLuint;
      v     : in GLshort_Pointer);

   procedure glVertexAttrib4Nbv
     (index : in GLuint;
      v     : in GLbyte_Pointer);

   procedure glVertexAttrib4Niv
     (index : in GLuint;
      v     : in GLint_Pointer);

   procedure glVertexAttrib4Nsv
     (index : in GLuint;
      v     : in GLshort_Pointer);

   procedure glVertexAttrib4Nub
     (index : in GLuint;
      x     : in GLubyte;
      y     : in GLubyte;
      z     : in GLubyte;
      w     : in GLubyte);

   procedure glVertexAttrib4Nubv
     (index : in GLuint;
      v     : in GLubyte_Pointer);

   procedure glVertexAttrib4Nuiv
     (index : in GLuint;
      v     : in GLuint_Pointer);

   procedure glVertexAttrib4Nusv
     (index : in GLuint;
      v     : in GLushort_Pointer);

   procedure glVertexAttrib4bv
     (index : in GLuint;
      v     : in GLbyte_Pointer);

   procedure glVertexAttrib4d
     (index : in GLuint;
      x     : in GLdouble;
      y     : in GLdouble;
      z     : in GLdouble;
      w     : in GLdouble);

   procedure glVertexAttrib4dv
     (index : in GLuint;
      v     : in GLdouble_Pointer);

   procedure glVertexAttrib4f
     (index : in GLuint;
      x     : in GLfloat;
      y     : in GLfloat;
      z     : in GLfloat;
      w     : in GLfloat);

   procedure glVertexAttrib4fv
     (index : in GLuint;
      v     : in GLfloat_Pointer);

   procedure glVertexAttrib4iv
     (index : in GLuint;
      v     : in GLint_Pointer);

   procedure glVertexAttrib4s
     (index : in GLuint;
      x     : in GLshort;
      y     : in GLshort;
      z     : in GLshort;
      w     : in GLshort);

   procedure glVertexAttrib4sv
     (index : in GLuint;
      v     : in GLshort_Pointer);

   procedure glVertexAttrib4ubv
     (index : in GLuint;
      v     : in GLubyte_Pointer);

   procedure glVertexAttrib4uiv
     (index : in GLuint;
      v     : in GLuint_Pointer);

   procedure glVertexAttrib4usv
     (index : in GLuint;
      v     : in GLushort_Pointer);

   procedure glVertexAttribPointer
     (index      : in GLuint;
      size       : in GLint;
      the_type   : in GLenum;
      normalized : in GLboolean;
      stride     : in GLsizei;
      pointer    : in GLvoid_Pointer);

   procedure glUniformMatrix2x3fv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLfloat_Pointer);

   procedure glUniformMatrix3x2fv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLfloat_Pointer);

   procedure glUniformMatrix2x4fv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLfloat_Pointer);

   procedure glUniformMatrix4x2fv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLfloat_Pointer);

   procedure glUniformMatrix3x4fv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLfloat_Pointer);

   procedure glUniformMatrix4x3fv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLfloat_Pointer);

   procedure glColorMaski
     (index : in GLuint;
      r     : in GLboolean;
      g     : in GLboolean;
      b     : in GLboolean;
      a     : in GLboolean);

   procedure glGetBooleani_v
     (target : in GLenum;
      index  : in GLuint;
      data   : in GLboolean_Pointer);

   procedure glGetIntegeri_v
     (target : in GLenum;
      index  : in GLuint;
      data   : in GLint_Pointer);

   procedure glEnablei (target : in GLenum; index : in GLuint);

   procedure glDisablei (target : in GLenum; index : in GLuint);

   function glIsEnabledi
     (target : in GLenum;
      index  : in GLuint)
      return   GLboolean;

   procedure glBeginTransformFeedback (primitiveMode : in GLenum);

   procedure glEndTransformFeedback;

   procedure glBindBufferRange
     (target : in GLenum;
      index  : in GLuint;
      buffer : in GLuint;
      offset : in glext.GLintptr;
      size   : in glext.GLsizeiptr);

   procedure glBindBufferBase
     (target : in GLenum;
      index  : in GLuint;
      buffer : in GLuint);

   procedure glTransformFeedbackVaryings
     (program    : in GLuint;
      count      : in GLsizei;
      varyings   : access GLchar_Pointer;
      bufferMode : in GLenum);

   procedure glGetTransformFeedbackVarying
     (program  : in GLuint;
      index    : in GLuint;
      bufSize  : in GLsizei;
      length   : in GLsizei_Pointer;
      size     : in GLsizei_Pointer;
      the_type : in GLenum_Pointer;
      name     : in GLchar_Pointer);

   procedure glClampColor (target : in GLenum; clamp : in GLenum);

   procedure glBeginConditionalRender
     (id   : in GLuint;
      mode : in GLenum);

   procedure glEndConditionalRender;

   procedure glVertexAttribIPointer
     (index    : in GLuint;
      size     : in GLint;
      the_type : in GLenum;
      stride   : in GLsizei;
      pointer  : in GLvoid_Pointer);

   procedure glGetVertexAttribIiv
     (index  : in GLuint;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glGetVertexAttribIuiv
     (index  : in GLuint;
      pname  : in GLenum;
      params : in GLuint_Pointer);

   procedure glVertexAttribI1i (index : in GLuint; x : in GLint);

   procedure glVertexAttribI2i
     (index : in GLuint;
      x     : in GLint;
      y     : in GLint);

   procedure glVertexAttribI3i
     (index : in GLuint;
      x     : in GLint;
      y     : in GLint;
      z     : in GLint);

   procedure glVertexAttribI4i
     (index : in GLuint;
      x     : in GLint;
      y     : in GLint;
      z     : in GLint;
      w     : in GLint);

   procedure glVertexAttribI1ui (index : in GLuint; x : in GLuint);

   procedure glVertexAttribI2ui
     (index : in GLuint;
      x     : in GLuint;
      y     : in GLuint);

   procedure glVertexAttribI3ui
     (index : in GLuint;
      x     : in GLuint;
      y     : in GLuint;
      z     : in GLuint);

   procedure glVertexAttribI4ui
     (index : in GLuint;
      x     : in GLuint;
      y     : in GLuint;
      z     : in GLuint;
      w     : in GLuint);

   procedure glVertexAttribI1iv
     (index : in GLuint;
      v     : in GLint_Pointer);

   procedure glVertexAttribI2iv
     (index : in GLuint;
      v     : in GLint_Pointer);

   procedure glVertexAttribI3iv
     (index : in GLuint;
      v     : in GLint_Pointer);

   procedure glVertexAttribI4iv
     (index : in GLuint;
      v     : in GLint_Pointer);

   procedure glVertexAttribI1uiv
     (index : in GLuint;
      v     : in GLuint_Pointer);

   procedure glVertexAttribI2uiv
     (index : in GLuint;
      v     : in GLuint_Pointer);

   procedure glVertexAttribI3uiv
     (index : in GLuint;
      v     : in GLuint_Pointer);

   procedure glVertexAttribI4uiv
     (index : in GLuint;
      v     : in GLuint_Pointer);

   procedure glVertexAttribI4bv
     (index : in GLuint;
      v     : in GLbyte_Pointer);

   procedure glVertexAttribI4sv
     (index : in GLuint;
      v     : in GLshort_Pointer);

   procedure glVertexAttribI4ubv
     (index : in GLuint;
      v     : in GLubyte_Pointer);

   procedure glVertexAttribI4usv
     (index : in GLuint;
      v     : in GLushort_Pointer);

   procedure glGetUniformuiv
     (program  : in GLuint;
      location : in GLint;
      params   : in GLuint_Pointer);

   procedure glBindFragDataLocation
     (program : in GLuint;
      color   : in GLuint;
      name    : in GLchar_Pointer);

   function glGetFragDataLocation
     (program : in GLuint;
      name    : in GLchar_Pointer)
      return    GLint;

   procedure glUniform1ui (location : in GLint; v0 : in GLuint);

   procedure glUniform2ui
     (location : in GLint;
      v0       : in GLuint;
      v1       : in GLuint);

   procedure glUniform3ui
     (location : in GLint;
      v0       : in GLuint;
      v1       : in GLuint;
      v2       : in GLuint);

   procedure glUniform4ui
     (location : in GLint;
      v0       : in GLuint;
      v1       : in GLuint;
      v2       : in GLuint;
      v3       : in GLuint);

   procedure glUniform1uiv
     (location : in GLint;
      count    : in GLsizei;
      value    : in GLuint_Pointer);

   procedure glUniform2uiv
     (location : in GLint;
      count    : in GLsizei;
      value    : in GLuint_Pointer);

   procedure glUniform3uiv
     (location : in GLint;
      count    : in GLsizei;
      value    : in GLuint_Pointer);

   procedure glUniform4uiv
     (location : in GLint;
      count    : in GLsizei;
      value    : in GLuint_Pointer);

   procedure glTexParameterIiv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glTexParameterIuiv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLuint_Pointer);

   procedure glGetTexParameterIiv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   procedure glGetTexParameterIuiv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLuint_Pointer);

   procedure glClearBufferiv
     (buffer     : in GLenum;
      drawbuffer : in GLint;
      value      : in GLint_Pointer);

   procedure glClearBufferuiv
     (buffer     : in GLenum;
      drawbuffer : in GLint;
      value      : in GLuint_Pointer);

   procedure glClearBufferfv
     (buffer     : in GLenum;
      drawbuffer : in GLint;
      value      : in GLfloat_Pointer);

   procedure glClearBufferfi
     (buffer     : in GLenum;
      drawbuffer : in GLint;
      depth      : in GLfloat;
      stencil    : in GLint);

   function glGetStringi
     (name  : in GLenum;
      index : in GLuint)
      return  GLubyte_Pointer;

   procedure glDrawArraysInstanced
     (mode      : in GLenum;
      first     : in GLint;
      count     : in GLsizei;
      primcount : in GLsizei);

   procedure glDrawElementsInstanced
     (mode      : in GLenum;
      count     : in GLsizei;
      the_type  : in GLenum;
      indices   : in GLvoid_Pointer;
      primcount : in GLsizei);

   procedure glTexBuffer
     (target         : in GLenum;
      internalformat : in GLenum;
      buffer         : in GLuint);

   procedure glPrimitiveRestartIndex (index : in GLuint);

   procedure glGetInteger64i_v
     (target : in GLenum;
      index  : in GLuint;
      data   : in GLint64_Pointer);

   procedure glGetBufferParameteri64v
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLint64_Pointer);

   procedure glProgramParameteri
     (program : in GLuint;
      pname   : in GLenum;
      value   : in GLint);

   procedure glFramebufferTexture
     (target     : in GLenum;
      attachment : in GLenum;
      texture    : in GLuint;
      level      : in GLint);

   function glIsRenderbuffer
     (renderbuffer : in GLuint)
      return         GLboolean;

   procedure glBindRenderbuffer
     (target       : in GLenum;
      renderbuffer : in GLuint);

   procedure glDeleteRenderbuffers
     (n             : in GLsizei;
      renderbuffers : in GLuint_Pointer);

   procedure glGenRenderbuffers
     (n             : in GLsizei;
      renderbuffers : in GLuint_Pointer);

   procedure glRenderbufferStorage
     (target         : in GLenum;
      internalformat : in GLenum;
      width          : in GLsizei;
      height         : in GLsizei);

   procedure glGetRenderbufferParameteriv
     (target : in GLenum;
      pname  : in GLenum;
      params : in GLint_Pointer);

   function glIsFramebuffer
     (framebuffer : in GLuint)
      return        GLboolean;

   procedure glBindFramebuffer
     (target      : in GLenum;
      framebuffer : in GLuint);

   procedure glDeleteFramebuffers
     (n            : in GLsizei;
      framebuffers : in GLuint_Pointer);

   procedure glGenFramebuffers
     (n            : in GLsizei;
      framebuffers : in GLuint_Pointer);

   function glCheckFramebufferStatus
     (target : in GLenum)
      return   GLenum;

   procedure glFramebufferTexture1D
     (target     : in GLenum;
      attachment : in GLenum;
      textarget  : in GLenum;
      texture    : in GLuint;
      level      : in GLint);

   procedure glFramebufferTexture2D
     (target     : in GLenum;
      attachment : in GLenum;
      textarget  : in GLenum;
      texture    : in GLuint;
      level      : in GLint);

   procedure glFramebufferTexture3D
     (target     : in GLenum;
      attachment : in GLenum;
      textarget  : in GLenum;
      texture    : in GLuint;
      level      : in GLint;
      zoffset    : in GLint);

   procedure glFramebufferRenderbuffer
     (target             : in GLenum;
      attachment         : in GLenum;
      renderbuffertarget : in GLenum;
      renderbuffer       : in GLuint);

   procedure glGetFramebufferAttachmentParameteriv
     (target     : in GLenum;
      attachment : in GLenum;
      pname      : in GLenum;
      params     : in GLint_Pointer);

   procedure glGenerateMipmap (target : in GLenum);

   procedure glBlitFramebuffer
     (srcX0  : in GLint;
      srcY0  : in GLint;
      srcX1  : in GLint;
      srcY1  : in GLint;
      dstX0  : in GLint;
      dstY0  : in GLint;
      dstX1  : in GLint;
      dstY1  : in GLint;
      mask   : in GLbitfield;
      filter : in GLenum);

   procedure glRenderbufferStorageMultisample
     (target         : in GLenum;
      samples        : in GLsizei;
      internalformat : in GLenum;
      width          : in GLsizei;
      height         : in GLsizei);

   procedure glFramebufferTextureLayer
     (target     : in GLenum;
      attachment : in GLenum;
      texture    : in GLuint;
      level      : in GLint;
      layer      : in GLint);

   function glMapBufferRange
     (target     : in GLenum;
      offset     : in glext.GLintptr;
      length     : in glext.GLsizeiptr;
      the_access : in GLbitfield)
      return       GLvoid_Pointer;

   procedure glFlushMappedBufferRange
     (target : in GLenum;
      offset : in glext.GLintptr;
      length : in glext.GLsizeiptr);

   procedure glBindVertexArray (the_array : in GLuint);

   procedure glDeleteVertexArrays
     (n      : in GLsizei;
      arrays : in GLuint_Pointer);

   procedure glGenVertexArrays
     (n      : in GLsizei;
      arrays : in GLuint_Pointer);

   function glIsVertexArray (the_array : in GLuint) return GLboolean;

   procedure glGetUniformIndices
     (program        : in GLuint;
      uniformCount   : in GLsizei;
      uniformNames   : access GLchar_Pointer;
      uniformIndices : in GLuint_Pointer);

   procedure glGetActiveUniformsiv
     (program        : in GLuint;
      uniformCount   : in GLsizei;
      uniformIndices : in GLuint_Pointer;
      pname          : in GLenum;
      params         : in GLint_Pointer);

   procedure glGetActiveUniformName
     (program      : in GLuint;
      uniformIndex : in GLuint;
      bufSize      : in GLsizei;
      length       : in GLsizei_Pointer;
      uniformName  : in GLchar_Pointer);

   function glGetUniformBlockIndex
     (program          : in GLuint;
      uniformBlockName : in GLchar_Pointer)
      return             GLuint;

   procedure glGetActiveUniformBlockiv
     (program           : in GLuint;
      uniformBlockIndex : in GLuint;
      pname             : in GLenum;
      params            : in GLint_Pointer);

   procedure glGetActiveUniformBlockName
     (program           : in GLuint;
      uniformBlockIndex : in GLuint;
      bufSize           : in GLsizei;
      length            : in GLsizei_Pointer;
      uniformBlockName  : in GLchar_Pointer);

   procedure glUniformBlockBinding
     (program             : in GLuint;
      uniformBlockIndex   : in GLuint;
      uniformBlockBinding : in GLuint);

   procedure glCopyBufferSubData
     (readTarget  : in GLenum;
      writeTarget : in GLenum;
      readOffset  : in glext.GLintptr;
      writeOffset : in glext.GLintptr;
      size        : in glext.GLsizeiptr);

   procedure glDrawElementsBaseVertex
     (mode       : in GLenum;
      count      : in GLsizei;
      the_type   : in GLenum;
      indices    : in GLvoid_Pointer;
      basevertex : in GLint);

   procedure glDrawRangeElementsBaseVertex
     (mode       : in GLenum;
      start      : in GLuint;
      the_end    : in GLuint;
      count      : in GLsizei;
      the_type   : in GLenum;
      indices    : in GLvoid_Pointer;
      basevertex : in GLint);

   procedure glDrawElementsInstancedBaseVertex
     (mode       : in GLenum;
      count      : in GLsizei;
      the_type   : in GLenum;
      indices    : in GLvoid_Pointer;
      primcount  : in GLsizei;
      basevertex : in GLint);

   procedure glMultiDrawElementsBaseVertex
     (mode       : in GLenum;
      count      : in GLsizei_Pointer;
      the_type   : in GLenum;
      indices    : in GLvoid_Pointer;
      primcount  : in GLsizei;
      basevertex : in GLint_Pointer);

   procedure glProvokingVertex (mode : in GLenum);

   procedure glTexImage2DMultisample
     (target               : in GLenum;
      samples              : in GLsizei;
      internalformat       : in GLint;
      width                : in GLsizei;
      height               : in GLsizei;
      fixedsamplelocations : in GLboolean);

   procedure glTexImage3DMultisample
     (target               : in GLenum;
      samples              : in GLsizei;
      internalformat       : in GLint;
      width                : in GLsizei;
      height               : in GLsizei;
      depth                : in GLsizei;
      fixedsamplelocations : in GLboolean);

   procedure glGetMultisamplefv
     (pname : in GLenum;
      index : in GLuint;
      val   : in GLfloat_Pointer);

   procedure glSampleMaski (index : in GLuint; mask : in GLbitfield);

   procedure glBlendEquationi (buf : in GLuint; mode : in GLenum);

   procedure glBlendEquationSeparatei
     (buf       : in GLuint;
      modeRGB   : in GLenum;
      modeAlpha : in GLenum);

   procedure glBlendFunci
     (buf : in GLuint;
      src : in GLenum;
      dst : in GLenum);

   procedure glBlendFuncSeparatei
     (buf      : in GLuint;
      srcRGB   : in GLenum;
      dstRGB   : in GLenum;
      srcAlpha : in GLenum;
      dstAlpha : in GLenum);

   procedure glMinSampleShading (value : in GLclampf);

   procedure glBindFragDataLocationIndexed
     (program     : in GLuint;
      colorNumber : in GLuint;
      index       : in GLuint;
      name        : in GLchar_Pointer);

   function glGetFragDataIndex
     (program : in GLuint;
      name    : in GLchar_Pointer)
      return    GLint;

   procedure glGenSamplers
     (count    : in GLsizei;
      samplers : in GLuint_Pointer);

   procedure glDeleteSamplers
     (count    : in GLsizei;
      samplers : in GLuint_Pointer);

   function glIsSampler (sampler : in GLuint) return GLboolean;

   procedure glBindSampler (unit : in GLuint; sampler : in GLuint);

   procedure glSamplerParameteri
     (sampler : in GLuint;
      pname   : in GLenum;
      param   : in GLint);

   procedure glSamplerParameteriv
     (sampler : in GLuint;
      pname   : in GLenum;
      param   : in GLint_Pointer);

   procedure glSamplerParameterf
     (sampler : in GLuint;
      pname   : in GLenum;
      param   : in GLfloat);

   procedure glSamplerParameterfv
     (sampler : in GLuint;
      pname   : in GLenum;
      param   : in GLfloat_Pointer);

   procedure glSamplerParameterIiv
     (sampler : in GLuint;
      pname   : in GLenum;
      param   : in GLint_Pointer);

   procedure glSamplerParameterIuiv
     (sampler : in GLuint;
      pname   : in GLenum;
      param   : in GLuint_Pointer);

   procedure glGetSamplerParameteriv
     (sampler : in GLuint;
      pname   : in GLenum;
      params  : in GLint_Pointer);

   procedure glGetSamplerParameterIiv
     (sampler : in GLuint;
      pname   : in GLenum;
      params  : in GLint_Pointer);

   procedure glGetSamplerParameterfv
     (sampler : in GLuint;
      pname   : in GLenum;
      params  : in GLfloat_Pointer);

   procedure glGetSamplerParameterIfv
     (sampler : in GLuint;
      pname   : in GLenum;
      params  : in GLfloat_Pointer);

   procedure glQueryCounter (id : in GLuint; target : in GLenum);

   procedure glGetQueryObjecti64v
     (id     : in GLuint;
      pname  : in GLenum;
      params : in GLint64_Pointer);

   procedure glGetQueryObjectui64v
     (id     : in GLuint;
      pname  : in GLenum;
      params : in GLuint64_Pointer);

   procedure glVertexP2ui (the_type : in GLenum; value : in GLuint);

   procedure glVertexP2uiv
     (the_type : in GLenum;
      value    : in GLuint_Pointer);

   procedure glVertexP3ui (the_type : in GLenum; value : in GLuint);

   procedure glVertexP3uiv
     (the_type : in GLenum;
      value    : in GLuint_Pointer);

   procedure glVertexP4ui (the_type : in GLenum; value : in GLuint);

   procedure glVertexP4uiv
     (the_type : in GLenum;
      value    : in GLuint_Pointer);

   procedure glTexCoordP1ui
     (the_type : in GLenum;
      coords   : in GLuint);

   procedure glTexCoordP1uiv
     (the_type : in GLenum;
      coords   : in GLuint_Pointer);

   procedure glTexCoordP2ui
     (the_type : in GLenum;
      coords   : in GLuint);

   procedure glTexCoordP2uiv
     (the_type : in GLenum;
      coords   : in GLuint_Pointer);

   procedure glTexCoordP3ui
     (the_type : in GLenum;
      coords   : in GLuint);

   procedure glTexCoordP3uiv
     (the_type : in GLenum;
      coords   : in GLuint_Pointer);

   procedure glTexCoordP4ui
     (the_type : in GLenum;
      coords   : in GLuint);

   procedure glTexCoordP4uiv
     (the_type : in GLenum;
      coords   : in GLuint_Pointer);

   procedure glMultiTexCoordP1ui
     (texture  : in GLenum;
      the_type : in GLenum;
      coords   : in GLuint);

   procedure glMultiTexCoordP1uiv
     (texture  : in GLenum;
      the_type : in GLenum;
      coords   : in GLuint_Pointer);

   procedure glMultiTexCoordP2ui
     (texture  : in GLenum;
      the_type : in GLenum;
      coords   : in GLuint);

   procedure glMultiTexCoordP2uiv
     (texture  : in GLenum;
      the_type : in GLenum;
      coords   : in GLuint_Pointer);

   procedure glMultiTexCoordP3ui
     (texture  : in GLenum;
      the_type : in GLenum;
      coords   : in GLuint);

   procedure glMultiTexCoordP3uiv
     (texture  : in GLenum;
      the_type : in GLenum;
      coords   : in GLuint_Pointer);

   procedure glMultiTexCoordP4ui
     (texture  : in GLenum;
      the_type : in GLenum;
      coords   : in GLuint);

   procedure glMultiTexCoordP4uiv
     (texture  : in GLenum;
      the_type : in GLenum;
      coords   : in GLuint_Pointer);

   procedure glNormalP3ui (the_type : in GLenum; coords : in GLuint);

   procedure glNormalP3uiv
     (the_type : in GLenum;
      coords   : in GLuint_Pointer);

   procedure glColorP3ui (the_type : in GLenum; color : in GLuint);

   procedure glColorP3uiv
     (the_type : in GLenum;
      color    : in GLuint_Pointer);

   procedure glColorP4ui (the_type : in GLenum; color : in GLuint);

   procedure glColorP4uiv
     (the_type : in GLenum;
      color    : in GLuint_Pointer);

   procedure glSecondaryColorP3ui
     (the_type : in GLenum;
      color    : in GLuint);

   procedure glSecondaryColorP3uiv
     (the_type : in GLenum;
      color    : in GLuint_Pointer);

   procedure glVertexAttribP1ui
     (index      : in GLuint;
      the_type   : in GLenum;
      normalized : in GLboolean;
      value      : in GLuint);

   procedure glVertexAttribP1uiv
     (index      : in GLuint;
      the_type   : in GLenum;
      normalized : in GLboolean;
      value      : in GLuint_Pointer);

   procedure glVertexAttribP2ui
     (index      : in GLuint;
      the_type   : in GLenum;
      normalized : in GLboolean;
      value      : in GLuint);

   procedure glVertexAttribP2uiv
     (index      : in GLuint;
      the_type   : in GLenum;
      normalized : in GLboolean;
      value      : in GLuint_Pointer);

   procedure glVertexAttribP3ui
     (index      : in GLuint;
      the_type   : in GLenum;
      normalized : in GLboolean;
      value      : in GLuint);

   procedure glVertexAttribP3uiv
     (index      : in GLuint;
      the_type   : in GLenum;
      normalized : in GLboolean;
      value      : in GLuint_Pointer);

   procedure glVertexAttribP4ui
     (index      : in GLuint;
      the_type   : in GLenum;
      normalized : in GLboolean;
      value      : in GLuint);

   procedure glVertexAttribP4uiv
     (index      : in GLuint;
      the_type   : in GLenum;
      normalized : in GLboolean;
      value      : in GLuint_Pointer);

   procedure glDrawArraysIndirect
     (mode     : in GLenum;
      indirect : in GLvoid_Pointer);

   procedure glDrawElementsIndirect
     (mode     : in GLenum;
      the_type : in GLenum;
      indirect : in GLvoid_Pointer);

   procedure glUniform1d (location : in GLint; x : in GLdouble);

   procedure glUniform2d
     (location : in GLint;
      x        : in GLdouble;
      y        : in GLdouble);

   procedure glUniform3d
     (location : in GLint;
      x        : in GLdouble;
      y        : in GLdouble;
      z        : in GLdouble);

   procedure glUniform4d
     (location : in GLint;
      x        : in GLdouble;
      y        : in GLdouble;
      z        : in GLdouble;
      w        : in GLdouble);

   procedure glUniform1dv
     (location : in GLint;
      count    : in GLsizei;
      value    : in GLdouble_Pointer);

   procedure glUniform2dv
     (location : in GLint;
      count    : in GLsizei;
      value    : in GLdouble_Pointer);

   procedure glUniform3dv
     (location : in GLint;
      count    : in GLsizei;
      value    : in GLdouble_Pointer);

   procedure glUniform4dv
     (location : in GLint;
      count    : in GLsizei;
      value    : in GLdouble_Pointer);

   procedure glUniformMatrix2dv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   procedure glUniformMatrix3dv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   procedure glUniformMatrix4dv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   procedure glUniformMatrix2x3dv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   procedure glUniformMatrix2x4dv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   procedure glUniformMatrix3x2dv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   procedure glUniformMatrix3x4dv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   procedure glUniformMatrix4x2dv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   procedure glUniformMatrix4x3dv
     (location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   procedure glGetUniformdv
     (program  : in GLuint;
      location : in GLint;
      params   : in GLdouble_Pointer);

   procedure glProgramUniform1dEXT
     (program  : in GLuint;
      location : in GLint;
      x        : in GLdouble);

   procedure glProgramUniform2dEXT
     (program  : in GLuint;
      location : in GLint;
      x        : in GLdouble;
      y        : in GLdouble);

   procedure glProgramUniform3dEXT
     (program  : in GLuint;
      location : in GLint;
      x        : in GLdouble;
      y        : in GLdouble;
      z        : in GLdouble);

   procedure glProgramUniform4dEXT
     (program  : in GLuint;
      location : in GLint;
      x        : in GLdouble;
      y        : in GLdouble;
      z        : in GLdouble;
      w        : in GLdouble);

   procedure glProgramUniform1dvEXT
     (program  : in GLuint;
      location : in GLint;
      count    : in GLsizei;
      value    : in GLdouble_Pointer);

   procedure glProgramUniform2dvEXT
     (program  : in GLuint;
      location : in GLint;
      count    : in GLsizei;
      value    : in GLdouble_Pointer);

   procedure glProgramUniform3dvEXT
     (program  : in GLuint;
      location : in GLint;
      count    : in GLsizei;
      value    : in GLdouble_Pointer);

   procedure glProgramUniform4dvEXT
     (program  : in GLuint;
      location : in GLint;
      count    : in GLsizei;
      value    : in GLdouble_Pointer);

   procedure glProgramUniformMatrix2dvEXT
     (program   : in GLuint;
      location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   procedure glProgramUniformMatrix3dvEXT
     (program   : in GLuint;
      location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   procedure glProgramUniformMatrix4dvEXT
     (program   : in GLuint;
      location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   procedure glProgramUniformMatrix2x3dvEXT
     (program   : in GLuint;
      location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   procedure glProgramUniformMatrix2x4dvEXT
     (program   : in GLuint;
      location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   procedure glProgramUniformMatrix3x2dvEXT
     (program   : in GLuint;
      location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   procedure glProgramUniformMatrix3x4dvEXT
     (program   : in GLuint;
      location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   procedure glProgramUniformMatrix4x2dvEXT
     (program   : in GLuint;
      location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   procedure glProgramUniformMatrix4x3dvEXT
     (program   : in GLuint;
      location  : in GLint;
      count     : in GLsizei;
      transpose : in GLboolean;
      value     : in GLdouble_Pointer);

   function glGetSubroutineUniformLocation
     (program    : in GLuint;
      shadertype : in GLenum;
      name       : in GLchar_Pointer)
      return       GLint;

   function glGetSubroutineIndex
     (program    : in GLuint;
      shadertype : in GLenum;
      name       : in GLchar_Pointer)
      return       GLuint;

   procedure glGetActiveSubroutineUniformiv
     (program    : in GLuint;
      shadertype : in GLenum;
      index      : in GLuint;
      pname      : in GLenum;
      values     : in GLint_Pointer);

   procedure glGetActiveSubroutineUniformName
     (program    : in GLuint;
      shadertype : in GLenum;
      index      : in GLuint;
      bufsize    : in GLsizei;
      length     : in GLsizei_Pointer;
      name       : in GLchar_Pointer);

   procedure glGetActiveSubroutineName
     (program    : in GLuint;
      shadertype : in GLenum;
      index      : in GLuint;
      bufsize    : in GLsizei;
      length     : in GLsizei_Pointer;
      name       : in GLchar_Pointer);

   procedure glUniformSubroutinesuiv
     (shadertype : in GLenum;
      count      : in GLsizei;
      indices    : in GLuint_Pointer);

   procedure glGetUniformSubroutineuiv
     (shadertype : in GLenum;
      location   : in GLint;
      params     : in GLuint_Pointer);

   procedure glGetProgramStageiv
     (program    : in GLuint;
      shadertype : in GLenum;
      pname      : in GLenum;
      values     : in GLint_Pointer);

   procedure glPatchParameteri (pname : in GLenum; value : in GLint);

   procedure glPatchParameterfv
     (pname  : in GLenum;
      values : in GLfloat_Pointer);

   procedure glBindTransformFeedback
     (target : in GLenum;
      id     : in GLuint);

   procedure glDeleteTransformFeedbacks
     (n   : in GLsizei;
      ids : in GLuint_Pointer);

   procedure glGenTransformFeedbacks
     (n   : in GLsizei;
      ids : in GLuint_Pointer);

   function glIsTransformFeedback (id : in GLuint) return GLboolean;

   procedure glPauseTransformFeedback;

   procedure glResumeTransformFeedback;

   procedure glDrawTransformFeedback
     (mode : in GLenum;
      id   : in GLuint);

   procedure glDrawTransformFeedbackStream
     (mode   : in GLenum;
      id     : in GLuint;
      stream : in GLuint);

   procedure glBeginQueryIndexed
     (target : in GLenum;
      index  : in GLuint;
      id     : in GLuint);

   procedure glEndQueryIndexed
     (target : in GLenum;
      index  : in GLuint);

   procedure glGetQueryIndexediv
     (target : in GLenum;
      index  : in GLuint;
      pname  : in GLenum;
      params : in GLint_Pointer);

private

   pragma Import (StdCall, glBlendFuncSeparate, "glBlendFuncSeparate");
   pragma Import (StdCall, glMultiDrawArrays, "glMultiDrawArrays");
   pragma Import (StdCall, glMultiDrawElements, "glMultiDrawElements");
   pragma Import (StdCall, glPointParameterf, "glPointParameterf");
   pragma Import (StdCall, glPointParameterfv, "glPointParameterfv");
   pragma Import (StdCall, glPointParameteri, "glPointParameteri");
   pragma Import (StdCall, glPointParameteriv, "glPointParameteriv");
   pragma Import (StdCall, glGenQueries, "glGenQueries");
   pragma Import (StdCall, glDeleteQueries, "glDeleteQueries");
   pragma Import (StdCall, glIsQuery, "glIsQuery");
   pragma Import (StdCall, glBeginQuery, "glBeginQuery");
   pragma Import (StdCall, glEndQuery, "glEndQuery");
   pragma Import (StdCall, glGetQueryiv, "glGetQueryiv");
   pragma Import (StdCall, glGetQueryObjectiv, "glGetQueryObjectiv");
   pragma Import (StdCall, glGetQueryObjectuiv, "glGetQueryObjectuiv");
   pragma Import (StdCall, glBindBuffer, "glBindBuffer");
   pragma Import (StdCall, glDeleteBuffers, "glDeleteBuffers");
   pragma Import (StdCall, glGenBuffers, "glGenBuffers");
   pragma Import (StdCall, glIsBuffer, "glIsBuffer");
   pragma Import (StdCall, glBufferData, "glBufferData");
   pragma Import (StdCall, glBufferSubData, "glBufferSubData");
   pragma Import (StdCall, glGetBufferSubData, "glGetBufferSubData");
   pragma Import (StdCall, glMapBuffer, "glMapBuffer");
   pragma Import (StdCall, glUnmapBuffer, "glUnmapBuffer");
   pragma Import (StdCall, glGetBufferParameteriv, "glGetBufferParameteriv");
   pragma Import (StdCall, glGetBufferPointerv, "glGetBufferPointerv");
   pragma Import (StdCall, glBlendEquationSeparate, "glBlendEquationSeparate");
   pragma Import (StdCall, glDrawBuffers, "glDrawBuffers");
   pragma Import (StdCall, glStencilOpSeparate, "glStencilOpSeparate");
   pragma Import (StdCall, glStencilFuncSeparate, "glStencilFuncSeparate");
   pragma Import (StdCall, glStencilMaskSeparate, "glStencilMaskSeparate");
   pragma Import (StdCall, glAttachShader, "glAttachShader");
   pragma Import (StdCall, glBindAttribLocation, "glBindAttribLocation");
   pragma Import (StdCall, glCompileShader, "glCompileShader");
   pragma Import (StdCall, glCreateProgram, "glCreateProgram");
   pragma Import (StdCall, glCreateShader, "glCreateShader");
   pragma Import (StdCall, glDeleteProgram, "glDeleteProgram");
   pragma Import (StdCall, glDeleteShader, "glDeleteShader");
   pragma Import (StdCall, glDetachShader, "glDetachShader");
   pragma Import
     (StdCall,
      glDisableVertexAttribArray,
      "glDisableVertexAttribArray");
   pragma Import
     (StdCall,
      glEnableVertexAttribArray,
      "glEnableVertexAttribArray");
   pragma Import (StdCall, glGetActiveAttrib, "glGetActiveAttrib");
   pragma Import (StdCall, glGetActiveUniform, "glGetActiveUniform");
   pragma Import (StdCall, glGetAttachedShaders, "glGetAttachedShaders");
   pragma Import (StdCall, glGetAttribLocation, "glGetAttribLocation");
   pragma Import (StdCall, glGetProgramiv, "glGetProgramiv");
   pragma Import (StdCall, glGetProgramInfoLog, "glGetProgramInfoLog");
   pragma Import (StdCall, glGetShaderiv, "glGetShaderiv");
   pragma Import (StdCall, glGetShaderInfoLog, "glGetShaderInfoLog");
   pragma Import (StdCall, glGetShaderSource, "glGetShaderSource");
   pragma Import (StdCall, glGetUniformLocation, "glGetUniformLocation");
   pragma Import (StdCall, glGetUniformfv, "glGetUniformfv");
   pragma Import (StdCall, glGetUniformiv, "glGetUniformiv");
   pragma Import (StdCall, glGetVertexAttribdv, "glGetVertexAttribdv");
   pragma Import (StdCall, glGetVertexAttribfv, "glGetVertexAttribfv");
   pragma Import (StdCall, glGetVertexAttribiv, "glGetVertexAttribiv");
   pragma Import
     (StdCall,
      glGetVertexAttribPointerv,
      "glGetVertexAttribPointerv");
   pragma Import (StdCall, glIsProgram, "glIsProgram");
   pragma Import (StdCall, glIsShader, "glIsShader");
   pragma Import (StdCall, glLinkProgram, "glLinkProgram");
   pragma Import (StdCall, glShaderSource, "glShaderSource");
   pragma Import (StdCall, glUseProgram, "glUseProgram");
   pragma Import (StdCall, glUniform1f, "glUniform1f");
   pragma Import (StdCall, glUniform2f, "glUniform2f");
   pragma Import (StdCall, glUniform3f, "glUniform3f");
   pragma Import (StdCall, glUniform4f, "glUniform4f");
   pragma Import (StdCall, glUniform1i, "glUniform1i");
   pragma Import (StdCall, glUniform2i, "glUniform2i");
   pragma Import (StdCall, glUniform3i, "glUniform3i");
   pragma Import (StdCall, glUniform4i, "glUniform4i");
   pragma Import (StdCall, glUniform1fv, "glUniform1fv");
   pragma Import (StdCall, glUniform2fv, "glUniform2fv");
   pragma Import (StdCall, glUniform3fv, "glUniform3fv");
   pragma Import (StdCall, glUniform4fv, "glUniform4fv");
   pragma Import (StdCall, glUniform1iv, "glUniform1iv");
   pragma Import (StdCall, glUniform2iv, "glUniform2iv");
   pragma Import (StdCall, glUniform3iv, "glUniform3iv");
   pragma Import (StdCall, glUniform4iv, "glUniform4iv");
   pragma Import (StdCall, glUniformMatrix2fv, "glUniformMatrix2fv");
   pragma Import (StdCall, glUniformMatrix3fv, "glUniformMatrix3fv");
   pragma Import (StdCall, glUniformMatrix4fv, "glUniformMatrix4fv");
   pragma Import (StdCall, glValidateProgram, "glValidateProgram");
   pragma Import (StdCall, glVertexAttrib1d, "glVertexAttrib1d");
   pragma Import (StdCall, glVertexAttrib1dv, "glVertexAttrib1dv");
   pragma Import (StdCall, glVertexAttrib1f, "glVertexAttrib1f");
   pragma Import (StdCall, glVertexAttrib1fv, "glVertexAttrib1fv");
   pragma Import (StdCall, glVertexAttrib1s, "glVertexAttrib1s");
   pragma Import (StdCall, glVertexAttrib1sv, "glVertexAttrib1sv");
   pragma Import (StdCall, glVertexAttrib2d, "glVertexAttrib2d");
   pragma Import (StdCall, glVertexAttrib2dv, "glVertexAttrib2dv");
   pragma Import (StdCall, glVertexAttrib2f, "glVertexAttrib2f");
   pragma Import (StdCall, glVertexAttrib2fv, "glVertexAttrib2fv");
   pragma Import (StdCall, glVertexAttrib2s, "glVertexAttrib2s");
   pragma Import (StdCall, glVertexAttrib2sv, "glVertexAttrib2sv");
   pragma Import (StdCall, glVertexAttrib3d, "glVertexAttrib3d");
   pragma Import (StdCall, glVertexAttrib3dv, "glVertexAttrib3dv");
   pragma Import (StdCall, glVertexAttrib3f, "glVertexAttrib3f");
   pragma Import (StdCall, glVertexAttrib3fv, "glVertexAttrib3fv");
   pragma Import (StdCall, glVertexAttrib3s, "glVertexAttrib3s");
   pragma Import (StdCall, glVertexAttrib3sv, "glVertexAttrib3sv");
   pragma Import (StdCall, glVertexAttrib4Nbv, "glVertexAttrib4Nbv");
   pragma Import (StdCall, glVertexAttrib4Niv, "glVertexAttrib4Niv");
   pragma Import (StdCall, glVertexAttrib4Nsv, "glVertexAttrib4Nsv");
   pragma Import (StdCall, glVertexAttrib4Nub, "glVertexAttrib4Nub");
   pragma Import (StdCall, glVertexAttrib4Nubv, "glVertexAttrib4Nubv");
   pragma Import (StdCall, glVertexAttrib4Nuiv, "glVertexAttrib4Nuiv");
   pragma Import (StdCall, glVertexAttrib4Nusv, "glVertexAttrib4Nusv");
   pragma Import (StdCall, glVertexAttrib4bv, "glVertexAttrib4bv");
   pragma Import (StdCall, glVertexAttrib4d, "glVertexAttrib4d");
   pragma Import (StdCall, glVertexAttrib4dv, "glVertexAttrib4dv");
   pragma Import (StdCall, glVertexAttrib4f, "glVertexAttrib4f");
   pragma Import (StdCall, glVertexAttrib4fv, "glVertexAttrib4fv");
   pragma Import (StdCall, glVertexAttrib4iv, "glVertexAttrib4iv");
   pragma Import (StdCall, glVertexAttrib4s, "glVertexAttrib4s");
   pragma Import (StdCall, glVertexAttrib4sv, "glVertexAttrib4sv");
   pragma Import (StdCall, glVertexAttrib4ubv, "glVertexAttrib4ubv");
   pragma Import (StdCall, glVertexAttrib4uiv, "glVertexAttrib4uiv");
   pragma Import (StdCall, glVertexAttrib4usv, "glVertexAttrib4usv");
   pragma Import (StdCall, glVertexAttribPointer, "glVertexAttribPointer");
   pragma Import (StdCall, glUniformMatrix2x3fv, "glUniformMatrix2x3fv");
   pragma Import (StdCall, glUniformMatrix3x2fv, "glUniformMatrix3x2fv");
   pragma Import (StdCall, glUniformMatrix2x4fv, "glUniformMatrix2x4fv");
   pragma Import (StdCall, glUniformMatrix4x2fv, "glUniformMatrix4x2fv");
   pragma Import (StdCall, glUniformMatrix3x4fv, "glUniformMatrix3x4fv");
   pragma Import (StdCall, glUniformMatrix4x3fv, "glUniformMatrix4x3fv");
   pragma Import (StdCall, glColorMaski, "glColorMaski");
   pragma Import (StdCall, glGetBooleani_v, "glGetBooleani_v");
   pragma Import (StdCall, glGetIntegeri_v, "glGetIntegeri_v");
   pragma Import (StdCall, glEnablei, "glEnablei");
   pragma Import (StdCall, glDisablei, "glDisablei");
   pragma Import (StdCall, glIsEnabledi, "glIsEnabledi");
   pragma Import
     (StdCall,
      glBeginTransformFeedback,
      "glBeginTransformFeedback");
   pragma Import (StdCall, glEndTransformFeedback, "glEndTransformFeedback");
   pragma Import (StdCall, glBindBufferRange, "glBindBufferRange");
   pragma Import (StdCall, glBindBufferBase, "glBindBufferBase");
   pragma Import
     (StdCall,
      glTransformFeedbackVaryings,
      "glTransformFeedbackVaryings");
   pragma Import
     (StdCall,
      glGetTransformFeedbackVarying,
      "glGetTransformFeedbackVarying");
   pragma Import (StdCall, glClampColor, "glClampColor");
   pragma Import
     (StdCall,
      glBeginConditionalRender,
      "glBeginConditionalRender");
   pragma Import (StdCall, glEndConditionalRender, "glEndConditionalRender");
   pragma Import (StdCall, glVertexAttribIPointer, "glVertexAttribIPointer");
   pragma Import (StdCall, glGetVertexAttribIiv, "glGetVertexAttribIiv");
   pragma Import (StdCall, glGetVertexAttribIuiv, "glGetVertexAttribIuiv");
   pragma Import (StdCall, glVertexAttribI1i, "glVertexAttribI1i");
   pragma Import (StdCall, glVertexAttribI2i, "glVertexAttribI2i");
   pragma Import (StdCall, glVertexAttribI3i, "glVertexAttribI3i");
   pragma Import (StdCall, glVertexAttribI4i, "glVertexAttribI4i");
   pragma Import (StdCall, glVertexAttribI1ui, "glVertexAttribI1ui");
   pragma Import (StdCall, glVertexAttribI2ui, "glVertexAttribI2ui");
   pragma Import (StdCall, glVertexAttribI3ui, "glVertexAttribI3ui");
   pragma Import (StdCall, glVertexAttribI4ui, "glVertexAttribI4ui");
   pragma Import (StdCall, glVertexAttribI1iv, "glVertexAttribI1iv");
   pragma Import (StdCall, glVertexAttribI2iv, "glVertexAttribI2iv");
   pragma Import (StdCall, glVertexAttribI3iv, "glVertexAttribI3iv");
   pragma Import (StdCall, glVertexAttribI4iv, "glVertexAttribI4iv");
   pragma Import (StdCall, glVertexAttribI1uiv, "glVertexAttribI1uiv");
   pragma Import (StdCall, glVertexAttribI2uiv, "glVertexAttribI2uiv");
   pragma Import (StdCall, glVertexAttribI3uiv, "glVertexAttribI3uiv");
   pragma Import (StdCall, glVertexAttribI4uiv, "glVertexAttribI4uiv");
   pragma Import (StdCall, glVertexAttribI4bv, "glVertexAttribI4bv");
   pragma Import (StdCall, glVertexAttribI4sv, "glVertexAttribI4sv");
   pragma Import (StdCall, glVertexAttribI4ubv, "glVertexAttribI4ubv");
   pragma Import (StdCall, glVertexAttribI4usv, "glVertexAttribI4usv");
   pragma Import (StdCall, glGetUniformuiv, "glGetUniformuiv");
   pragma Import (StdCall, glBindFragDataLocation, "glBindFragDataLocation");
   pragma Import (StdCall, glGetFragDataLocation, "glGetFragDataLocation");
   pragma Import (StdCall, glUniform1ui, "glUniform1ui");
   pragma Import (StdCall, glUniform2ui, "glUniform2ui");
   pragma Import (StdCall, glUniform3ui, "glUniform3ui");
   pragma Import (StdCall, glUniform4ui, "glUniform4ui");
   pragma Import (StdCall, glUniform1uiv, "glUniform1uiv");
   pragma Import (StdCall, glUniform2uiv, "glUniform2uiv");
   pragma Import (StdCall, glUniform3uiv, "glUniform3uiv");
   pragma Import (StdCall, glUniform4uiv, "glUniform4uiv");
   pragma Import (StdCall, glTexParameterIiv, "glTexParameterIiv");
   pragma Import (StdCall, glTexParameterIuiv, "glTexParameterIuiv");
   pragma Import (StdCall, glGetTexParameterIiv, "glGetTexParameterIiv");
   pragma Import (StdCall, glGetTexParameterIuiv, "glGetTexParameterIuiv");
   pragma Import (StdCall, glClearBufferiv, "glClearBufferiv");
   pragma Import (StdCall, glClearBufferuiv, "glClearBufferuiv");
   pragma Import (StdCall, glClearBufferfv, "glClearBufferfv");
   pragma Import (StdCall, glClearBufferfi, "glClearBufferfi");
   pragma Import (StdCall, glGetStringi, "glGetStringi");
   pragma Import (StdCall, glDrawArraysInstanced, "glDrawArraysInstanced");
   pragma Import (StdCall, glDrawElementsInstanced, "glDrawElementsInstanced");
   pragma Import (StdCall, glTexBuffer, "glTexBuffer");
   pragma Import (StdCall, glPrimitiveRestartIndex, "glPrimitiveRestartIndex");
   pragma Import (StdCall, glGetInteger64i_v, "glGetInteger64i_v");
   pragma Import
     (StdCall,
      glGetBufferParameteri64v,
      "glGetBufferParameteri64v");
   pragma Import (StdCall, glProgramParameteri, "glProgramParameteri");
   pragma Import (StdCall, glFramebufferTexture, "glFramebufferTexture");
   pragma Import (StdCall, glIsRenderbuffer, "glIsRenderbuffer");
   pragma Import (StdCall, glBindRenderbuffer, "glBindRenderbuffer");
   pragma Import (StdCall, glDeleteRenderbuffers, "glDeleteRenderbuffers");
   pragma Import (StdCall, glGenRenderbuffers, "glGenRenderbuffers");
   pragma Import (StdCall, glRenderbufferStorage, "glRenderbufferStorage");
   pragma Import
     (StdCall,
      glGetRenderbufferParameteriv,
      "glGetRenderbufferParameteriv");
   pragma Import (StdCall, glIsFramebuffer, "glIsFramebuffer");
   pragma Import (StdCall, glBindFramebuffer, "glBindFramebuffer");
   pragma Import (StdCall, glDeleteFramebuffers, "glDeleteFramebuffers");
   pragma Import (StdCall, glGenFramebuffers, "glGenFramebuffers");
   pragma Import
     (StdCall,
      glCheckFramebufferStatus,
      "glCheckFramebufferStatus");
   pragma Import (StdCall, glFramebufferTexture1D, "glFramebufferTexture1D");
   pragma Import (StdCall, glFramebufferTexture2D, "glFramebufferTexture2D");
   pragma Import (StdCall, glFramebufferTexture3D, "glFramebufferTexture3D");
   pragma Import
     (StdCall,
      glFramebufferRenderbuffer,
      "glFramebufferRenderbuffer");
   pragma Import
     (StdCall,
      glGetFramebufferAttachmentParameteriv,
      "glGetFramebufferAttachmentParameteriv");
   pragma Import (StdCall, glGenerateMipmap, "glGenerateMipmap");
   pragma Import (StdCall, glBlitFramebuffer, "glBlitFramebuffer");
   pragma Import
     (StdCall,
      glRenderbufferStorageMultisample,
      "glRenderbufferStorageMultisample");
   pragma Import
     (StdCall,
      glFramebufferTextureLayer,
      "glFramebufferTextureLayer");
   pragma Import (StdCall, glMapBufferRange, "glMapBufferRange");
   pragma Import
     (StdCall,
      glFlushMappedBufferRange,
      "glFlushMappedBufferRange");
   pragma Import (StdCall, glBindVertexArray, "glBindVertexArray");
   pragma Import (StdCall, glDeleteVertexArrays, "glDeleteVertexArrays");
   pragma Import (StdCall, glGenVertexArrays, "glGenVertexArrays");
   pragma Import (StdCall, glIsVertexArray, "glIsVertexArray");
   pragma Import (StdCall, glGetUniformIndices, "glGetUniformIndices");
   pragma Import (StdCall, glGetActiveUniformsiv, "glGetActiveUniformsiv");
   pragma Import (StdCall, glGetActiveUniformName, "glGetActiveUniformName");
   pragma Import (StdCall, glGetUniformBlockIndex, "glGetUniformBlockIndex");
   pragma Import
     (StdCall,
      glGetActiveUniformBlockiv,
      "glGetActiveUniformBlockiv");
   pragma Import
     (StdCall,
      glGetActiveUniformBlockName,
      "glGetActiveUniformBlockName");
   pragma Import (StdCall, glUniformBlockBinding, "glUniformBlockBinding");
   pragma Import (StdCall, glCopyBufferSubData, "glCopyBufferSubData");
   pragma Import
     (StdCall,
      glDrawElementsBaseVertex,
      "glDrawElementsBaseVertex");
   pragma Import
     (StdCall,
      glDrawRangeElementsBaseVertex,
      "glDrawRangeElementsBaseVertex");
   pragma Import
     (StdCall,
      glDrawElementsInstancedBaseVertex,
      "glDrawElementsInstancedBaseVertex");
   pragma Import
     (StdCall,
      glMultiDrawElementsBaseVertex,
      "glMultiDrawElementsBaseVertex");
   pragma Import (StdCall, glProvokingVertex, "glProvokingVertex");
   pragma Import (StdCall, glTexImage2DMultisample, "glTexImage2DMultisample");
   pragma Import (StdCall, glTexImage3DMultisample, "glTexImage3DMultisample");
   pragma Import (StdCall, glGetMultisamplefv, "glGetMultisamplefv");
   pragma Import (StdCall, glSampleMaski, "glSampleMaski");
   pragma Import (StdCall, glBlendEquationi, "glBlendEquationi");
   pragma Import
     (StdCall,
      glBlendEquationSeparatei,
      "glBlendEquationSeparatei");
   pragma Import (StdCall, glBlendFunci, "glBlendFunci");
   pragma Import (StdCall, glBlendFuncSeparatei, "glBlendFuncSeparatei");
   pragma Import (StdCall, glMinSampleShading, "glMinSampleShading");
   pragma Import
     (StdCall,
      glBindFragDataLocationIndexed,
      "glBindFragDataLocationIndexed");
   pragma Import (StdCall, glGetFragDataIndex, "glGetFragDataIndex");
   pragma Import (StdCall, glGenSamplers, "glGenSamplers");
   pragma Import (StdCall, glDeleteSamplers, "glDeleteSamplers");
   pragma Import (StdCall, glIsSampler, "glIsSampler");
   pragma Import (StdCall, glBindSampler, "glBindSampler");
   pragma Import (StdCall, glSamplerParameteri, "glSamplerParameteri");
   pragma Import (StdCall, glSamplerParameteriv, "glSamplerParameteriv");
   pragma Import (StdCall, glSamplerParameterf, "glSamplerParameterf");
   pragma Import (StdCall, glSamplerParameterfv, "glSamplerParameterfv");
   pragma Import (StdCall, glSamplerParameterIiv, "glSamplerParameterIiv");
   pragma Import (StdCall, glSamplerParameterIuiv, "glSamplerParameterIuiv");
   pragma Import (StdCall, glGetSamplerParameteriv, "glGetSamplerParameteriv");
   pragma Import
     (StdCall,
      glGetSamplerParameterIiv,
      "glGetSamplerParameterIiv");
   pragma Import (StdCall, glGetSamplerParameterfv, "glGetSamplerParameterfv");
   pragma Import
     (StdCall,
      glGetSamplerParameterIfv,
      "glGetSamplerParameterIfv");
   pragma Import (StdCall, glQueryCounter, "glQueryCounter");
   pragma Import (StdCall, glGetQueryObjecti64v, "glGetQueryObjecti64v");
   pragma Import (StdCall, glGetQueryObjectui64v, "glGetQueryObjectui64v");
   pragma Import (StdCall, glVertexP2ui, "glVertexP2ui");
   pragma Import (StdCall, glVertexP2uiv, "glVertexP2uiv");
   pragma Import (StdCall, glVertexP3ui, "glVertexP3ui");
   pragma Import (StdCall, glVertexP3uiv, "glVertexP3uiv");
   pragma Import (StdCall, glVertexP4ui, "glVertexP4ui");
   pragma Import (StdCall, glVertexP4uiv, "glVertexP4uiv");
   pragma Import (StdCall, glTexCoordP1ui, "glTexCoordP1ui");
   pragma Import (StdCall, glTexCoordP1uiv, "glTexCoordP1uiv");
   pragma Import (StdCall, glTexCoordP2ui, "glTexCoordP2ui");
   pragma Import (StdCall, glTexCoordP2uiv, "glTexCoordP2uiv");
   pragma Import (StdCall, glTexCoordP3ui, "glTexCoordP3ui");
   pragma Import (StdCall, glTexCoordP3uiv, "glTexCoordP3uiv");
   pragma Import (StdCall, glTexCoordP4ui, "glTexCoordP4ui");
   pragma Import (StdCall, glTexCoordP4uiv, "glTexCoordP4uiv");
   pragma Import (StdCall, glMultiTexCoordP1ui, "glMultiTexCoordP1ui");
   pragma Import (StdCall, glMultiTexCoordP1uiv, "glMultiTexCoordP1uiv");
   pragma Import (StdCall, glMultiTexCoordP2ui, "glMultiTexCoordP2ui");
   pragma Import (StdCall, glMultiTexCoordP2uiv, "glMultiTexCoordP2uiv");
   pragma Import (StdCall, glMultiTexCoordP3ui, "glMultiTexCoordP3ui");
   pragma Import (StdCall, glMultiTexCoordP3uiv, "glMultiTexCoordP3uiv");
   pragma Import (StdCall, glMultiTexCoordP4ui, "glMultiTexCoordP4ui");
   pragma Import (StdCall, glMultiTexCoordP4uiv, "glMultiTexCoordP4uiv");
   pragma Import (StdCall, glNormalP3ui, "glNormalP3ui");
   pragma Import (StdCall, glNormalP3uiv, "glNormalP3uiv");
   pragma Import (StdCall, glColorP3ui, "glColorP3ui");
   pragma Import (StdCall, glColorP3uiv, "glColorP3uiv");
   pragma Import (StdCall, glColorP4ui, "glColorP4ui");
   pragma Import (StdCall, glColorP4uiv, "glColorP4uiv");
   pragma Import (StdCall, glSecondaryColorP3ui, "glSecondaryColorP3ui");
   pragma Import (StdCall, glSecondaryColorP3uiv, "glSecondaryColorP3uiv");
   pragma Import (StdCall, glVertexAttribP1ui, "glVertexAttribP1ui");
   pragma Import (StdCall, glVertexAttribP1uiv, "glVertexAttribP1uiv");
   pragma Import (StdCall, glVertexAttribP2ui, "glVertexAttribP2ui");
   pragma Import (StdCall, glVertexAttribP2uiv, "glVertexAttribP2uiv");
   pragma Import (StdCall, glVertexAttribP3ui, "glVertexAttribP3ui");
   pragma Import (StdCall, glVertexAttribP3uiv, "glVertexAttribP3uiv");
   pragma Import (StdCall, glVertexAttribP4ui, "glVertexAttribP4ui");
   pragma Import (StdCall, glVertexAttribP4uiv, "glVertexAttribP4uiv");
   pragma Import (StdCall, glDrawArraysIndirect, "glDrawArraysIndirect");
   pragma Import (StdCall, glDrawElementsIndirect, "glDrawElementsIndirect");
   pragma Import (StdCall, glUniform1d, "glUniform1d");
   pragma Import (StdCall, glUniform2d, "glUniform2d");
   pragma Import (StdCall, glUniform3d, "glUniform3d");
   pragma Import (StdCall, glUniform4d, "glUniform4d");
   pragma Import (StdCall, glUniform1dv, "glUniform1dv");
   pragma Import (StdCall, glUniform2dv, "glUniform2dv");
   pragma Import (StdCall, glUniform3dv, "glUniform3dv");
   pragma Import (StdCall, glUniform4dv, "glUniform4dv");
   pragma Import (StdCall, glUniformMatrix2dv, "glUniformMatrix2dv");
   pragma Import (StdCall, glUniformMatrix3dv, "glUniformMatrix3dv");
   pragma Import (StdCall, glUniformMatrix4dv, "glUniformMatrix4dv");
   pragma Import (StdCall, glUniformMatrix2x3dv, "glUniformMatrix2x3dv");
   pragma Import (StdCall, glUniformMatrix2x4dv, "glUniformMatrix2x4dv");
   pragma Import (StdCall, glUniformMatrix3x2dv, "glUniformMatrix3x2dv");
   pragma Import (StdCall, glUniformMatrix3x4dv, "glUniformMatrix3x4dv");
   pragma Import (StdCall, glUniformMatrix4x2dv, "glUniformMatrix4x2dv");
   pragma Import (StdCall, glUniformMatrix4x3dv, "glUniformMatrix4x3dv");
   pragma Import (StdCall, glGetUniformdv, "glGetUniformdv");
   pragma Import (StdCall, glProgramUniform1dEXT, "glProgramUniform1dEXT");
   pragma Import (StdCall, glProgramUniform2dEXT, "glProgramUniform2dEXT");
   pragma Import (StdCall, glProgramUniform3dEXT, "glProgramUniform3dEXT");
   pragma Import (StdCall, glProgramUniform4dEXT, "glProgramUniform4dEXT");
   pragma Import (StdCall, glProgramUniform1dvEXT, "glProgramUniform1dvEXT");
   pragma Import (StdCall, glProgramUniform2dvEXT, "glProgramUniform2dvEXT");
   pragma Import (StdCall, glProgramUniform3dvEXT, "glProgramUniform3dvEXT");
   pragma Import (StdCall, glProgramUniform4dvEXT, "glProgramUniform4dvEXT");
   pragma Import
     (StdCall,
      glProgramUniformMatrix2dvEXT,
      "glProgramUniformMatrix2dvEXT");
   pragma Import
     (StdCall,
      glProgramUniformMatrix3dvEXT,
      "glProgramUniformMatrix3dvEXT");
   pragma Import
     (StdCall,
      glProgramUniformMatrix4dvEXT,
      "glProgramUniformMatrix4dvEXT");
   pragma Import
     (StdCall,
      glProgramUniformMatrix2x3dvEXT,
      "glProgramUniformMatrix2x3dvEXT");
   pragma Import
     (StdCall,
      glProgramUniformMatrix2x4dvEXT,
      "glProgramUniformMatrix2x4dvEXT");
   pragma Import
     (StdCall,
      glProgramUniformMatrix3x2dvEXT,
      "glProgramUniformMatrix3x2dvEXT");
   pragma Import
     (StdCall,
      glProgramUniformMatrix3x4dvEXT,
      "glProgramUniformMatrix3x4dvEXT");
   pragma Import
     (StdCall,
      glProgramUniformMatrix4x2dvEXT,
      "glProgramUniformMatrix4x2dvEXT");
   pragma Import
     (StdCall,
      glProgramUniformMatrix4x3dvEXT,
      "glProgramUniformMatrix4x3dvEXT");
   pragma Import
     (StdCall,
      glGetSubroutineUniformLocation,
      "glGetSubroutineUniformLocation");
   pragma Import (StdCall, glGetSubroutineIndex, "glGetSubroutineIndex");
   pragma Import
     (StdCall,
      glGetActiveSubroutineUniformiv,
      "glGetActiveSubroutineUniformiv");
   pragma Import
     (StdCall,
      glGetActiveSubroutineUniformName,
      "glGetActiveSubroutineUniformName");
   pragma Import
     (StdCall,
      glGetActiveSubroutineName,
      "glGetActiveSubroutineName");
   pragma Import (StdCall, glUniformSubroutinesuiv, "glUniformSubroutinesuiv");
   pragma Import
     (StdCall,
      glGetUniformSubroutineuiv,
      "glGetUniformSubroutineuiv");
   pragma Import (StdCall, glGetProgramStageiv, "glGetProgramStageiv");
   pragma Import (StdCall, glPatchParameteri, "glPatchParameteri");
   pragma Import (StdCall, glPatchParameterfv, "glPatchParameterfv");
   pragma Import (StdCall, glBindTransformFeedback, "glBindTransformFeedback");
   pragma Import
     (StdCall,
      glDeleteTransformFeedbacks,
      "glDeleteTransformFeedbacks");
   pragma Import (StdCall, glGenTransformFeedbacks, "glGenTransformFeedbacks");
   pragma Import (StdCall, glIsTransformFeedback, "glIsTransformFeedback");
   pragma Import
     (StdCall,
      glPauseTransformFeedback,
      "glPauseTransformFeedback");
   pragma Import
     (StdCall,
      glResumeTransformFeedback,
      "glResumeTransformFeedback");
   pragma Import (StdCall, glDrawTransformFeedback, "glDrawTransformFeedback");
   pragma Import
     (StdCall,
      glDrawTransformFeedbackStream,
      "glDrawTransformFeedbackStream");
   pragma Import (StdCall, glBeginQueryIndexed, "glBeginQueryIndexed");
   pragma Import (StdCall, glEndQueryIndexed, "glEndQueryIndexed");
   pragma Import (StdCall, glGetQueryIndexediv, "glGetQueryIndexediv");

end GLext.Binding;
