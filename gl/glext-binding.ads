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

   pragma Import (C, glBlendFuncSeparate, "glBlendFuncSeparate");
   pragma Import (C, glMultiDrawArrays, "glMultiDrawArrays");
   pragma Import (C, glMultiDrawElements, "glMultiDrawElements");
   pragma Import (C, glPointParameterf, "glPointParameterf");
   pragma Import (C, glPointParameterfv, "glPointParameterfv");
   pragma Import (C, glPointParameteri, "glPointParameteri");
   pragma Import (C, glPointParameteriv, "glPointParameteriv");
   pragma Import (C, glGenQueries, "glGenQueries");
   pragma Import (C, glDeleteQueries, "glDeleteQueries");
   pragma Import (C, glIsQuery, "glIsQuery");
   pragma Import (C, glBeginQuery, "glBeginQuery");
   pragma Import (C, glEndQuery, "glEndQuery");
   pragma Import (C, glGetQueryiv, "glGetQueryiv");
   pragma Import (C, glGetQueryObjectiv, "glGetQueryObjectiv");
   pragma Import (C, glGetQueryObjectuiv, "glGetQueryObjectuiv");
   pragma Import (C, glBindBuffer, "glBindBuffer");
   pragma Import (C, glDeleteBuffers, "glDeleteBuffers");
   pragma Import (C, glGenBuffers, "glGenBuffers");
   pragma Import (C, glIsBuffer, "glIsBuffer");
   pragma Import (C, glBufferData, "glBufferData");
   pragma Import (C, glBufferSubData, "glBufferSubData");
   pragma Import (C, glGetBufferSubData, "glGetBufferSubData");
   pragma Import (C, glMapBuffer, "glMapBuffer");
   pragma Import (C, glUnmapBuffer, "glUnmapBuffer");
   pragma Import (C, glGetBufferParameteriv, "glGetBufferParameteriv");
   pragma Import (C, glGetBufferPointerv, "glGetBufferPointerv");
   pragma Import (C, glBlendEquationSeparate, "glBlendEquationSeparate");
   pragma Import (C, glDrawBuffers, "glDrawBuffers");
   pragma Import (C, glStencilOpSeparate, "glStencilOpSeparate");
   pragma Import (C, glStencilFuncSeparate, "glStencilFuncSeparate");
   pragma Import (C, glStencilMaskSeparate, "glStencilMaskSeparate");
   pragma Import (C, glAttachShader, "glAttachShader");
   pragma Import (C, glBindAttribLocation, "glBindAttribLocation");
   pragma Import (C, glCompileShader, "glCompileShader");
   pragma Import (C, glCreateProgram, "glCreateProgram");
   pragma Import (C, glCreateShader, "glCreateShader");
   pragma Import (C, glDeleteProgram, "glDeleteProgram");
   pragma Import (C, glDeleteShader, "glDeleteShader");
   pragma Import (C, glDetachShader, "glDetachShader");
   pragma Import
     (C,
      glDisableVertexAttribArray,
      "glDisableVertexAttribArray");
   pragma Import
     (C,
      glEnableVertexAttribArray,
      "glEnableVertexAttribArray");
   pragma Import (C, glGetActiveAttrib, "glGetActiveAttrib");
   pragma Import (C, glGetActiveUniform, "glGetActiveUniform");
   pragma Import (C, glGetAttachedShaders, "glGetAttachedShaders");
   pragma Import (C, glGetAttribLocation, "glGetAttribLocation");
   pragma Import (C, glGetProgramiv, "glGetProgramiv");
   pragma Import (C, glGetProgramInfoLog, "glGetProgramInfoLog");
   pragma Import (C, glGetShaderiv, "glGetShaderiv");
   pragma Import (C, glGetShaderInfoLog, "glGetShaderInfoLog");
   pragma Import (C, glGetShaderSource, "glGetShaderSource");
   pragma Import (C, glGetUniformLocation, "glGetUniformLocation");
   pragma Import (C, glGetUniformfv, "glGetUniformfv");
   pragma Import (C, glGetUniformiv, "glGetUniformiv");
   pragma Import (C, glGetVertexAttribdv, "glGetVertexAttribdv");
   pragma Import (C, glGetVertexAttribfv, "glGetVertexAttribfv");
   pragma Import (C, glGetVertexAttribiv, "glGetVertexAttribiv");
   pragma Import
     (C,
      glGetVertexAttribPointerv,
      "glGetVertexAttribPointerv");
   pragma Import (C, glIsProgram, "glIsProgram");
   pragma Import (C, glIsShader, "glIsShader");
   pragma Import (C, glLinkProgram, "glLinkProgram");
   pragma Import (C, glShaderSource, "glShaderSource");
   pragma Import (C, glUseProgram, "glUseProgram");
   pragma Import (C, glUniform1f, "glUniform1f");
   pragma Import (C, glUniform2f, "glUniform2f");
   pragma Import (C, glUniform3f, "glUniform3f");
   pragma Import (C, glUniform4f, "glUniform4f");
   pragma Import (C, glUniform1i, "glUniform1i");
   pragma Import (C, glUniform2i, "glUniform2i");
   pragma Import (C, glUniform3i, "glUniform3i");
   pragma Import (C, glUniform4i, "glUniform4i");
   pragma Import (C, glUniform1fv, "glUniform1fv");
   pragma Import (C, glUniform2fv, "glUniform2fv");
   pragma Import (C, glUniform3fv, "glUniform3fv");
   pragma Import (C, glUniform4fv, "glUniform4fv");
   pragma Import (C, glUniform1iv, "glUniform1iv");
   pragma Import (C, glUniform2iv, "glUniform2iv");
   pragma Import (C, glUniform3iv, "glUniform3iv");
   pragma Import (C, glUniform4iv, "glUniform4iv");
   pragma Import (C, glUniformMatrix2fv, "glUniformMatrix2fv");
   pragma Import (C, glUniformMatrix3fv, "glUniformMatrix3fv");
   pragma Import (C, glUniformMatrix4fv, "glUniformMatrix4fv");
   pragma Import (C, glValidateProgram, "glValidateProgram");
   pragma Import (C, glVertexAttrib1d, "glVertexAttrib1d");
   pragma Import (C, glVertexAttrib1dv, "glVertexAttrib1dv");
   pragma Import (C, glVertexAttrib1f, "glVertexAttrib1f");
   pragma Import (C, glVertexAttrib1fv, "glVertexAttrib1fv");
   pragma Import (C, glVertexAttrib1s, "glVertexAttrib1s");
   pragma Import (C, glVertexAttrib1sv, "glVertexAttrib1sv");
   pragma Import (C, glVertexAttrib2d, "glVertexAttrib2d");
   pragma Import (C, glVertexAttrib2dv, "glVertexAttrib2dv");
   pragma Import (C, glVertexAttrib2f, "glVertexAttrib2f");
   pragma Import (C, glVertexAttrib2fv, "glVertexAttrib2fv");
   pragma Import (C, glVertexAttrib2s, "glVertexAttrib2s");
   pragma Import (C, glVertexAttrib2sv, "glVertexAttrib2sv");
   pragma Import (C, glVertexAttrib3d, "glVertexAttrib3d");
   pragma Import (C, glVertexAttrib3dv, "glVertexAttrib3dv");
   pragma Import (C, glVertexAttrib3f, "glVertexAttrib3f");
   pragma Import (C, glVertexAttrib3fv, "glVertexAttrib3fv");
   pragma Import (C, glVertexAttrib3s, "glVertexAttrib3s");
   pragma Import (C, glVertexAttrib3sv, "glVertexAttrib3sv");
   pragma Import (C, glVertexAttrib4Nbv, "glVertexAttrib4Nbv");
   pragma Import (C, glVertexAttrib4Niv, "glVertexAttrib4Niv");
   pragma Import (C, glVertexAttrib4Nsv, "glVertexAttrib4Nsv");
   pragma Import (C, glVertexAttrib4Nub, "glVertexAttrib4Nub");
   pragma Import (C, glVertexAttrib4Nubv, "glVertexAttrib4Nubv");
   pragma Import (C, glVertexAttrib4Nuiv, "glVertexAttrib4Nuiv");
   pragma Import (C, glVertexAttrib4Nusv, "glVertexAttrib4Nusv");
   pragma Import (C, glVertexAttrib4bv, "glVertexAttrib4bv");
   pragma Import (C, glVertexAttrib4d, "glVertexAttrib4d");
   pragma Import (C, glVertexAttrib4dv, "glVertexAttrib4dv");
   pragma Import (C, glVertexAttrib4f, "glVertexAttrib4f");
   pragma Import (C, glVertexAttrib4fv, "glVertexAttrib4fv");
   pragma Import (C, glVertexAttrib4iv, "glVertexAttrib4iv");
   pragma Import (C, glVertexAttrib4s, "glVertexAttrib4s");
   pragma Import (C, glVertexAttrib4sv, "glVertexAttrib4sv");
   pragma Import (C, glVertexAttrib4ubv, "glVertexAttrib4ubv");
   pragma Import (C, glVertexAttrib4uiv, "glVertexAttrib4uiv");
   pragma Import (C, glVertexAttrib4usv, "glVertexAttrib4usv");
   pragma Import (C, glVertexAttribPointer, "glVertexAttribPointer");
   pragma Import (C, glUniformMatrix2x3fv, "glUniformMatrix2x3fv");
   pragma Import (C, glUniformMatrix3x2fv, "glUniformMatrix3x2fv");
   pragma Import (C, glUniformMatrix2x4fv, "glUniformMatrix2x4fv");
   pragma Import (C, glUniformMatrix4x2fv, "glUniformMatrix4x2fv");
   pragma Import (C, glUniformMatrix3x4fv, "glUniformMatrix3x4fv");
   pragma Import (C, glUniformMatrix4x3fv, "glUniformMatrix4x3fv");
   pragma Import (C, glColorMaski, "glColorMaski");
   pragma Import (C, glGetBooleani_v, "glGetBooleani_v");
   pragma Import (C, glGetIntegeri_v, "glGetIntegeri_v");
   pragma Import (C, glEnablei, "glEnablei");
   pragma Import (C, glDisablei, "glDisablei");
   pragma Import (C, glIsEnabledi, "glIsEnabledi");
   pragma Import
     (C,
      glBeginTransformFeedback,
      "glBeginTransformFeedback");
   pragma Import (C, glEndTransformFeedback, "glEndTransformFeedback");
   pragma Import (C, glBindBufferRange, "glBindBufferRange");
   pragma Import (C, glBindBufferBase, "glBindBufferBase");
   pragma Import
     (C,
      glTransformFeedbackVaryings,
      "glTransformFeedbackVaryings");
   pragma Import
     (C,
      glGetTransformFeedbackVarying,
      "glGetTransformFeedbackVarying");
   pragma Import (C, glClampColor, "glClampColor");
   pragma Import
     (C,
      glBeginConditionalRender,
      "glBeginConditionalRender");
   pragma Import (C, glEndConditionalRender, "glEndConditionalRender");
   pragma Import (C, glVertexAttribIPointer, "glVertexAttribIPointer");
   pragma Import (C, glGetVertexAttribIiv, "glGetVertexAttribIiv");
   pragma Import (C, glGetVertexAttribIuiv, "glGetVertexAttribIuiv");
   pragma Import (C, glVertexAttribI1i, "glVertexAttribI1i");
   pragma Import (C, glVertexAttribI2i, "glVertexAttribI2i");
   pragma Import (C, glVertexAttribI3i, "glVertexAttribI3i");
   pragma Import (C, glVertexAttribI4i, "glVertexAttribI4i");
   pragma Import (C, glVertexAttribI1ui, "glVertexAttribI1ui");
   pragma Import (C, glVertexAttribI2ui, "glVertexAttribI2ui");
   pragma Import (C, glVertexAttribI3ui, "glVertexAttribI3ui");
   pragma Import (C, glVertexAttribI4ui, "glVertexAttribI4ui");
   pragma Import (C, glVertexAttribI1iv, "glVertexAttribI1iv");
   pragma Import (C, glVertexAttribI2iv, "glVertexAttribI2iv");
   pragma Import (C, glVertexAttribI3iv, "glVertexAttribI3iv");
   pragma Import (C, glVertexAttribI4iv, "glVertexAttribI4iv");
   pragma Import (C, glVertexAttribI1uiv, "glVertexAttribI1uiv");
   pragma Import (C, glVertexAttribI2uiv, "glVertexAttribI2uiv");
   pragma Import (C, glVertexAttribI3uiv, "glVertexAttribI3uiv");
   pragma Import (C, glVertexAttribI4uiv, "glVertexAttribI4uiv");
   pragma Import (C, glVertexAttribI4bv, "glVertexAttribI4bv");
   pragma Import (C, glVertexAttribI4sv, "glVertexAttribI4sv");
   pragma Import (C, glVertexAttribI4ubv, "glVertexAttribI4ubv");
   pragma Import (C, glVertexAttribI4usv, "glVertexAttribI4usv");
   pragma Import (C, glGetUniformuiv, "glGetUniformuiv");
   pragma Import (C, glBindFragDataLocation, "glBindFragDataLocation");
   pragma Import (C, glGetFragDataLocation, "glGetFragDataLocation");
   pragma Import (C, glUniform1ui, "glUniform1ui");
   pragma Import (C, glUniform2ui, "glUniform2ui");
   pragma Import (C, glUniform3ui, "glUniform3ui");
   pragma Import (C, glUniform4ui, "glUniform4ui");
   pragma Import (C, glUniform1uiv, "glUniform1uiv");
   pragma Import (C, glUniform2uiv, "glUniform2uiv");
   pragma Import (C, glUniform3uiv, "glUniform3uiv");
   pragma Import (C, glUniform4uiv, "glUniform4uiv");
   pragma Import (C, glTexParameterIiv, "glTexParameterIiv");
   pragma Import (C, glTexParameterIuiv, "glTexParameterIuiv");
   pragma Import (C, glGetTexParameterIiv, "glGetTexParameterIiv");
   pragma Import (C, glGetTexParameterIuiv, "glGetTexParameterIuiv");
   pragma Import (C, glClearBufferiv, "glClearBufferiv");
   pragma Import (C, glClearBufferuiv, "glClearBufferuiv");
   pragma Import (C, glClearBufferfv, "glClearBufferfv");
   pragma Import (C, glClearBufferfi, "glClearBufferfi");
   pragma Import (C, glGetStringi, "glGetStringi");
   pragma Import (C, glDrawArraysInstanced, "glDrawArraysInstanced");
   pragma Import (C, glDrawElementsInstanced, "glDrawElementsInstanced");
   pragma Import (C, glTexBuffer, "glTexBuffer");
   pragma Import (C, glPrimitiveRestartIndex, "glPrimitiveRestartIndex");
   pragma Import (C, glGetInteger64i_v, "glGetInteger64i_v");
   pragma Import
     (C,
      glGetBufferParameteri64v,
      "glGetBufferParameteri64v");
   pragma Import (C, glProgramParameteri, "glProgramParameteri");
   pragma Import (C, glFramebufferTexture, "glFramebufferTexture");
   pragma Import (C, glIsRenderbuffer, "glIsRenderbuffer");
   pragma Import (C, glBindRenderbuffer, "glBindRenderbuffer");
   pragma Import (C, glDeleteRenderbuffers, "glDeleteRenderbuffers");
   pragma Import (C, glGenRenderbuffers, "glGenRenderbuffers");
   pragma Import (C, glRenderbufferStorage, "glRenderbufferStorage");
   pragma Import
     (C,
      glGetRenderbufferParameteriv,
      "glGetRenderbufferParameteriv");
   pragma Import (C, glIsFramebuffer, "glIsFramebuffer");
   pragma Import (C, glBindFramebuffer, "glBindFramebuffer");
   pragma Import (C, glDeleteFramebuffers, "glDeleteFramebuffers");
   pragma Import (C, glGenFramebuffers, "glGenFramebuffers");
   pragma Import
     (C,
      glCheckFramebufferStatus,
      "glCheckFramebufferStatus");
   pragma Import (C, glFramebufferTexture1D, "glFramebufferTexture1D");
   pragma Import (C, glFramebufferTexture2D, "glFramebufferTexture2D");
   pragma Import (C, glFramebufferTexture3D, "glFramebufferTexture3D");
   pragma Import
     (C,
      glFramebufferRenderbuffer,
      "glFramebufferRenderbuffer");
   pragma Import
     (C,
      glGetFramebufferAttachmentParameteriv,
      "glGetFramebufferAttachmentParameteriv");
   pragma Import (C, glGenerateMipmap, "glGenerateMipmap");
   pragma Import (C, glBlitFramebuffer, "glBlitFramebuffer");
   pragma Import
     (C,
      glRenderbufferStorageMultisample,
      "glRenderbufferStorageMultisample");
   pragma Import
     (C,
      glFramebufferTextureLayer,
      "glFramebufferTextureLayer");
   pragma Import (C, glMapBufferRange, "glMapBufferRange");
   pragma Import
     (C,
      glFlushMappedBufferRange,
      "glFlushMappedBufferRange");
   pragma Import (C, glBindVertexArray, "glBindVertexArray");
   pragma Import (C, glDeleteVertexArrays, "glDeleteVertexArrays");
   pragma Import (C, glGenVertexArrays, "glGenVertexArrays");
   pragma Import (C, glIsVertexArray, "glIsVertexArray");
   pragma Import (C, glGetUniformIndices, "glGetUniformIndices");
   pragma Import (C, glGetActiveUniformsiv, "glGetActiveUniformsiv");
   pragma Import (C, glGetActiveUniformName, "glGetActiveUniformName");
   pragma Import (C, glGetUniformBlockIndex, "glGetUniformBlockIndex");
   pragma Import
     (C,
      glGetActiveUniformBlockiv,
      "glGetActiveUniformBlockiv");
   pragma Import
     (C,
      glGetActiveUniformBlockName,
      "glGetActiveUniformBlockName");
   pragma Import (C, glUniformBlockBinding, "glUniformBlockBinding");
   pragma Import (C, glCopyBufferSubData, "glCopyBufferSubData");
   pragma Import
     (C,
      glDrawElementsBaseVertex,
      "glDrawElementsBaseVertex");
   pragma Import
     (C,
      glDrawRangeElementsBaseVertex,
      "glDrawRangeElementsBaseVertex");
   pragma Import
     (C,
      glDrawElementsInstancedBaseVertex,
      "glDrawElementsInstancedBaseVertex");
   pragma Import
     (C,
      glMultiDrawElementsBaseVertex,
      "glMultiDrawElementsBaseVertex");
   pragma Import (C, glProvokingVertex, "glProvokingVertex");
   pragma Import (C, glTexImage2DMultisample, "glTexImage2DMultisample");
   pragma Import (C, glTexImage3DMultisample, "glTexImage3DMultisample");
   pragma Import (C, glGetMultisamplefv, "glGetMultisamplefv");
   pragma Import (C, glSampleMaski, "glSampleMaski");
   pragma Import (C, glBlendEquationi, "glBlendEquationi");
   pragma Import
     (C,
      glBlendEquationSeparatei,
      "glBlendEquationSeparatei");
   pragma Import (C, glBlendFunci, "glBlendFunci");
   pragma Import (C, glBlendFuncSeparatei, "glBlendFuncSeparatei");
   pragma Import (C, glMinSampleShading, "glMinSampleShading");
   pragma Import
     (C,
      glBindFragDataLocationIndexed,
      "glBindFragDataLocationIndexed");
   pragma Import (C, glGetFragDataIndex, "glGetFragDataIndex");
   pragma Import (C, glGenSamplers, "glGenSamplers");
   pragma Import (C, glDeleteSamplers, "glDeleteSamplers");
   pragma Import (C, glIsSampler, "glIsSampler");
   pragma Import (C, glBindSampler, "glBindSampler");
   pragma Import (C, glSamplerParameteri, "glSamplerParameteri");
   pragma Import (C, glSamplerParameteriv, "glSamplerParameteriv");
   pragma Import (C, glSamplerParameterf, "glSamplerParameterf");
   pragma Import (C, glSamplerParameterfv, "glSamplerParameterfv");
   pragma Import (C, glSamplerParameterIiv, "glSamplerParameterIiv");
   pragma Import (C, glSamplerParameterIuiv, "glSamplerParameterIuiv");
   pragma Import (C, glGetSamplerParameteriv, "glGetSamplerParameteriv");
   pragma Import
     (C,
      glGetSamplerParameterIiv,
      "glGetSamplerParameterIiv");
   pragma Import (C, glGetSamplerParameterfv, "glGetSamplerParameterfv");
   pragma Import
     (C,
      glGetSamplerParameterIfv,
      "glGetSamplerParameterIfv");
   pragma Import (C, glQueryCounter, "glQueryCounter");
   pragma Import (C, glGetQueryObjecti64v, "glGetQueryObjecti64v");
   pragma Import (C, glGetQueryObjectui64v, "glGetQueryObjectui64v");
   pragma Import (C, glVertexP2ui, "glVertexP2ui");
   pragma Import (C, glVertexP2uiv, "glVertexP2uiv");
   pragma Import (C, glVertexP3ui, "glVertexP3ui");
   pragma Import (C, glVertexP3uiv, "glVertexP3uiv");
   pragma Import (C, glVertexP4ui, "glVertexP4ui");
   pragma Import (C, glVertexP4uiv, "glVertexP4uiv");
   pragma Import (C, glTexCoordP1ui, "glTexCoordP1ui");
   pragma Import (C, glTexCoordP1uiv, "glTexCoordP1uiv");
   pragma Import (C, glTexCoordP2ui, "glTexCoordP2ui");
   pragma Import (C, glTexCoordP2uiv, "glTexCoordP2uiv");
   pragma Import (C, glTexCoordP3ui, "glTexCoordP3ui");
   pragma Import (C, glTexCoordP3uiv, "glTexCoordP3uiv");
   pragma Import (C, glTexCoordP4ui, "glTexCoordP4ui");
   pragma Import (C, glTexCoordP4uiv, "glTexCoordP4uiv");
   pragma Import (C, glMultiTexCoordP1ui, "glMultiTexCoordP1ui");
   pragma Import (C, glMultiTexCoordP1uiv, "glMultiTexCoordP1uiv");
   pragma Import (C, glMultiTexCoordP2ui, "glMultiTexCoordP2ui");
   pragma Import (C, glMultiTexCoordP2uiv, "glMultiTexCoordP2uiv");
   pragma Import (C, glMultiTexCoordP3ui, "glMultiTexCoordP3ui");
   pragma Import (C, glMultiTexCoordP3uiv, "glMultiTexCoordP3uiv");
   pragma Import (C, glMultiTexCoordP4ui, "glMultiTexCoordP4ui");
   pragma Import (C, glMultiTexCoordP4uiv, "glMultiTexCoordP4uiv");
   pragma Import (C, glNormalP3ui, "glNormalP3ui");
   pragma Import (C, glNormalP3uiv, "glNormalP3uiv");
   pragma Import (C, glColorP3ui, "glColorP3ui");
   pragma Import (C, glColorP3uiv, "glColorP3uiv");
   pragma Import (C, glColorP4ui, "glColorP4ui");
   pragma Import (C, glColorP4uiv, "glColorP4uiv");
   pragma Import (C, glSecondaryColorP3ui, "glSecondaryColorP3ui");
   pragma Import (C, glSecondaryColorP3uiv, "glSecondaryColorP3uiv");
   pragma Import (C, glVertexAttribP1ui, "glVertexAttribP1ui");
   pragma Import (C, glVertexAttribP1uiv, "glVertexAttribP1uiv");
   pragma Import (C, glVertexAttribP2ui, "glVertexAttribP2ui");
   pragma Import (C, glVertexAttribP2uiv, "glVertexAttribP2uiv");
   pragma Import (C, glVertexAttribP3ui, "glVertexAttribP3ui");
   pragma Import (C, glVertexAttribP3uiv, "glVertexAttribP3uiv");
   pragma Import (C, glVertexAttribP4ui, "glVertexAttribP4ui");
   pragma Import (C, glVertexAttribP4uiv, "glVertexAttribP4uiv");
   pragma Import (C, glDrawArraysIndirect, "glDrawArraysIndirect");
   pragma Import (C, glDrawElementsIndirect, "glDrawElementsIndirect");
   pragma Import (C, glUniform1d, "glUniform1d");
   pragma Import (C, glUniform2d, "glUniform2d");
   pragma Import (C, glUniform3d, "glUniform3d");
   pragma Import (C, glUniform4d, "glUniform4d");
   pragma Import (C, glUniform1dv, "glUniform1dv");
   pragma Import (C, glUniform2dv, "glUniform2dv");
   pragma Import (C, glUniform3dv, "glUniform3dv");
   pragma Import (C, glUniform4dv, "glUniform4dv");
   pragma Import (C, glUniformMatrix2dv, "glUniformMatrix2dv");
   pragma Import (C, glUniformMatrix3dv, "glUniformMatrix3dv");
   pragma Import (C, glUniformMatrix4dv, "glUniformMatrix4dv");
   pragma Import (C, glUniformMatrix2x3dv, "glUniformMatrix2x3dv");
   pragma Import (C, glUniformMatrix2x4dv, "glUniformMatrix2x4dv");
   pragma Import (C, glUniformMatrix3x2dv, "glUniformMatrix3x2dv");
   pragma Import (C, glUniformMatrix3x4dv, "glUniformMatrix3x4dv");
   pragma Import (C, glUniformMatrix4x2dv, "glUniformMatrix4x2dv");
   pragma Import (C, glUniformMatrix4x3dv, "glUniformMatrix4x3dv");
   pragma Import (C, glGetUniformdv, "glGetUniformdv");
   pragma Import (C, glProgramUniform1dEXT, "glProgramUniform1dEXT");
   pragma Import (C, glProgramUniform2dEXT, "glProgramUniform2dEXT");
   pragma Import (C, glProgramUniform3dEXT, "glProgramUniform3dEXT");
   pragma Import (C, glProgramUniform4dEXT, "glProgramUniform4dEXT");
   pragma Import (C, glProgramUniform1dvEXT, "glProgramUniform1dvEXT");
   pragma Import (C, glProgramUniform2dvEXT, "glProgramUniform2dvEXT");
   pragma Import (C, glProgramUniform3dvEXT, "glProgramUniform3dvEXT");
   pragma Import (C, glProgramUniform4dvEXT, "glProgramUniform4dvEXT");
   pragma Import
     (C,
      glProgramUniformMatrix2dvEXT,
      "glProgramUniformMatrix2dvEXT");
   pragma Import
     (C,
      glProgramUniformMatrix3dvEXT,
      "glProgramUniformMatrix3dvEXT");
   pragma Import
     (C,
      glProgramUniformMatrix4dvEXT,
      "glProgramUniformMatrix4dvEXT");
   pragma Import
     (C,
      glProgramUniformMatrix2x3dvEXT,
      "glProgramUniformMatrix2x3dvEXT");
   pragma Import
     (C,
      glProgramUniformMatrix2x4dvEXT,
      "glProgramUniformMatrix2x4dvEXT");
   pragma Import
     (C,
      glProgramUniformMatrix3x2dvEXT,
      "glProgramUniformMatrix3x2dvEXT");
   pragma Import
     (C,
      glProgramUniformMatrix3x4dvEXT,
      "glProgramUniformMatrix3x4dvEXT");
   pragma Import
     (C,
      glProgramUniformMatrix4x2dvEXT,
      "glProgramUniformMatrix4x2dvEXT");
   pragma Import
     (C,
      glProgramUniformMatrix4x3dvEXT,
      "glProgramUniformMatrix4x3dvEXT");
   pragma Import
     (C,
      glGetSubroutineUniformLocation,
      "glGetSubroutineUniformLocation");
   pragma Import (C, glGetSubroutineIndex, "glGetSubroutineIndex");
   pragma Import
     (C,
      glGetActiveSubroutineUniformiv,
      "glGetActiveSubroutineUniformiv");
   pragma Import
     (C,
      glGetActiveSubroutineUniformName,
      "glGetActiveSubroutineUniformName");
   pragma Import
     (C,
      glGetActiveSubroutineName,
      "glGetActiveSubroutineName");
   pragma Import (C, glUniformSubroutinesuiv, "glUniformSubroutinesuiv");
   pragma Import
     (C,
      glGetUniformSubroutineuiv,
      "glGetUniformSubroutineuiv");
   pragma Import (C, glGetProgramStageiv, "glGetProgramStageiv");
   pragma Import (C, glPatchParameteri, "glPatchParameteri");
   pragma Import (C, glPatchParameterfv, "glPatchParameterfv");
   pragma Import (C, glBindTransformFeedback, "glBindTransformFeedback");
   pragma Import
     (C,
      glDeleteTransformFeedbacks,
      "glDeleteTransformFeedbacks");
   pragma Import (C, glGenTransformFeedbacks, "glGenTransformFeedbacks");
   pragma Import (C, glIsTransformFeedback, "glIsTransformFeedback");
   pragma Import
     (C,
      glPauseTransformFeedback,
      "glPauseTransformFeedback");
   pragma Import
     (C,
      glResumeTransformFeedback,
      "glResumeTransformFeedback");
   pragma Import (C, glDrawTransformFeedback, "glDrawTransformFeedback");
   pragma Import
     (C,
      glDrawTransformFeedbackStream,
      "glDrawTransformFeedbackStream");
   pragma Import (C, glBeginQueryIndexed, "glBeginQueryIndexed");
   pragma Import (C, glEndQueryIndexed, "glEndQueryIndexed");
   pragma Import (C, glGetQueryIndexediv, "glGetQueryIndexediv");

end GLext.Binding;
