with
     GL,
     System;


package GLU
--
--  Provides a subset of the functions in GLU, tailored to be suitable for use with the openGL 'Embedded' profile.
--
--  Currently only 'gluScaleImage' is ported.
--
is
   use GL;


   procedure gluScaleImage (format    : in GLenum;
                            widthin   : in GLsizei;
                            heightin  : in GLsizei;
                            typein    : in GLenum;
                            datain    : in System.Address;
                            widthout  : in GLsizei;
                            heightout : in GLsizei;
                            typeout   : in GLenum;
                            dataout   : in System.Address);

   GLU_INVALID_VALUE,
   GLU_INVALID_ENUM,
   GLU_INVALID_TYPE,
   GLU_INVALID_OPERATION,
   GLU_OUT_OF_MEMORY : exception;

end GLU;
