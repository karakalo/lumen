#ifdef __cplusplus
extern "C" 
{
#endif


#include "GL/glx.h"


VisualID
get_visualid (XVisualInfo*   Self)
{
   return Self->visualid;
}


#ifdef __cplusplus
}
#endif
