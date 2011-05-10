with Interfaces.C;
with System;



package GLU is

   pragma Pure;

   use Interfaces;



   -- GLUnurbs
   --
   subtype GLUnurbs is system.Address;

   type GLUnurbs_array is
     array (C.size_t range <>) of aliased GLUnurbs;

   -- GLUquadric
   --
   subtype GLUquadric is system.Address;

   type GLUquadric_array is
     array (C.size_t range <>) of aliased GLUquadric;

   -- GLUtesselator
   --
   subtype GLUtesselator is system.Address;

   type GLUtesselator_array is
     array (C.size_t range <>) of aliased GLUtesselator;

   -- GLUnurbsObj
   --
   subtype GLUnurbsObj is GLUnurbs;

   type GLUnurbsObj_array is
     array (C.size_t range <>) of aliased GLUnurbsObj;

   -- GLUquadricObj
   --
   subtype GLUquadricObj is GLUquadric;

   type GLUquadricObj_array is
     array (C.size_t range <>) of aliased GLUquadricObj;

   -- GLUtesselatorObj
   --
   subtype GLUtesselatorObj is GLUtesselator;

   type GLUtesselatorObj_array is
     array (C.size_t range <>) of aliased GLUtesselatorObj;

   -- GLUtriangulatorObj
   --
   subtype GLUtriangulatorObj is GLUtesselator;

   type GLUtriangulatorObj_array is
     array (C.size_t range <>) of aliased GLUtriangulatorObj;

   -- a_GLUfuncptr
   --
   type a_GLUfuncptr is access procedure;
   pragma Convention (C, a_GLUfuncptr);

   type a_GLUfuncptr_array is
     array (C.size_t range <>) of aliased a_GLUfuncptr;

   GLU_EXT_object_space_tess       : constant := 1;
   GLU_EXT_nurbs_tessellator       : constant := 1;
   GLU_FALSE                       : constant := 0;
   GLU_TRUE                        : constant := 1;
   GLU_VERSION_1_1                 : constant := 1;
   GLU_VERSION_1_2                 : constant := 1;
   GLU_VERSION_1_3                 : constant := 1;
   GLU_VERSION                     : constant := 100800;
   GLU_EXTENSIONS                  : constant := 100801;
   GLU_INVALID_ENUM                : constant := 100900;
   GLU_INVALID_VALUE               : constant := 100901;
   GLU_OUT_OF_MEMORY               : constant := 100902;
   GLU_INCOMPATIBLE_GL_VERSION     : constant := 100903;
   GLU_INVALID_OPERATION           : constant := 100904;
   GLU_OUTLINE_POLYGON             : constant := 100240;
   GLU_OUTLINE_PATCH               : constant := 100241;
   GLU_NURBS_ERROR                 : constant := 100103;
   GLU_ERROR                       : constant := 100103;
   GLU_NURBS_BEGIN                 : constant := 100164;
   GLU_NURBS_BEGIN_EXT             : constant := 100164;
   GLU_NURBS_VERTEX                : constant := 100165;
   GLU_NURBS_VERTEX_EXT            : constant := 100165;
   GLU_NURBS_NORMAL                : constant := 100166;
   GLU_NURBS_NORMAL_EXT            : constant := 100166;
   GLU_NURBS_COLOR                 : constant := 100167;
   GLU_NURBS_COLOR_EXT             : constant := 100167;
   GLU_NURBS_TEXTURE_COORD         : constant := 100168;
   GLU_NURBS_TEX_COORD_EXT         : constant := 100168;
   GLU_NURBS_END                   : constant := 100169;
   GLU_NURBS_END_EXT               : constant := 100169;
   GLU_NURBS_BEGIN_DATA            : constant := 100170;
   GLU_NURBS_BEGIN_DATA_EXT        : constant := 100170;
   GLU_NURBS_VERTEX_DATA           : constant := 100171;
   GLU_NURBS_VERTEX_DATA_EXT       : constant := 100171;
   GLU_NURBS_NORMAL_DATA           : constant := 100172;
   GLU_NURBS_NORMAL_DATA_EXT       : constant := 100172;
   GLU_NURBS_COLOR_DATA            : constant := 100173;
   GLU_NURBS_COLOR_DATA_EXT        : constant := 100173;
   GLU_NURBS_TEXTURE_COORD_DATA    : constant := 100174;
   GLU_NURBS_TEX_COORD_DATA_EXT    : constant := 100174;
   GLU_NURBS_END_DATA              : constant := 100175;
   GLU_NURBS_END_DATA_EXT          : constant := 100175;
   GLU_NURBS_ERROR1                : constant := 100251;
   GLU_NURBS_ERROR2                : constant := 100252;
   GLU_NURBS_ERROR3                : constant := 100253;
   GLU_NURBS_ERROR4                : constant := 100254;
   GLU_NURBS_ERROR5                : constant := 100255;
   GLU_NURBS_ERROR6                : constant := 100256;
   GLU_NURBS_ERROR7                : constant := 100257;
   GLU_NURBS_ERROR8                : constant := 100258;
   GLU_NURBS_ERROR9                : constant := 100259;
   GLU_NURBS_ERROR10               : constant := 100260;
   GLU_NURBS_ERROR11               : constant := 100261;
   GLU_NURBS_ERROR12               : constant := 100262;
   GLU_NURBS_ERROR13               : constant := 100263;
   GLU_NURBS_ERROR14               : constant := 100264;
   GLU_NURBS_ERROR15               : constant := 100265;
   GLU_NURBS_ERROR16               : constant := 100266;
   GLU_NURBS_ERROR17               : constant := 100267;
   GLU_NURBS_ERROR18               : constant := 100268;
   GLU_NURBS_ERROR19               : constant := 100269;
   GLU_NURBS_ERROR20               : constant := 100270;
   GLU_NURBS_ERROR21               : constant := 100271;
   GLU_NURBS_ERROR22               : constant := 100272;
   GLU_NURBS_ERROR23               : constant := 100273;
   GLU_NURBS_ERROR24               : constant := 100274;
   GLU_NURBS_ERROR25               : constant := 100275;
   GLU_NURBS_ERROR26               : constant := 100276;
   GLU_NURBS_ERROR27               : constant := 100277;
   GLU_NURBS_ERROR28               : constant := 100278;
   GLU_NURBS_ERROR29               : constant := 100279;
   GLU_NURBS_ERROR30               : constant := 100280;
   GLU_NURBS_ERROR31               : constant := 100281;
   GLU_NURBS_ERROR32               : constant := 100282;
   GLU_NURBS_ERROR33               : constant := 100283;
   GLU_NURBS_ERROR34               : constant := 100284;
   GLU_NURBS_ERROR35               : constant := 100285;
   GLU_NURBS_ERROR36               : constant := 100286;
   GLU_NURBS_ERROR37               : constant := 100287;
   GLU_AUTO_LOAD_MATRIX            : constant := 100200;
   GLU_CULLING                     : constant := 100201;
   GLU_SAMPLING_TOLERANCE          : constant := 100203;
   GLU_DISPLAY_MODE                : constant := 100204;
   GLU_PARAMETRIC_TOLERANCE        : constant := 100202;
   GLU_SAMPLING_METHOD             : constant := 100205;
   GLU_U_STEP                      : constant := 100206;
   GLU_V_STEP                      : constant := 100207;
   GLU_NURBS_MODE                  : constant := 100160;
   GLU_NURBS_MODE_EXT              : constant := 100160;
   GLU_NURBS_TESSELLATOR           : constant := 100161;
   GLU_NURBS_TESSELLATOR_EXT       : constant := 100161;
   GLU_NURBS_RENDERER              : constant := 100162;
   GLU_NURBS_RENDERER_EXT          : constant := 100162;
   GLU_OBJECT_PARAMETRIC_ERROR     : constant := 100208;
   GLU_OBJECT_PARAMETRIC_ERROR_EXT : constant := 100208;
   GLU_OBJECT_PATH_LENGTH          : constant := 100209;
   GLU_OBJECT_PATH_LENGTH_EXT      : constant := 100209;
   GLU_PATH_LENGTH                 : constant := 100215;
   GLU_PARAMETRIC_ERROR            : constant := 100216;
   GLU_DOMAIN_DISTANCE             : constant := 100217;
   GLU_MAP1_TRIM_2                 : constant := 100210;
   GLU_MAP1_TRIM_3                 : constant := 100211;
   GLU_POINT                       : constant := 100010;
   GLU_LINE                        : constant := 100011;
   GLU_FILL                        : constant := 100012;
   GLU_SILHOUETTE                  : constant := 100013;
   GLU_SMOOTH                      : constant := 100000;
   GLU_FLAT                        : constant := 100001;
   GLU_NONE                        : constant := 100002;
   GLU_OUTSIDE                     : constant := 100020;
   GLU_INSIDE                      : constant := 100021;
   GLU_TESS_BEGIN                  : constant := 100100;
   GLU_BEGIN                       : constant := 100100;
   GLU_TESS_VERTEX                 : constant := 100101;
   GLU_VERTEX                      : constant := 100101;
   GLU_TESS_END                    : constant := 100102;
   GLU_END                         : constant := 100102;
   GLU_TESS_ERROR                  : constant := 100103;
   GLU_TESS_EDGE_FLAG              : constant := 100104;
   GLU_EDGE_FLAG                   : constant := 100104;
   GLU_TESS_COMBINE                : constant := 100105;
   GLU_TESS_BEGIN_DATA             : constant := 100106;
   GLU_TESS_VERTEX_DATA            : constant := 100107;
   GLU_TESS_END_DATA               : constant := 100108;
   GLU_TESS_ERROR_DATA             : constant := 100109;
   GLU_TESS_EDGE_FLAG_DATA         : constant := 100110;
   GLU_TESS_COMBINE_DATA           : constant := 100111;
   GLU_CW                          : constant := 100120;
   GLU_CCW                         : constant := 100121;
   GLU_INTERIOR                    : constant := 100122;
   GLU_EXTERIOR                    : constant := 100123;
   GLU_UNKNOWN                     : constant := 100124;
   GLU_TESS_WINDING_RULE           : constant := 100140;
   GLU_TESS_BOUNDARY_ONLY          : constant := 100141;
   GLU_TESS_TOLERANCE              : constant := 100142;
   GLU_TESS_ERROR1                 : constant := 100151;
   GLU_TESS_ERROR2                 : constant := 100152;
   GLU_TESS_ERROR3                 : constant := 100153;
   GLU_TESS_ERROR4                 : constant := 100154;
   GLU_TESS_ERROR5                 : constant := 100155;
   GLU_TESS_ERROR6                 : constant := 100156;
   GLU_TESS_ERROR7                 : constant := 100157;
   GLU_TESS_ERROR8                 : constant := 100158;
   GLU_TESS_MISSING_BEGIN_POLYGON  : constant := 100151;
   GLU_TESS_MISSING_BEGIN_CONTOUR  : constant := 100152;
   GLU_TESS_MISSING_END_POLYGON    : constant := 100153;
   GLU_TESS_MISSING_END_CONTOUR    : constant := 100154;
   GLU_TESS_COORD_TOO_LARGE        : constant := 100155;
   GLU_TESS_NEED_COMBINE_CALLBACK  : constant := 100156;
   GLU_TESS_WINDING_ODD            : constant := 100130;
   GLU_TESS_WINDING_NONZERO        : constant := 100131;
   GLU_TESS_WINDING_POSITIVE       : constant := 100132;
   GLU_TESS_WINDING_NEGATIVE       : constant := 100133;
   GLU_TESS_WINDING_ABS_GEQ_TWO    : constant := 100134;

end GLU;
