   -- MultiTexCoord v1.3
   -- 1D
   procedure Multi_Tex_Coord (Target : in Enum;
                            S      : in Double);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Doubles_1);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            S      : in Float);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Floats_1);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            S      : in Int);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Ints_1);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            S      : in Short);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Shorts_1);
   pragma Inline(Multi_Tex_Coord);

   -- 2D
   procedure Multi_Tex_Coord (Target : in Enum;
                            S, T   : in Double);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Doubles_2);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            S, T   : in Float);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Floats_2);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            S, T   : in Int);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Ints_2);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            S, T   : in Short);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Shorts_2);
   pragma Inline(Multi_Tex_Coord);

   -- 3D
   procedure Multi_Tex_Coord (Target  : in Enum;
                            S, T, R : in Double);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Doubles_3);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target  : in Enum;
                            S, T, R : in Float);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Floats_3);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target  : in Enum;
                            S, T, R : in Int);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Ints_3);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target  : in Enum;
                            S, T, R : in Short);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Shorts_3);

   -- 4D
   procedure Multi_Tex_Coord (Target     : in Enum;
                            S, T, R, Q : in Double);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Doubles_4);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target     : in Enum;
                            S, T, R, Q : in Float);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Floats_4);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target     : in Enum;
                            S, T, R, Q : in Int);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Ints_4);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target     : in Enum;
                            S, T, R, Q : in Short);
   pragma Inline(Multi_Tex_Coord);

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Shorts_4);
   pragma Inline(Multi_Tex_Coord);

   -- Texturing
   -- 1D
   procedure Multi_Tex_Coord (Target : in Enum;
                            S      : in Double) is
      procedure glMultiTexCoord1d (Target : in Enum;
                                   S      : in Double);
      pragma Import (StdCall, glMultiTexCoord1d, "glMultiTexCoord1d");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord1d (Target, S);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Doubles_1) is
      procedure glMultiTexCoord1dv (Target : in Enum;
                                    V      : in Doubles_1);
      pragma Import (StdCall, glMultiTexCoord1dv, "glMultiTexCoord1dv");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord1dv (Target, V);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            S      : in Float) is
      procedure glMultiTexCoord1f (Target : in Enum;
                                   S      : in Float);
      pragma Import (StdCall, glMultiTexCoord1f, "glMultiTexCoord1f");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord1f (Target, S);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Floats_1) is
      procedure glMultiTexCoord1fv (Target : in Enum;
                                    V      : in Floats_1);
      pragma Import (StdCall, glMultiTexCoord1fv, "glMultiTexCoord1fv");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord1fv (Target, V);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            S      : in Int) is
      procedure glMultiTexCoord1i (Target : in Enum;
                                   S      : in Int);
      pragma Import (StdCall, glMultiTexCoord1i, "glMultiTexCoord1i");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord1i (Target, S);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Ints_1) is
      procedure glMultiTexCoord1iv (Target : in Enum;
                                    V      : in Ints_1);
      pragma Import (StdCall, glMultiTexCoord1iv, "glMultiTexCoord1iv");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord1iv (Target, V);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            S      : in Short) is
      procedure glMultiTexCoord1s (Target : in Enum;
                                   S      : in Short);
      pragma Import (StdCall, glMultiTexCoord1s, "glMultiTexCoord1s");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord1s (Target, S);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Shorts_1) is
      procedure glMultiTexCoord1sv (Target : in Enum;
                                    V      : in Shorts_1);
      pragma Import (StdCall, glMultiTexCoord1sv, "glMultiTexCoord1sv");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord1sv (Target, V);
   end Multi_Tex_Coord;

   -- 2D
   procedure Multi_Tex_Coord (Target : in Enum;
                            S, T   : in Double) is
      procedure glMultiTexCoord2d (Target : in Enum;
                                   S, T   : in Double);
      pragma Import (StdCall, glMultiTexCoord2d, "glMultiTexCoord2d");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord2d (Target, S, T);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Doubles_2) is
      procedure glMultiTexCoord2dv (Target : in Enum;
                                    V      : in Doubles_2);
      pragma Import (StdCall, glMultiTexCoord2dv, "glMultiTexCoord2dv");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord2dv (Target, V);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            S, T   : in Float) is
      procedure glMultiTexCoord2f (Target : in Enum;
                                   S, T   : in Float);
      pragma Import (StdCall, GlMultiTexCoord2f, "glMultiTexCoord2f");
   begin  -- Multi_Tex_Coord
      GlMultiTexCoord2f (Target, S, T);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Floats_2) is
      procedure glMultiTexCoord2fv (Target : in Enum;
                                    V      : in Floats_2);
      pragma Import (StdCall, glMultiTexCoord2fv, "glMultiTexCoord2fv");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord2fv (Target, V);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            S, T   : in Int) is
      procedure glMultiTexCoord2i (Target : in Enum;
                                   S, T   : in Int);
      pragma Import (StdCall, glMultiTexCoord2i, "glMultiTexCoord2i");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord2i (Target, S, T);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Ints_2) is
      procedure glMultiTexCoord2iv (Target : in Enum;
                                    V      : in Ints_2);
      pragma Import (StdCall, glMultiTexCoord2iv, "glMultiTexCoord2iv");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord2iv (Target, V);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            S, T   : in Short) is
      procedure glMultiTexCoord2s (Target : in Enum;
                                   S, T   : in Short);
      pragma Import (StdCall, glMultiTexCoord2s, "glMultiTexCoord2s");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord2s (Target, S, T);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Shorts_2) is
      procedure glMultiTexCoord2sv (Target : in Enum;
                                    V      : in Shorts_2);
      pragma Import (StdCall, glMultiTexCoord2sv, "glMultiTexCoord2sv");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord2sv (Target, V);
   end Multi_Tex_Coord;

   -- 3D
   procedure Multi_Tex_Coord (Target  : in Enum;
                            S, T, R : in Double) is
      procedure glMultiTexCoord3d (Target  : in Enum;
                                   S, T, R : in Double);
      pragma Import (StdCall, glMultiTexCoord3d, "glMultiTexCoord3d");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord3d (Target, S, T, R);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Doubles_3) is
      procedure glMultiTexCoord3dv (Target : in Enum;
                                    V      : in Doubles_3);
      pragma Import (StdCall, glMultiTexCoord3dv, "glMultiTexCoord3dv");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord3dv (Target, V);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target  : in Enum;
                            S, T, R : in Float) is
      procedure glMultiTexCoord3f (Target  : in Enum;
                                   S, T, R : in Float);
      pragma Import (StdCall, glMultiTexCoord3f, "glMultiTexCoord3f");
   begin  -- Multi_Tex_Coord
      GlMultiTexCoord3f (Target, S, T, R);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Floats_3) is
      procedure glMultiTexCoord3fv (Target : in Enum;
                                    V      : in Floats_3);
      pragma Import (StdCall, glMultiTexCoord3fv, "glMultiTexCoord3fv");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord3fv (Target, V);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target  : in Enum;
                            S, T, R : in Int) is
      procedure glMultiTexCoord3i (Target  : in Enum;
                                   S, T, R : in Int);
      pragma Import (StdCall, glMultiTexCoord3i, "glMultiTexCoord3i");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord3i (Target, S, T, R);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Ints_3) is
      procedure glMultiTexCoord3iv (Target : in Enum;
                                    V      : in Ints_3);
      pragma Import (StdCall, glMultiTexCoord3iv, "glMultiTexCoord3iv");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord3iv (Target, V);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target  : in Enum;
                            S, T, R : in Short) is
      procedure glMultiTexCoord3s (Target  : in Enum;
                                   S, T, R : in Short);
      pragma Import (StdCall, glMultiTexCoord3s, "glMultiTexCoord3s");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord3s (Target, S, T, R);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Shorts_3) is
      procedure glMultiTexCoord3sv (Target : in Enum;
                                    V      : in Shorts_3);
      pragma Import (StdCall, glMultiTexCoord3sv, "glMultiTexCoord3sv");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord3sv (Target, V);
   end Multi_Tex_Coord;

   -- 4D
   procedure Multi_Tex_Coord (Target     : in Enum;
                            S, T, R, Q : in Double) is
      procedure glMultiTexCoord4d (Target     : in Enum;
                                   S, T, R, Q : in Double);
      pragma Import (StdCall, glMultiTexCoord4d, "glMultiTexCoord4d");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord4d (Target, S, T, R, Q);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Doubles_4) is
      procedure glMultiTexCoord4dv (Target : in Enum;
                                    V      : in Doubles_4);
      pragma Import (StdCall, glMultiTexCoord4dv, "glMultiTexCoord4dv");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord4dv (Target, V);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target     : in Enum;
                            S, T, R, Q : in Float) is
      procedure glMultiTexCoord4f (Target     : in Enum;
                                   S, T, R, Q : in Float);
      pragma Import (StdCall, glMultiTexCoord4f, "glMultiTexCoord4f");
   begin  -- Multi_Tex_Coord
      GlMultiTexCoord4f (Target, S, T, R, Q);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Floats_4) is
      procedure glMultiTexCoord4fv (Target : in Enum;
                                    V      : in Floats_4);
      pragma Import (StdCall, glMultiTexCoord4fv, "glMultiTexCoord4fv");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord4fv (Target, V);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target     : in Enum;
                            S, T, R, Q : in Int) is
      procedure glMultiTexCoord4i (Target     : in Enum;
                                   S, T, R, Q : in Int);
      pragma Import (StdCall, glMultiTexCoord4i, "glMultiTexCoord4i");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord4i (Target, S, T, R, Q);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Ints_4) is
      procedure glMultiTexCoord4iv (Target : in Enum;
                                    V      : in Ints_4);
      pragma Import (StdCall, glMultiTexCoord4iv, "glMultiTexCoord4iv");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord4iv (Target, V);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target     : in Enum;
                            S, T, R, Q : in Short) is
      procedure glMultiTexCoord4s (Target     : in Enum;
                                   S, T, R, Q : in Short);
      pragma Import (StdCall, glMultiTexCoord4s, "glMultiTexCoord4s");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord4s (Target, S, T, R, Q);
   end Multi_Tex_Coord;

   procedure Multi_Tex_Coord (Target : in Enum;
                            V      : in Shorts_4) is
      procedure glMultiTexCoord4sv (Target : in Enum;
                                    V      : in Shorts_4);
      pragma Import (StdCall, glMultiTexCoord4sv, "glMultiTexCoord4sv");
   begin  -- Multi_Tex_Coord
      glMultiTexCoord4sv (Target, V);
   end Multi_Tex_Coord;

