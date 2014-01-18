
	--begin addendum to package Lumen.GL (lumen-gl.ads)
	--this seems to work as I need.
	--I believe it is essential to be able to grab/store the ModelviewMatrix
	--in an accumulator.  18jan14 fastrgv
   procedure glGetDoublev
     (pname  : in Enum;
      params : out Double_Matrix);
   pragma Import (StdCall, glGetDoublev, "glGetDoublev");

