: GEN_LUT
	255 B, \ cos(0) = 1, which obviously we can't have in the table because 1Q0.8 = 256
	1
	BEGIN DUP 256 < WHILE
		DUP DUP

		\ first, generate the first half of cosine, shifted and scaled
		I>X [ 256.0 FPI F/ ]X, XF/
		XFCOS X1. XF+ XF2/

		H# B0 >= IF XF2* XF2* THEN

		\ then, convert back to Q0.8
		[ 256.0 ]X, XF* X>
		B,

		1+
	REPEAT
;

GEN_LUT
