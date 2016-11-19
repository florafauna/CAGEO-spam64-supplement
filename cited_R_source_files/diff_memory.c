Index: src/main/memory.c
===================================================================
--- src/main/memory.c	(revision 59004)
+++ src/main/memory.c	(revision 59009)
@@ -2,7 +2,7 @@
 /*
  *  R : A Computer Language for Statistical Data Analysis
  *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
- *  Copyright (C) 1998--2011  The R Development Core Team.
+ *  Copyright (C) 1998--2012  The R Development Core Team.
  *
  *  This program is free software; you can redistribute it and/or modify
  *  it under the terms of the GNU General Public License as published by
@@ -903,25 +903,25 @@
     R_size_t size;
     switch (TYPEOF(s)) {	/* get size in bytes */
     case CHARSXP:
-	size = LENGTH(s) + 1;
+	size = XLENGTH(s) + 1;
 	break;
     case RAWSXP:
-	size = LENGTH(s);
+	size = XLENGTH(s);
 	break;
     case LGLSXP:
     case INTSXP:
-	size = LENGTH(s) * sizeof(int);
+	size = XLENGTH(s) * sizeof(int);
 	break;
     case REALSXP:
-	size = LENGTH(s) * sizeof(double);
+	size = XLENGTH(s) * sizeof(double);
 	break;
     case CPLXSXP:
-	size = LENGTH(s) * sizeof(Rcomplex);
+	size = XLENGTH(s) * sizeof(Rcomplex);
 	break;
     case STRSXP:
     case EXPRSXP:
     case VECSXP:
-	size = LENGTH(s) * sizeof(SEXP);
+	size = XLENGTH(s) * sizeof(SEXP);
 	break;
     default:
 	register_bad_sexp_type(s, __LINE__);
@@ -939,7 +939,7 @@
 	    R_size_t size;
 #ifdef PROTECTCHECK
 	    if (TYPEOF(s) == FREESXP)
-		size = LENGTH(s);
+		size = XLENGTH(s);
 	    else
 		/* should not get here -- arrange for a warning/error? */
 		size = getVecSizeInVEC(s);
@@ -949,7 +949,14 @@
 	    UNSNAP_NODE(s);
 	    R_LargeVallocSize -= size;
 	    R_GenHeap[LARGE_NODE_CLASS].AllocCount--;
+#ifdef LONG_VECTOR_SUPPORT
+	    if (IS_LONG_VEC(s))
+		free(((char *) s) - sizeof(R_long_vec_hdr_t));
+	    else
+		free(s);
+#else
 	    free(s);
+#endif
 	}
 	s = next;
     }
@@ -1599,7 +1606,7 @@
 		      calculating size */
 		if (CHAR(s) != NULL) {
 		    R_size_t size = getVecSizeInVEC(s);
-		    LENGTH(s) = size;
+		    SETLENGTH(s, size);
 		}
 		SETOLDTYPE(s, TYPEOF(s));
 		TYPEOF(s) = FREESXP;
@@ -1961,10 +1968,14 @@
 char *R_alloc(size_t nelem, int eltsize)
 {
     R_size_t size = nelem * eltsize;
-    double dsize = (double)nelem * eltsize;
+    double dsize = (double) nelem * eltsize;
     if (dsize > 0) { /* precaution against integer overflow */
 	SEXP s;
-#if SIZEOF_SIZE_T > 4
+#ifdef LONG_VECTOR_SUPPORT
+	/* Must be 64-bit platform, so calculation is done as 64-bit.
+	   Eventually should worry about the 2^63-1 limit */
+	s = allocVector(RAWSXP, size + 1);
+#elif SIZEOF_SIZE_T > 4
 	/* In this case by allocating larger units we can get up to
 	   size(double) * (2^31 - 1) bytes, approx 16Gb */
 	if(dsize < R_LEN_T_MAX)
@@ -2193,7 +2204,7 @@
 */
 #define intCHARSXP 73
 
-SEXP allocVector(SEXPTYPE type, R_len_t length)
+SEXP allocVector(SEXPTYPE type, R_xlen_t length)
 {
     SEXP s;     /* For the generational collector it would be safer to
 		   work in terms of a VECSEXP here, but that would
@@ -2239,15 +2250,18 @@
 	    R_SmallVallocSize += alloc_size;
 	    ATTRIB(s) = R_NilValue;
 	    TYPEOF(s) = type;
-	    LENGTH(s) = length;
-	    TRUELENGTH(s) = 0;
+	    SET_SHORT_VEC_LENGTH(s, length);
+	    SET_SHORT_VEC_TRUELENGTH(s, 0);
 	    NAMED(s) = 0;
 	    return(s);
 	}
     }
 
-    if (length < 0 )
+    if (length > R_XLEN_T_MAX)
 	errorcall(R_GlobalContext->call,
+		  _("vector is too large")); /**** put length into message */
+    else if (length < 0 )
+	errorcall(R_GlobalContext->call,
 		  _("negative length vectors are not allowed"));
     /* number of vector cells to allocate */
     switch (type) {
@@ -2375,22 +2389,46 @@
 	    s->sxpinfo = UnmarkedNodeTemplate.sxpinfo;
 	    SET_NODE_CLASS(s, node_class);
 	    R_SmallVallocSize += alloc_size;
+	    SET_SHORT_VEC_LENGTH(s, length);
 	}
 	else {
 	    Rboolean success = FALSE;
-	    s = NULL; /* initialize to suppress warning */
-	    if (size < (R_SIZE_T_MAX / sizeof(VECREC)) - sizeof(SEXPREC_ALIGN)) {
-		s = malloc(sizeof(SEXPREC_ALIGN) + size * sizeof(VECREC));
-		if (s == NULL) {
+	    R_size_t hdrsize = sizeof(SEXPREC_ALIGN);
+#ifdef LONG_VECTOR_SUPPORT
+	    if (length > R_SHORT_LEN_MAX)
+		hdrsize = sizeof(SEXPREC_ALIGN) + sizeof(R_long_vec_hdr_t);
+#endif
+	    void *mem = NULL; /* initialize to suppress warning */
+	    if (size < (R_SIZE_T_MAX / sizeof(VECREC)) - hdrsize) { /*** not sure this test is quite right -- why subtract the header? LT */
+		mem = malloc(hdrsize + size * sizeof(VECREC));
+		if (mem == NULL) {
 		    /* If we are near the address space limit, we
 		       might be short of address space.  So return
 		       all unused objects to malloc and try again. */
 		    R_gc_full(alloc_size);
-		    s = malloc(sizeof(SEXPREC_ALIGN) + size * sizeof(VECREC));
+		    mem = malloc(hdrsize + size * sizeof(VECREC));
 		}
-		if (s != NULL) success = TRUE;
+		if (mem != NULL) {
+#ifdef LONG_VECTOR_SUPPORT
+		    if (length > R_SHORT_LEN_MAX) {
+			s = (SEXP) (((char *) mem) + sizeof(R_long_vec_hdr_t));
+			SET_SHORT_VEC_LENGTH(s, R_LONG_VEC_TOKEN);
+			SET_LONG_VEC_LENGTH(s, length);
+			SET_LONG_VEC_TRUELENGTH(s, 0);
+		    }
+		    else {
+			s = mem;
+			SET_SHORT_VEC_LENGTH(s, length);
+		    }
+#else
+		    s = mem;
+		    SETLENGTH(s, length);
+#endif
+		    success = TRUE;
+		}
+		else s = NULL;
 #ifdef R_MEMORY_PROFILING
-		R_ReportAllocation(sizeof(SEXPREC_ALIGN) + size * sizeof(VECREC));
+		R_ReportAllocation(hdrsize + size * sizeof(VECREC));
 #endif
 	    }
 	    if (! success) {
@@ -2422,9 +2460,9 @@
     }
     else {
 	GC_PROT(s = allocSExpNonCons(type));
+	SET_SHORT_VEC_LENGTH(s, length);
     }
-    LENGTH(s) = length;
-    TRUELENGTH(s) = 0;
+    SET_SHORT_VEC_TRUELENGTH(s, 0);
     NAMED(s) = 0;
 
     /* The following prevents disaster in the case */
@@ -2991,6 +3029,9 @@
 int (TRUELENGTH)(SEXP x) { return TRUELENGTH(CHK(x)); }
 void (SETLENGTH)(SEXP x, int v) { SETLENGTH(CHK(x), v); }
 void (SET_TRUELENGTH)(SEXP x, int v) { SET_TRUELENGTH(CHK(x), v); }
+R_xlen_t (XLENGTH)(SEXP x) { return XLENGTH(CHK(x)); }
+R_xlen_t (XTRUELENGTH)(SEXP x) { return XTRUELENGTH(CHK(x)); }
+int  (IS_LONG_VEC)(SEXP x) { return IS_LONG_VEC(CHK(x)); }
 
 const char *(R_CHAR)(SEXP x) {
     if(TYPEOF(x) != CHARSXP)
@@ -3467,3 +3508,10 @@
 }
 
 
+#ifdef LONG_VECTOR_SUPPORT
+R_len_t R_BadLongVector(SEXP x, const char *file, int line)
+{
+    error(_("long vectors not supported yet: %s:%d"), file, line);
+    return 0; /* not reached */
+}
+#endif
